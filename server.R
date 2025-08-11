####################################################
#      Marketing Segmentation                      #
####################################################

# library('devtools')
# library('shiny')
library('cluster')
library('ggbiplot')
library('mclust')
library('MASS')
#library('kableExtra')
library('ggplot2')
library('scales')
library('gridExtra')
library('data.table')
library('tibble')
library('DT')
library('dendextend')
library('dplyr')

shinyServer(function(input, output){

  # Reactive expression for the original uploaded data, read as-is.
  # This will be used to display the data in the "Data" tab.
  RawData <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = TRUE))
    }
  })

  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = TRUE))
      rownames(Dataset) = Dataset[,1]
      
      indx <- sapply(Dataset, is.factor)
      Dataset[indx] <- lapply(Dataset[indx], function(x) as.numeric(x))
      
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      Dataset1 = as.data.frame(scale(Dataset1, center = T, scale = T))
      return(Dataset1)
    }
  })
  
  Dataset2 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = TRUE))      
      rownames(Dataset) = Dataset[,1]      
      indx <- sapply(Dataset, is.factor)
      Dataset[indx] <- lapply(Dataset[indx], function(x) as.numeric(x))
      Dataset1 = Dataset[,2:ncol(Dataset)]    
      return(Dataset1)
    }
  })
  
  # CORRECTED: This now uses the RawData() reactive to show the original data.
  # Numeric columns are formatted to 3 decimal places for display.
  output$up_data <- DT::renderDataTable(
    if (is.null(RawData())) {
      return(NULL)
    } else {
      DT::datatable(RawData(), options = list(pageLength = 25)) %>%
      formatRound(columns = which(sapply(RawData(), is.numeric)), digits = 3)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "ConneCtorPDASegmentation.csv" },
    content = function(file) {
      write.csv(read.csv("data/ConneCtorPDASegmentation.csv"), file, row.names=F)
    }
  )
  
  # Partial example
  output$colList <- renderUI({
    varSelectInput("selVar",label = "Select Variables",data = Dataset(),multiple = TRUE,selectize = TRUE,selected = colnames(Dataset()))
  })
  
  
  Data_for_algo <- reactive({if(input$scale==TRUE){
    return(Dataset())
  }else{
    return(Dataset2())
  }})
  
fit1 <- reactive({ 
    set.seed(12345)    

  if (input$select == "K-Means") ({
      if (is.null(input$file)) { return(data.frame())      } # User has not uploaded a file yet
      else {
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        Dataset3 = as.data.frame(scale(Dataset3, center = FALSE, scale = apply(Dataset3, 2, sd, na.rm = TRUE)))
        Dataset3 = round(Dataset3,3)
        fit = kmeans(Dataset3,input$Clust)
	return(fit) }
	}) # if ends

    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) { return(data.frame()) }    # User has not uploaded a file yet
      else {
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        distm <- dist(Dataset3, method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward") 
        return(fit)       } # else ends
    }) # if ends

})  # reactive ends

t0 <- reactive({ 
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        Dataset3 = as.data.frame(scale(Dataset3, center = FALSE, scale = apply(Dataset3, 2, sd, na.rm = TRUE)))
        Dataset3 = round(Dataset3,3)

	fit <- fit1()
        Segment.Membership =  paste0("segment","_",fit$cluster)
        d = data.frame(r.name = row.names(Dataset3),Segment.Membership,Dataset3)
        return(d)
	}) # t0 ends
  
  
# REPLACE THE OLD output$seg_count WITH THIS
output$seg_count <- renderTable({
  if (is.null(input$file)) { return(NULL) }
  else {
    # Select only the columns to display and rename for clarity
    df <- seg_table_reactive()[, c("Segment", "Member_Count")]
    colnames(df) <- c("Segment", "Member Count")
    return(df)
  }
})

# ADD THIS REACTIVE EXPRESSION
seg_table_reactive <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else {
    # Create a frequency table of segment membership
    seg_table <- as.data.frame(table(t0()$Segment.Membership))
    colnames(seg_table) <- c("Segment", "Member_Count")
    
    # Calculate percentages and add to the table
    seg_table <- seg_table %>%
      mutate(Percentage = Member_Count / sum(Member_Count) * 100)
      
    return(seg_table)
  }
})

# ADD THIS NEW PLOT OUTPUT
output$segment_pie_chart <- renderPlot({
  if (is.null(seg_table_reactive())) { return(NULL) }

  df <- seg_table_reactive()
  
  # Create clean labels for the pie chart slices
  pie_labels <- paste0(df$Segment, "\n(", round(df$Percentage, 1), "%)")
  
  # Use ggplot2 to create a pie chart
  ggplot(df, aes(x = "", y = Percentage, fill = Segment)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) + # This turns the bar chart into a pie chart
    theme_void() + # Removes unnecessary background and gridlines
    geom_text(aes(label = pie_labels), position = position_stack(vjust = 0.5)) +
    theme(legend.position = "none") # Hides the legend as labels are on the chart
})
	
output$table <- renderDataTable({
	d <- t0()
	return(d) }) 
  
  output$caption1 <- renderText({
    if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
    else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
    else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
    else return (NULL)
  })
  
# CORRECTED: This version uses a grayscale color palette for the heatmap.
output$summary <- renderDataTable({    
	d <- t0()
     	summ <- d[-1]%>% group_by(Segment.Membership) %>%
          summarise_if(is.numeric, ~round(mean(.),3))
        
        summ_t <- as.data.frame(t(summ))%>%`colnames<-`(.[1, ]) %>% .[-1, ]
        summ_t[] <- lapply(summ_t, function(x) as.numeric(as.character(x)))
        summ_t<- summ_t %>% rownames_to_column("Variable")
        
        brks <- quantile(summ_t[-1], probs = seq(.05, .95, .05), na.rm = TRUE)
        
        # THIS IS THE LINE THAT HAS BEEN CHANGED
        clrs <- round(seq(230, 50, length.out = length(brks) + 1), 0) %>%
          {paste0("rgb(", ., ",", ., ",", ., ")")}
        
        Summary<- DT::datatable(summ_t,options = list(pageLength =25)) %>% 
          DT::formatStyle(names(summ_t), backgroundColor = styleInterval(brks, clrs))
        
        return(Summary)
})
	
  output$plotpca = renderPlot({ 
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else {
      Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
      data.pca <- prcomp(Dataset3,center = TRUE,scale. = TRUE)
      plot(data.pca, type = "l"); abline(h=1)    
    }
  })
  
  output$plot = renderPlot({  
    set.seed(12345)
    if (input$select == "K-Means") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
      fit = kmeans(Dataset3,input$Clust)
      classif1 = paste0("segment","_",fit$cluster)
      data.pca <- prcomp(Dataset3,
                         center = TRUE,
                         scale. = TRUE)
      # plot(data.pca, type = "l"); abline(h=1)    
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = TRUE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal',
                     legend.position = 'top')
      print(g)
      
    })
    
    if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
      d <- dist(Dataset3, method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward.D2")  
      fit1 <- as.dendrogram(fit)
      fit1 %>% color_branches(k = input$Clust) %>% plot(main = "Dendrogram",horiz=FALSE)
     
      fit1 %>% rect.dendrogram(k=input$Clust, horiz = FALSE,
                               border = 8, lty = 5, lwd = 1)
    })
  })

    
  output$downloadData4 <- downloadHandler(
    filename = function() { "segmentation.csv" },
    content = function(file) {
      write.csv(t0(), file, row.names=F)
    }
  )
  
# CORRECTED: Switched from round(..., 2) to round(..., 3) in two places
output$table1 <- renderDataTable({
	#fit = kmeans(Dataset3,input$Clust)
	fit <- fit1()
	df2 = round(t(fit$centers), 3)

	  df2 = rbind(df2, as.numeric(table(fit$cluster))); df2
	  row.names(df2)[nrow(df2)] = "segmt_size"; df2
    
	  vec1 = vector(mode="list",length(nrow(df2)))
	  for (i in 1:nrow(df2)){ vec1[i] = (max(df2[i,]) - min(df2[i,])) |> round(3)  }
	  df3 = data.frame(df2, range=unlist(vec1)); #df3
	  df4 <- DT::datatable(df3, options = list(pageLength =25))
	  return(df4)
})

# CORRECTED: Switched from round(..., 2) to round(..., 3)
output$table2 <- renderTable({ 

	#fit = kmeans(Dataset3,input$Clust)
	fit <- fit1()
	  df2 = round(t(fit$centers), 3)
	  n1 = input$Clust 
	  empty_df <- data.frame(segment = colnames(df2),
                 maxima_basis = character(n1),
                 minima_basis = character(n1))

	  for (i0 in 1:n1){
	    maxima_list = NULL; minima_list = NULL
	    for (i1 in 1:nrow(df2)){
	      if (df2[i1,i0] == max(df2[i1,])) { maxima_list = c(maxima_list, rownames(df2)[i1])}
	      if (df2[i1,i0] == min(df2[i1,])) { minima_list = c(minima_list, rownames(df2)[i1])}
	    } # i1 loop ends
	    empty_df$maxima_basis[i0] = paste(maxima_list, collapse=", ")
	    empty_df$minima_basis[i0] = paste(minima_list, collapse=", ")
	  }  # i0 loop ends
	  return(empty_df) 
 }) 
  
})





