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
  
  output$up_data <- DT::renderDataTable(
        DT::datatable(Dataset(),options = list(pageLength =25))
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
  
  
  output$seg_count <- renderTable({
    if (is.null(input$file)) { return(NULL) }
    else{seg_table <- as.data.frame(table(t0()$Segment.Membership))
    colnames(seg_table) <- c("Segment", "Member Count") 
    return(seg_table)
    }
    
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
  
output$summary <- renderDataTable({    
	d <- t0()
     	summ <- d[-1]%>% group_by(Segment.Membership) %>%
          summarise_if(is.numeric, ~round(mean(.),2))
        
        summ_t <- as.data.frame(t(summ))%>%`colnames<-`(.[1, ]) %>% .[-1, ]
        summ_t[] <- lapply(summ_t, function(x) as.numeric(as.character(x)))
        summ_t<- summ_t %>% rownames_to_column("Variable")
        
        brks <- quantile(summ_t[-1], probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
          {paste0("rgb(255,", ., ",", ., ")")}
        
        Summary<- DT::datatable(summ_t,options = list(pageLength =25)) %>% DT::formatStyle(names(summ_t), backgroundColor = styleInterval(brks, clrs))
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
  
output$table1 <- renderDataTable({
	#fit = kmeans(Dataset3,input$Clust)
	fit <- fit1()
	df2 = round(t(fit$centers), 2)

	  df2 = rbind(df2, as.numeric(table(fit$cluster))); df2
	  row.names(df2)[nrow(df2)] = "segmt_size"; df2
    
	  vec1 = vector(mode="list",length(nrow(df2)))
	  for (i in 1:nrow(df2)){ vec1[i] = (max(df2[i,]) - min(df2[i,])) |> round(2)  }
	  df3 = data.frame(df2, range=unlist(vec1)); #df3
	  df4 <- DT::datatable(df3, options = list(pageLength =25))
	  return(df4)
})

output$table2 <- renderTable({ 

	#fit = kmeans(Dataset3,input$Clust)
	fit <- fit1()
	  df2 = round(t(fit$centers), 2)
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

