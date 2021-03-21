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
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
     # Dataset1 = as.data.frame(scale(Dataset1, center = T, scale = T))
      return(Dataset1)
    }
  })
  
  Dataset2 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    }
  })
  
  
  output$up_data <- DT::renderDataTable(DT::datatable(Dataset(),options = list(pageLength =25)))

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
  
  
  t0 = reactive({
    set.seed(12345)
    if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        
        if(input$scale==TRUE){
          Dataset3 = as.data.frame(scale(Dataset3, center = T, scale = T))
          Dataset3 = round(Dataset3,3)
        }
        
        fit = kmeans(Dataset3,input$Clust)
        Segment.Membership =  paste0("segment","_",fit$cluster)
        d = data.frame(r.name = row.names(Dataset3),Segment.Membership,Dataset3)
        #d <- d %>% mutate(across(where(is.numeric), round, 3))
        return(d)
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        distm <- dist(Dataset3, method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward") 
        Segment.Membership =  cutree(fit, k=input$Clust)
        Segment.Membership =  paste0("segment","_",Segment.Membership)
        d = data.frame(r.name = row.names(Dataset3),Segment.Membership,Dataset3)
        #d <- d %>% mutate(across(where(is.numeric), round, 3))
        return(d)
      }
    })
  })
    
  
  output$seg_count <- renderTable({
                          if (is.null(input$file)) { return(NULL) }
                          else{seg_table <- as.data.frame(table(t0()$Segment.Membership))
                          colnames(seg_table) <- c("Segment", "Member Count") 
                          return(seg_table)
                          }
                          
                        })
  
    output$table <- renderDataTable({
      t0()%>% mutate(across(where(is.numeric), round, 3))
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption1 <- renderText({
      if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
    
   
    
    
     output$summary <- renderDataTable({
      
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
 
          summ <- t0()[-1]%>% group_by(Segment.Membership) %>%
            summarise_if(is.numeric, ~round(mean(.),2))

          summ_t <- as.data.frame(t(summ))%>%`colnames<-`(.[1, ]) %>% .[-1, ]
          summ_t[] <- lapply(summ_t, function(x) as.numeric(as.character(x)))
          summ_t<- summ_t %>% rownames_to_column("Variable")

          
          brks <- quantile(summ_t[-1], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {paste0("rgb(255,", ., ",", ., ")")}
          
          
          Summary<- DT::datatable(summ_t) %>% DT::formatStyle(names(summ_t), backgroundColor = styleInterval(brks, clrs))
          
          
        return(Summary)
        }
      })  
      else if (input$select == "Hierarchical") ({
            if (is.null(input$file)) {
              # User has not uploaded a file yet
              return(data.frame())
            }
            else {
           
              summ <- t0()[-1]%>% group_by(Segment.Membership) %>%
                summarise_if(is.numeric, ~round(mean(.),2))
              
              summ_t <- as.data.frame(t(summ))%>%`colnames<-`(.[1, ]) %>% .[-1, ]
              summ_t[] <- lapply(summ_t, function(x) as.numeric(as.character(x)))
              summ_t<- summ_t %>% rownames_to_column("Variable")
              
              
              brks <- quantile(summ_t[-1], probs = seq(.05, .95, .05), na.rm = TRUE)
              clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
                {paste0("rgb(255,", ., ",", ., ")")}
              
              
              Summary<- DT::datatable(summ_t) %>% formatStyle(names(summ_t), backgroundColor = styleInterval(brks, clrs))
        
              return(Summary)
            }
          })
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
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        Dataset3 <- Data_for_algo() %>% dplyr::select(!!!input$selVar)
        d <- dist(Dataset3, method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward.D2")  
        fit1 <- as.dendrogram(fit)
        fit1 %>% color_branches(k = input$Clust) %>% plot(main = "Dendrogram",horiz=FALSE)
       # fit1<- dendextend::set(fit1, "labels_cex", 0.5)
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
    

    
    
})
  
