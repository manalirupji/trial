options(shiny.maxRequestSize=1000*1024^2)
options(shiny.sanitize.errors = FALSE) 

source("functions/zClust.R")
source("functions/modzClust.R")
source("functions/col_color.R")




function(input, output) {
  
  
  output$download_GW_Ex1 <- downloadHandler(
    filename= function() {paste('Data_input.csv')}, 
    content = function(file) {
      d <- read.csv("data/Data_input.csv", header = TRUE, sep = ",", stringsAsFactors = F)
      write.csv(d, file, row.names = FALSE) }
  )
  
  
  input_gw_data <- reactive({
    
    if(input$gw_file1 == 'GW_Example1'){
      data <- read.csv("data/Data_input.csv", header = TRUE, sep = ",", stringsAsFactors = F)
    } else if(input$gw_file1 == 'load_my_own_gw'){
      inFile <- input$gw_file2
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".csv", inFile[1])) { data = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F) }
      else if(grepl(".txt", inFile[1])) { data = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F) }
      else if(grepl(".rds", inFile[1])) { data = readRDS(as.character(inFile$datapath)) }
      
    }
    
    n = ncol(data)-2
    
    data2 <- data.frame(data[-1,], stringsAsFactors = F)
    rownames(data2) = paste(data2$gene_id, data2$Group, sep = "|")
    data2 <- data.frame(data2[, c(-1, -2)], stringsAsFactors = F)
    colnames(data2) = paste(names(data2), as.vector(unlist(data[1,c(-1, -2)])), sep = "||")
    data2 <- data.frame(as.matrix(data2), stringsAsFactors = F)
    data2 <- data.frame(apply(data2, 2, function(x) as.numeric(as.character(x))))
    rownames(data2) = paste(data$gene_id[-1], data$Group[-1], sep = "|")
    
    data3 <- data.matrix(data2)
    getVar <- apply(data3, 1, var)
    data4 <- data3[getVar != 0, ]
    
    col.groups <- as.character(as.vector(data[1,]))
    col.groups <- col.groups[c(-1, -2)] # calculate no. of column groups
    col.groups.name <- names(table(col.groups))
    number.col.groups <- length(col.groups.name)
    
    # box plot data
    data2$var <- apply(data2[, c(1:n)], 1, var)
    data2$mad <- apply(data2[, c(1:n)], 1, mad)
    data2$IQR <- apply(data2[, c(1:n)], 1, IQR)
    data2$Rank.var <- rank(data2$var,ties.method= "min")
    data2$Rank.mad <- rank(data2$mad,ties.method= "min")
    data2$Rank.IQR <- rank(data2$IQR,ties.method= "min")
    data2$sumofranks <- data2$Rank.var + data2$Rank.mad + data2$Rank.IQR
    
    
    #scatter plot data
    if(is.null(input$gw_subset)) {
      return()
    } else if(length(input$gw_subset) == 1) {
      if(input$gw_subset == 'VAR') {
        data2 <- data2[order(data2$var),]
        data2$var_percen <- ifelse(input$var_PercenChoice == 'Percentile Slider', quantile(data2$var, as.numeric(input$var_pslider)/100),quantile(data2$var, as.numeric(input$var_pInput)/100))
        percen <- ifelse(input$var_PercenChoice == 'Percentile Slider', input$var_pslider,  input$var_pInput)
      } else if(input$gw_subset == 'MAD') {
        data2 <- data2[order(data2$mad),]
        data2$mad_percen <- ifelse(input$mad_PercenChoice == 'Percentile Slider', quantile(data2$mad, input$mad_pslider/100),quantile(data2$mad, input$mad_pInput/100))
        percen <- ifelse(input$mad_PercenChoice == 'Percentile Slider', input$mad_pslider,  input$mad_pInput)
      } else if(input$gw_subset == 'IQR') {
        data2 <- data2[order(data2$IQR),]
        data2$iqr_percen <- ifelse(input$iqr_PercenChoice == 'Percentile Slider', quantile(data2$IQR, input$iqr_pslider/100),quantile(data2$IQR, input$iqr_pInput/100))
        percen <- ifelse(input$iqr_PercenChoice == 'Percentile Slider', input$iqr_pslider,  input$iqr_pInput)
      } 
      
    }
    else 
      if(length(input$gw_subset) > 1) {
        
        if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset) {
          data2 <- data2[order(data2$sumofranks),]
          data2$IMVA_percen <- ifelse(input$IMVA_PercenChoice == 'Percentile Slider', quantile(data2$sumofranks, as.integer(input$IMVA_pslider)/100),quantile(data2$sumofranks, as.integer(input$IMVA_pInput)/100))
          percen <- ifelse(input$IMVA_PercenChoice == 'Percentile Slider', input$IMVA_pslider,  input$IMVA_pInput)
          
        } else if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & !("IQR" %in% input$gw_subset) ) {
          data2$sumofranks_VM <- data2$Rank.var + data2$Rank.mad 
          data2 <- data2[order(data2$sumofranks_VM),]
          data2$var_mad_percen <- ifelse(input$var_mad_PercenChoice == 'Percentile Slider', quantile(data2$sumofranks_VM, input$var_mad_pslider/100),quantile(data2$sumofranks_VM, input$var_mad_pInput/100))
          percen <- ifelse(input$var_mad_PercenChoice == 'Percentile Slider', input$var_mad_pslider, input$var_mad_pInput)
          
        } else if("VAR" %in% input$gw_subset & "IQR" %in% input$gw_subset & !("MAD" %in% input$gw_subset)) {
          data2$sumofranks_VI <- data2$Rank.var + data2$Rank.IQR 
          data2 <- data2[order(data2$sumofranks_VI),]
          data2$var_iqr_percen <- ifelse(input$var_iqr_PercenChoice == 'Percentile Slider', quantile(data2$sumofranks_VI, input$var_iqr_pslider/100),quantile(data2$sumofranks_VI, input$var_iqr_pInput/100))
          percen <- ifelse(input$var_iqr_PercenChoice == 'Percentile Slider', input$var_iqr_pslider,  input$var_iqr_pInput)
          
        } else  if("MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset & !("VAR" %in% input$gw_subset)) {
          data2$sumofranks_MI <- data2$Rank.mad + data2$Rank.IQR 
          data2 <- data2[order(data2$sumofranks_MI),]
          data2$mad_iqr_percen <- ifelse(input$mad_iqr_PercenChoice == 'Percentile Slider', quantile(data2$sumofranks_MI, input$mad_iqr_pslider/100),quantile(data2$sumofranks_MI, input$mad_iqr_pInput/100))
          percen <- ifelse(input$mad_iqr_PercenChoice == 'Percentile Slider', input$mad_iqr_pslider,  input$mad_iqr_pInput)
          
        }  
        
        
      }
    
    if(is.null(input$gw_subset)) {
      return(NULL)
    } else if(length(input$gw_subset) == 1) {
      if(input$gw_subset == 'VAR') {
        data5 <- data2[data2$var > data2$var_percen[1], 1:n]
      } else if(input$gw_subset == 'MAD') {
        data5 <- data2[data2$mad > data2$mad_percen[1], 1:n]
      } else if(input$gw_subset == 'IQR') {
        data5 <- data2[data2$IQR > data2$iqr_percen[1], 1:n]
      } 
    }
    else 
      if(length(input$gw_subset) > 1) {
        if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset) {
          data5 <- data2[data2$sumofranks > data2$IMVA_percen[1], 1:n]
        }else if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & !("IQR" %in% input$gw_subset)) {
          data5 <- data2[data2$sumofranks_VM > data2$var_mad_percen[1], 1:n]
        } else if("VAR" %in% input$gw_subset & "IQR" %in% input$gw_subset & !("MAD" %in% input$gw_subset)) {
          data5 <- data2[data2$sumofranks_VI > data2$var_iqr_percen[1], 1:n]
        } else  if("MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset & !("VAR" %in% input$gw_subset)) {
          data5 <- data2[data2$sumofranks_MI > data2$mad_iqr_percen[1], 1:n]
        }  
      }
    
    dat = list(data=data, n= n, dendo_data = data4, colgroups = col.groups, colgroupsname= col.groups.name, numbercolgroups = number.col.groups, bp_sp_data = data2, extracted_data = data5, n_samples= ncol(data4), n_genes = nrow(data4),  percen = percen )
    
  })
  
  dend1 <- eventReactive(input$button1, {
    data4 <- input_gw_data()$dendo_data
    
    col.groups <- input_gw_data()$colgroups
    col.groups.name <- input_gw_data()$colgroupsname
    number.col.groups <- input_gw_data()$numbercolgroups
    
    cc1 = vector()
    
    ## Set color palette
    col1 <- colorRampPalette(c("red", "black","green"))(299)
    colors <- c(seq(-1,-0.2,length=100),seq(-0.2,0.2,length=100),seq(0.2,1,length=100)) # check slider
    
    cc1 <- col_color(col1 = col1, col.groups= col.groups, number.col.groups= number.col.groups, col.groups.name= col.groups.name)
    
    z <- list()
    
    if(input$GW_norm == "Z-Score") {
      z <- zClust(data4, scale =input$GW_norm2, zlim=c(input$GW_inSlider[1],input$GW_inSlider[2]))
    } else if (input$GW_norm == "Modified Z-Score") { 
      z <- modzClust(data4, scale =input$GW_norm2, zlim=c(input$GW_inSlider[1],input$GW_inSlider[2]))
    } else if(input$GW_norm == "none") {
      z[[1]] <- as.matrix(data4)
    }
    
    # scaling and normalization
    if(input$GW_dist == "pearson correlation") {
      dist.func =  function(x) as.dist((1-cor(t(x))))
      hc = hclust(dist.func(t(z[[1]])), method = input$GW_hclust)
    } else {
      hc = hclust(dist(t(z[[1]]), method = input$GW_dist), method = input$GW_hclust)
    }
    
    par(cex=input$gw_dend_size)
    dend <- as.dendrogram(hc)
    d <- data.frame(v1 = hc$order, v2=1:length(hc$order))
    m <- data.frame(v3 = 1:length(cc1), v4 = cc1)
    
    colbar <- data.frame(v1=d$v2, v4=m[match(d$v1, m$v3), 2])
    colbar <- colbar[,2]
    labels_colors(dend) <- as.character(colbar)
    plot(dend)
   
    if(number.col.groups==1) {
    } else if(number.col.groups==2) {
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("darkblue", "grey"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==3) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("darkblue", "grey", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==4) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("darkblue", "grey", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==5) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("darkblue", "grey", "orange", "yellow", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==6) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("darkblue", "grey", "orange", "yellow", "purple", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==7) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7])), col = c("darkblue", "grey", "orange", "yellow", "purple", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==8) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8])), col = c("darkblue", "grey", "orange", "yellow", "purple", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==9) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9])), col = c("darkblue", "grey", "orange", "yellow", "purple", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    } else if(number.col.groups==10) {  
      legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("darkblue", "grey", "orange", "yellow", "purple", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$gw_dend_size)
    }
    
  })
  
  output$gw_dend <- renderPlot({
     dend1()
  })
  
  
  
  output$geneSelector <- renderUI({
    selectizeInput(inputId = "Genes", "Choose Option:", as.list(getOSgenes()),options=list(maxOptions=getOSgenes())) 
  })
  
  output$dropdowngene <- renderText({ 
    paste("You have selected gene", input$Genes)
  })
  
  
  
  getOSgenes <- reactive({
    if(!is.null(input_gw_data()$data)) 
    {
      data <- input_gw_data()$data
      return(as.character(data[, 1]))
    }
    else 
      return(NULL)
  })
  
  output$n_selected <- renderUI({ 
    input$button1
   
    data3 <- input_gw_data()$extracted_data
    st1 <- paste("The number of genes selected : ")
    st2 <- paste(nrow(data3))
    
    n_sel <<- nrow(data3)
    isolate(
    HTML(paste(st1, strong(st2)), sep = ' ')
    )
  })
  
  bplot <- eventReactive(input$button1, {
    data <- input_gw_data()$bp_sp_data
    
    y <- list(
      title = " ")
    plot_ly(data, y = data$var, type = 'box', name = 'VAR') %>%
      add_trace(y = data$mad, name = 'MAD')  %>%
      add_trace(y = data$IQR, name = 'IQR') %>%
      layout(yaxis = y)
  })
  
  output$Boxplot <- renderPlotly({
    bplot()
  })
  
  splot <- eventReactive(input$button1, {
    data <- input_gw_data()$bp_sp_data
    
    goi <- ifelse(input$Genes== "", NA, input$Genes)
   
    if(is.null(input$gw_subset)) {
      return()
    }
 
    if(length(input$gw_subset) == 1) {
      if(input$gw_subset == 'VAR') {   
        p <- plot_ly(data, y=~var, type = "scatter", mode = "markers", name = "Ordered variances", color = I('cornflowerblue')) %>%
          add_trace(p, y = ~var_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name = 'Percentile cut-off'  )
        if (!is.na(goi[1])) {
          p <- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$var, text = input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
        }
        p %>% layout(showlegend = TRUE)
        p
      }
      else if(input$gw_subset == 'MAD') {
        p<- plot_ly(data, y=~mad, type = "scatter", mode = "markers", name = "Ordered MAD", color = I('darkgoldenrod1')) %>%
          add_trace(p, y = data$mad_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name = 'Percentile cut-off' )
        if (!is.na(goi[1])) {
          p<- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$mad, text = input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
        }
        p %>% layout(showlegend = TRUE)
        p
      } else if(input$gw_subset == 'IQR') {
        p <- plot_ly(data, y=~IQR, type = "scatter", mode = "markers", name = "Ordered IQR", color = I('chartreuse3')) %>%
          add_trace(p, y = ~iqr_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name ='Percentile cut-off' )
        if (!is.na(goi[1])) {
          p<- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$IQR, text = input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
        }
        p %>% layout(showlegend = TRUE)
        p
        
        
      } 
      
    } else 
      if(length(input$gw_subset) > 1) {
        
        if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset) {
          
          p <- plot_ly(data, y=~sumofranks, type = "scatter", mode = "markers", name = "Ordered sum of VAR, MAD and IQR", color = I('grey')) %>%
            add_trace(p, y = ~IMVA_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name = 'Percentile cut-off' ) 
          if (!is.na(goi[1])) {
            p <- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$sumofranks, text= input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
          }
          p %>% layout(showlegend = TRUE)
          p
        }
        
        else if("VAR" %in% input$gw_subset & "MAD" %in% input$gw_subset & !("IQR" %in% input$gw_subset) ) {
          p <- plot_ly(data, y=~sumofranks_VM, type = "scatter", mode = "markers", name = "Ordered sum of VAR and MAD",  color = I('grey')) %>%
            add_trace(p, y = ~var_mad_percen, line=list(dash=3, width= 1, color = "grey" ), mode = "lines", name = 'Percentile cut-off'  )
          if (!is.na(goi[1])) {
            p <- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$sumofranks_VM, text= input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
          }
          p %>% layout(showlegend = TRUE)
          p
        }
        else if ("VAR" %in% input$gw_subset & "IQR" %in% input$gw_subset& !("MAD" %in% input$gw_subset)) {
          
          p <- plot_ly(data, y=~sumofranks_VI, type = "scatter", mode = "markers", name = "Ordered sum of VAR and IQR", color = I('grey')) %>%
            add_trace(p, y = ~var_iqr_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name = 'Percentile cut-off'  )
          if (!is.na(goi[1])) {
            p <- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$sumofranks_VI, text= input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
          }
          p %>% layout(showlegend = TRUE)
          p
        }
        else if ("MAD" %in% input$gw_subset & "IQR" %in% input$gw_subset & !("VAR" %in% input$gw_subset)) {
          p <- plot_ly(data, y=~sumofranks_MI, type = "scatter", mode = "markers", name = "Ordered sum of MAD and IQR", color = I('grey')) %>%
            add_trace(p, y = ~mad_iqr_percen, line=list(dash=3, width= 1, color = "red"), mode = "lines", name = 'Percentile cut-off'  )
          if (!is.na(goi[1])) {
            p <- add_trace(p, x= which(grepl(input$Genes, rownames(data))), y= data[which(grepl(input$Genes, rownames(data))),]$sumofranks_MI, text= input$Genes, showlegend = TRUE, type = "scatter", mode = "markers", name = input$Genes, marker = list(color = "orange"))
          }
          p %>% layout(showlegend = TRUE)
          p
        } 
      }
  })
  
  output$GW_Scatter_LH <- renderPlotly({
     splot()
  }) 
  
  extracted_data2 <- reactive({
    if (input$gw_file_1 == 'GW_Example_1') {
       
        data3 <- input_gw_data()$extracted_data
        colheaders <- sub("^.*\\.","", colnames(data3))
        names(data3) <- gsub("\\.{2}.*", "", colnames(data3))
        data4 <- rbind(colheaders, data3)
        Groups <- sub("^.*\\|","", rownames(data4))
        Groups[1] <- " "
        gene_id <- sub("\\|.*$", "", rownames(data4)) 
        gene_id[1] <- " "
      
        data5 <- cbind(gene_id, Groups, data4)
        rownames(data5)[1] <- ""
      } else if(input$gw_file_1 == "load_my_own_gw_subset_1") {
        inFile_1 <- input$gw_file_2
        if (is.null(inFile_1))
          return(NULL)
        else if(grepl(".csv", inFile_1[1])) { data5 = read.csv(as.character(inFile_1$datapath), header = TRUE, sep = ",", stringsAsFactors = F) }
        else if(grepl(".txt", inFile_1[1])) { data5 = read.table(as.character(inFile_1$datapath), header = TRUE, sep = "\t", stringsAsFactors = F) }
        else if(grepl(".rds", inFile_1[1])) { data5 = readRDS(as.character(inFile_1$datapath)) }
      } 
      return(data.frame(data5))
  
   })
  
  output$downloadSubset <- downloadHandler(
    filename= function() {paste(input$fname_subset, Sys.time(),'.csv', sep='')}, 
    content = function(file) {
      d <- extracted_data2()
      write.csv(d, file, row.names = FALSE) }
  )
  
  
  
  
  
  
}


