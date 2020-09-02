library(shiny)
library(data.table)
library(parallel)
library(ranger)
library(openxlsx)
library(pacman)
library(bootstrap)
library(shinyjs)
## ui.R

library(shiny)

ui = shinyUI(fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  # Application title
  titlePanel("SERRF"),
  
  em("09/01/2020"),
  
  p("this is a temperal website for SERRF created by Shiny R. Contact slfan at ucdavis dot edu if more information is needed."),
  
  
  p("1. Click Browse to upload dataset. 2. Click Start and wait for normalization. 3. Download result when finish. Message will be given if success or fail."),
  
  url <- a("Example Dataset", href="https://github.com/slfan2013/Shiny-SERRF/raw/master/SERRF%20example%20dataset.xlsx"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
                  #"Number of bins:",
                  #min = 1,
                  #max = 50,
                  #value = 30),
      # checkboxInput(perform_cv, "Perform Cross-Validation", value = FALSE, width = NULL),
      fileInput("file1", "Upload Dataset",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",".xlsx")),
      actionButton("go", "Start"),
      downloadButton("downloadData", label = "Download")
      # actionButton("btn", "Click me"),
      # textInput("element", "Watch what happens to me")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot"),
      textOutput("text")
    )
  )
))

## server.R

library(shiny)
options(shiny.maxRequestSize=300*1024^2)
server = shinyServer(function(input, output) {
  
  # observeEvent(input$btn, {
  #   # Change the following line for more examples
  #   toggleState("element")
  # })
  
  shinyjs::disable("downloadData")
  
  result_text <- eventReactive(input$go,{
    shinyjs::disable("go")
    showNotification("Reading Dataset...", duration = 15000)
    # cl = makeCluster(8)
    # stopCluster(cl)
    # input = list(file1 = "SERRF example dataset")
    req(input$file1)
    read_data = function (input
                          # = "/Users/silifan/Downloads/460812_Gamboa_negHILIC_SERRF.csv"
                          )
    {
      pacman::p_load(openxlsx, data.table)
      
      if (grepl("xlsx", input)) {
        d <- openxlsx::read.xlsx(input, sheet = 1, colNames = FALSE)
      }else if (grepl("csv", input)) {
        d <- data.table::fread(input)
      }
      data = d
      start_col_index = min(which(diff(which(is.na(data[1,])))>1)+1)
      if(start_col_index==Inf){
        start_col_index = max(which(is.na(data[1,])))+1
      }
      
      start_row_index = min(which(diff(which(is.na(data[,1])))>1)+1)
      if(start_row_index==Inf){
        start_row_index = max(which(is.na(data[,1])))+1
      }
      
      if(start_col_index == -Inf){
        start_col_index = max(which(data[1,]==""))+1
      }
      if(start_row_index == -Inf){
        start_row_index = max(which(data[,1]==""))+1
      }
      
      
      p = data.table(t(data[1:start_row_index,start_col_index:ncol(data)]))
      colnames(p) = as.character(p[1,])
      p = p[-1,]
      if(is.null(dim(p))){
        p = data.table(label = p)
      }
      p$label = make.unique(p$label)
      
      f = data.table(data[start_row_index:nrow(data),1:start_col_index])
      colnames(f) = as.character(f[1,])
      f = f[-1,]
      if(is.null(dim(f))){
        f= data.table(label = f)
      }
      f$label = make.unique(f$label)
      
      e = data.matrix(data[(start_row_index+1):nrow(data),(start_col_index+1):ncol(data)])
      rownames(e) = f$label
      colnames(e) = p$label
      
      result =list(p = p, f = f, e = e)
      
      
      data_matrix = as.matrix(e)
      sum_NA = sum(is.na(data_matrix))
      if (sum_NA > 0) {
        message_from_R = paste0("<em>Please note, your data has ",
                                sum_NA, " missing values. These values will be replace by half-minimum for each compound before normalization.</em>")
      }else {
        message_from_R = ""
      }
      num_NAs = c()
      for(i in 1:nrow(data_matrix)){
        num_NAs[i] = sum(is.na(data_matrix[i,]))
      }
      too_many_na = num_NAs > (0.05 * ncol(data_matrix))
      # data_matrix = data_matrix[!too_many_na,]
      # f = f[!too_many_na,]
      for (i in 1:nrow(data_matrix)) {
        data_matrix[i, is.na(data_matrix[i, ])] = 1/2 * runif(1,min(data_matrix[i,!is.na(data_matrix[i, ])])*0.9,min(data_matrix[i,!is.na(data_matrix[i, ])])*1.1)
      }
      constant_compound = apply(data_matrix,1,sd,na.rm = TRUE) == 0
      data_matrix = data_matrix[!constant_compound,]
      f = f[!constant_compound,]
      if (!"label" %in% names(p)) {
        stop("cannot find 'label' in your dataset. Please download the example file and read the second sheet: 'Explanation'. ")
      }
      if (!"batch" %in% names(p)) {
        stop("cannot find 'batch' in your dataset. Please download the example file and read the second sheet: 'Explanation'.")
      }
      if (!"sampleType" %in% names(p)) {
        stop("cannot find 'sampleType' in your dataset. Please download the example file and read the second sheet: 'Explanation'.")
      }
      if (!"time" %in% names(p)) {
        stop("cannot find 'time' in your dataset. Please download the example file and read the second sheet: 'Explanation'.")
      }
      if (!"qc" %in% unique(p$sampleType)) {
        stop("cannot find 'qc' in your dataset. Please download the example file and read the second sheet: 'Explanation'.")
      }
      if (!"sample" %in% unique(p$sampleType)) {
        stop("cannot find 'sample' in your dataset. Please download the example file and read the second sheet: 'Explanation'.")
      }
      p$time = as.numeric(p$time)
      if (sum(is.na(p$time))>0) {
        stop(paste0("'time' has NA "))
      }
      if (any(table(p$batch[p$sampleType == "qc"]) < 5)) {
        stop("Some batches have too little QCs. At least 5 qc needed for EACH batch.")
      }
      
      
      
      good_index = which(p$sampleType %in% c("qc", "validate", "sample"))
      bad_index = which(!p$sampleType %in% c("qc", "validate", "sample"))
      
      p_good = p[good_index, ]
      data_matrix_good = data_matrix[,good_index]
      
      
      p_bad = p[bad_index, ]
      data_matrix_bad = data_matrix[,bad_index]
      
      sample_rank = rank(p_good$time, ties.method = "first")
      sample_order = order(p_good$time, decreasing = FALSE)
      
      sample_time = p_good$time
      data_matrix = data_matrix_good[, sample_order]
      # p = p[sample_order, ]
      # p$time = order(p$time)
      
      print(length(sample_rank))
      
      # p$sampleType[!p$sampleType %in% c("qc", "validate")] = "sample"
      return(list(e = data_matrix_good, f = f, p = p_good, data_matrix = data_matrix,
                  sample_size_summary = as.data.frame.matrix(table(p$batch,
                                                                   p$sampleType)), sample_order = sample_order, sample_time = sample_time,
                  bad_p = p_bad,bad_data_matrix = data_matrix_bad,
                  sample_rank = sample_rank, message_from_R = message_from_R, good_index = good_index, bad_index = bad_index))
    }
    
    
    remove_outlier = function(v){
      out = boxplot.stats(v)$out
      return(list(value = v[!v%in%out],index = which(v%in%out)))
    }
    
    loess_wrapper_extrapolate <- function (x, y, span.vals = seq(0.25, 1, by = 0.05), folds = 5){
      # Do model selection using mean absolute error, which is more robust than squared error.
      mean.abs.error <- numeric(length(span.vals))
      
      # Quantify error for each span, using CV
      loess.model <- function(x, y, span){
        loess(y ~ x, span = span, control=loess.control(surface="interpolate",statistics='exact'),family = "gaussian")
      }
      
      loess.predict <- function(fit, newdata) {
        predict(fit, newdata = newdata)
      }
      
      span.index <- 0
      
      for (each.span in span.vals) {
        span.index <- span.index + 1
        mean.abs.error[span.index] = tryCatch({
          y.hat.cv <- bootstrap::crossval(x, y, theta.fit = loess.model, theta.predict = loess.predict, span = each.span, ngroup = folds)$cv.fit
          non.empty.indices <- !is.na(y.hat.cv)
          diff = (y[non.empty.indices] / y.hat.cv[non.empty.indices]) * mean(y[non.empty.indices])
          sd(diff)/mean(diff)
        },error = function(er){
          NA
        })
      }
      best.span <- span.vals[which.min(mean.abs.error)]
      if(length(best.span)==0){
        best.span = 0.75
      }
      
      best.model <- loess(y ~ x, span = best.span, control=loess.control(surface="interpolate",statistics='exact'),family = "gaussian")
      
      return(list(best.model, min(mean.abs.error, na.rm = TRUE),best.span))
    }
    shiftData = function(ori,norm){
      ori.min = apply(ori,1,min,na.rm=T)
      norm.min = apply(norm,1,min,na.rm=T)
      return(norm - c(norm.min - ori.min))
    }
    RSD = function(data){
      return(apply(data,1,function(x){
        x = remove_outlier(x)[[1]]
        return(sd(x,na.rm=T)/mean(x,na.rm=T))
      }))
    }
    
    
    cat("<--------- Waiting User to Select Dataset File --------->\n")
    # df <- read.csv(input$file1$datapath, header = FALSE, stringsAsFactors = FALSE)
    # input = list(file1 = list(datapath = "SERRF example dataset - with validate.xlsx"))
    file_location = input$file1$datapath
    dta = read_data(file_location)
    # cat("<--------- Dataset is read --------->\n")
    e = dta$e
    f = dta$f
    p = dta$p
    if('validate'%in%p$sampleType){
      with_validate = TRUE
    }else{
      with_validate = FALSE
    }
    
    if(e[1,1] == 167879){# example
      
      is_example = TRUE
      
    }else{
      
      is_example = FALSE
    
    }
    normalized_dataset = list()
    qc_RSDs = list()
    methods = "SERRF"
    print(is_example)
    
    showNotification(paste0("Number of QC: ",sum(p$sampleType=='qc')), duration = 15000)
    showNotification(paste0("Number of Samples: ",sum(p$sampleType=='sample')), duration = 15000)
    if(with_validate){
      val_RSDs = list()
      
      showNotification(paste0("Number of Valiate Samples: ",sum(p$sampleType=='validate')), duration = 15000)
      # cat(paste0("Number of Valiate Samples: ",sum(p$sampleType=='validate')," \n"))
    }
    showNotification(paste0("Number of batches: ",length(unique(p$batch))," \n"), duration = 15000)
    
    if(!is_example){
    sample_rank = dta$sample_rank
    
    
    
    
    
    qc_RSDs[["none"]] = RSD(e[,p$sampleType == 'qc'])
    
    
    calculation_times = list()
    
    
    # cat(paste0("Number of batches: ",length(unique(p$batch))," \n"))
    start = Sys.time()
    e_norm = matrix(,nrow=nrow(e),ncol=ncol(e))
    QC.index = p[["sampleType"]]
    batch = p[["batch"]]
    time = p[["time"]]
    batch = factor(batch)
    num = 10
    start = Sys.time();
    cl = 1
    serrfR = function(train = e[,p$sampleType == 'qc'],
                      target = e[,p$sampleType == 'sample'],
                      num = 10,
                      batch. = factor(c(batch[p$sampleType=='qc'],batch[p$sampleType=='sample'])),
                      time. = c(time[p$sampleType=='qc'],time[p$sampleType=='sample']),
                      sampleType. = c(p$sampleType[p$sampleType=='qc'],p$sampleType[p$sampleType=='sample']),cl){
      
      
      all = cbind(train, target)
      normalized = rep(0, ncol(all))
      for(j in 1:nrow(all)){
        for(b in 1:length(unique(batch.))){
          current_batch = levels(batch.)[b]
          all[j,batch.%in%current_batch][all[j,batch.%in%current_batch] == 0] = rnorm(length(all[j,batch.%in%current_batch][all[j,batch.%in%current_batch] == 0]))
          all[j,batch.%in%current_batch][is.na(all[j,batch.%in%current_batch])] = rnorm(length(all[j,batch.%in%current_batch][is.na(all[j,batch.%in%current_batch])]),mean = all[j,batch.%in%current_batch][!is.na(all[j,batch.%in%current_batch])])
        }
      }
      
      corrs_train = list()
      corrs_target = list()
      for(b in 1:length(unique(batch.))){
        
        current_batch = levels(batch.)[b]
        
        train_scale = t(apply(train[,batch.[sampleType.=='qc']%in%current_batch],1,scale))
        if(is.null(target[,batch.[!sampleType.=='qc']%in%current_batch])){
          target_scale = t(apply(target[,batch.[!sampleType.=='qc']%in%current_batch],1,scale))
        }else{
          target_scale = scale(target[,batch.[!sampleType.=='qc']%in%current_batch])
        }
        
        corrs_train[[current_batch]] = cor(t(train_scale), method = "spearman")
        corrs_target[[current_batch]] = cor(t(target_scale), method = "spearman")
      }
      
      
      
      
      # pred = parSapply(cl, X = 1:nrow(all), function(j,all,batch.,ranger, sampleType., time., num,corrs_train,corrs_target){
      pred = matrix(nrow = nrow(all), ncol = length(sampleType.))
      
      withProgress(message = 'Normalization in Progress.', value = 0, {
        for(j in 1:nrow(all)){
          
          
          
          print(j)
          normalized  = rep(0, ncol(all))
          qc_train_value = list()
          qc_predict_value = list()
          sample_value = list()
          sample_predict_value = list()
          
          for(b in 1:length(levels(batch.))){
            current_batch = levels(batch.)[b]
            e_current_batch = all[,batch.%in%current_batch]
            corr_train = corrs_train[[current_batch]]
            corr_target = corrs_target[[current_batch]]
            
            
            corr_train_order = order(abs(corr_train[,j]),decreasing = TRUE)
            corr_target_order = order(abs(corr_target[,j]),decreasing = TRUE)
            
            sel_var = c()
            l = num
            while(length(sel_var)<(num)){
              sel_var = intersect(corr_train_order[1:l], corr_target_order[1:l])
              sel_var = sel_var[!sel_var == j]
              l = l+1
            }
            train.index_current_batch = sampleType.[batch.%in%current_batch]
            train_data_y = scale(e_current_batch[j, train.index_current_batch=='qc'],scale=F)
            train_data_x = apply(e_current_batch[sel_var, train.index_current_batch=='qc'],1,scale)
            
            if(is.null(dim(e_current_batch[sel_var, !train.index_current_batch=='qc']))){
              test_data_x = t(scale(e_current_batch[sel_var, !train.index_current_batch=='qc']))
            }else{
              test_data_x = apply(e_current_batch[sel_var, !train.index_current_batch=='qc'],1,scale)
            }
            
            train_NA_index  = apply(train_data_x,2,function(x){
              sum(is.na(x))>0
            })
            
            train_data_x = train_data_x[,!train_NA_index]
            test_data_x = test_data_x[,!train_NA_index]
            
            if(!class(test_data_x)=="matrix"){
              test_data_x = t(test_data_x)
            }
            
            good_column = apply(train_data_x,2,function(x){sum(is.na(x))==0}) & apply(test_data_x,2,function(x){sum(is.na(x))==0})
            train_data_x = train_data_x[,good_column]
            test_data_x = test_data_x[,good_column]
            if(!class(test_data_x)=="matrix"){
              test_data_x = t(test_data_x)
            }
            train_data = data.frame(y = train_data_y,train_data_x )
            colnames(train_data) = c("y", paste0("V",1:(ncol(train_data)-1)))
            model = ranger(y~., data = train_data)
            
            test_data = data.frame(test_data_x)
            colnames(test_data) = colnames(train_data)[-1]
            
            norm = e_current_batch[j,]
            
            norm[train.index_current_batch=='qc'] = e_current_batch[j, train.index_current_batch=='qc']/((predict(model, data = train_data)$prediction+mean(e_current_batch[j,train.index_current_batch=='qc'],na.rm=TRUE))/mean(all[j,sampleType.=='qc'],na.rm=TRUE))
            # norm[!train.index_current_batch=='qc'] =(e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model, data = test_data)$prediction + mean(e_current_batch[j,!train.index_current_batch=='qc'],na.rm=TRUE))/mean(e_current_batch[j,!train.index_current_batch=='qc'],na.rm=TRUE))
            
            norm[!train.index_current_batch=='qc'] =(e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))/(median(all[j,!sampleType.=='qc'],na.rm = TRUE)))
            norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]=e_current_batch[j,!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]
            
            
            # plot(p$time[batch.%in%b][!train.index_current_batch=='qc'], (e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, train.index_current_batch=='qc'],na.rm=TRUE))/(median(e_current_batch[j,!train.index_current_batch=='qc'],na.rm = TRUE))))
            
            
            norm[train.index_current_batch=='qc'] = norm[train.index_current_batch=='qc']/(median(norm[train.index_current_batch=='qc'],na.rm=TRUE)/median(all[j,sampleType.=='qc'],na.rm=TRUE))
            norm[!train.index_current_batch=='qc'] = norm[!train.index_current_batch=='qc']/(median(norm[!train.index_current_batch=='qc'],na.rm=TRUE)/median(all[j,!sampleType.=='qc'],na.rm=TRUE))
            norm[!is.finite(norm)] = rnorm(length(norm[!is.finite(norm)]),sd = sd(norm[is.finite(norm)],na.rm=TRUE)*0.01)
            
            out = boxplot.stats(norm, coef = 3)$out
            norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']%in%out] = ((e_current_batch[j,!train.index_current_batch=='qc'])-((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))-(median(all[j,!sampleType.=='qc'],na.rm = TRUE))))[norm[!train.index_current_batch=='qc']%in%out];
            norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]=e_current_batch[j,!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]
            
            normalized[batch.%in%current_batch] = norm
            # points(current_time, norm, pch = (as.numeric(factor(train.index_current_batch))-1)*19, col = "blue", cex = 0.7)
            
            qc_train_value[[b]] = train_data_y + mean(e_current_batch[j, train.index_current_batch=='qc'])
            qc_predict_value[[b]] = predict(model,data = train_data)$predictions + mean(e_current_batch[j, train.index_current_batch=='qc'])
            sample_value[[b]] = e_current_batch[j,!train.index_current_batch=='qc']
            sample_predict_value[[b]] = predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'])
            
          }
          
          # return(normalized)
          # },all,batch.,ranger, sampleType., time., num,corrs_train,corrs_target)
          pred[j,] = normalized
          incProgress(1/nrow(all), detail = paste("Working on compound", j,"/", nrow(all)))
        }
      })
      
      
      normed = pred
      
      normed_target = normed[,!sampleType.=='qc']
      
      for(i in 1:nrow(normed_target)){
        normed_target[i,is.na(normed_target[i,])] = rnorm(sum(is.na(normed_target[i,])), mean = min(normed_target[i,!is.na(normed_target[i,])], na.rm = TRUE), sd = sd(normed_target[i,!is.na(normed_target[i,])])*0.1)
      }
      
      for(i in 1:nrow(normed_target)){
        normed_target[i,normed_target[i,]<0] = runif(1) * min(normed_target[i,normed_target[i,]>0], na.rm = TRUE)
      }
      
      
      normed_train = normed[,sampleType.=='qc']
      
      
      for(i in 1:nrow(normed_train)){
        normed_train[i,is.na(normed_train[i,])] = rnorm(sum(is.na(normed_train[i,])), mean = min(normed_train[i,!is.na(normed_train[i,])], na.rm = TRUE), sd = sd(normed_train[i,!is.na(normed_train[i,])])*0.1)
      }
      
      for(i in 1:nrow(normed_train)){
        normed_train[i,normed_train[i,]<0] = runif(1) * min(normed_train[i,normed_train[i,]>0], na.rm = TRUE)
      }
      return(list(normed_train=normed_train,normed_target=normed_target))
    }
    serrf_normalized = e
    # train = e[,p$sampleType == 'qc']
    # target = e[,p$sampleType == 'sample']
    # batch. = factor(c(batch[p$sampleType=='qc'],batch[p$sampleType=='sample']))
    # time. = c(time[p$sampleType=='qc'],time[p$sampleType=='sample'])
    # sampleType. = c(p$sampleType[p$sampleType=='qc'],p$sampleType[p$sampleType=='sample'])
    
    
    
    
    serrf_normalized_modeled = serrfR(train = e[,p$sampleType == 'qc'], target = e[,p$sampleType == 'sample'], num = num,batch. = factor(c(batch[p$sampleType=='qc'],batch[p$sampleType=='sample'])),time. = c(time[p$sampleType=='qc'],time[p$sampleType=='sample']),sampleType. = c(p$sampleType[p$sampleType=='qc'],p$sampleType[p$sampleType=='sample']),cl)
    
    
    
    
    cv = 3
    showNotification(paste0("Performing ",cv, "-fold Cross-Validation"), duration = 15000)
    
    serrf_normalized[,p$sampleType == 'qc'] = serrf_normalized_modeled$normed_train
    serrf_normalized[,p$sampleType == 'sample'] = serrf_normalized_modeled$normed_target
    qc_only_data = e[,p$sampleType=='qc']
    
    
    
    RSDs = list()
    if(any(table(p$batch[p$sampleType=='qc'])<7)){
      ratio = 0.7
    }else{
      ratio = 0.8
    }
    withProgress(message = paste0(cv,'-fold Cross-Validation in Progress.'), value = 0, {
      for(k in 1:cv){
        incProgress(1/cv, detail = paste("Working on the", k,"th cross-validation."))
        train_index = sample(1L:sum(p$sampleType=='qc'),round(sum(p$sampleType=='qc')*ratio))
        test_index = c(1L:sum(p$sampleType=='qc'))[!(c(1L:sum(p$sampleType=='qc'))%in%train_index)]
        while(length(unique(batch[p$sampleType=='qc'][test_index]))<length(unique(batch))){
          train_index = sample(1L:sum(p$sampleType=='qc'),round(sum(p$sampleType=='qc')*ratio))
          test_index = c(1L:sum(p$sampleType=='qc'))[!(c(1L:sum(p$sampleType=='qc'))%in%train_index)]
        }
        serrf_normalized_on_cross_validate = serrfR(train = qc_only_data[,train_index], target = qc_only_data[,test_index], num = num,batch. = factor(c(batch[p$sampleType=='qc'][train_index],batch[p$sampleType=='qc'][test_index])),time. = c(time[p$sampleType=='qc'][train_index],time[p$sampleType=='qc'][test_index]),sampleType. = rep(c("qc","sample"),c(length(train_index),length(test_index))),cl)
        
        RSDs[[k]] = RSD(serrf_normalized_on_cross_validate$normed_target)
      }
    })
    
    qc_RSD = apply(do.call("cbind",RSDs),1,mean)
    
    
    
    if(with_validate){
      showNotification("Working on the validate samples ...", duration = 15000)
      serrf_normalized_validate = serrfR(train = e[,p$sampleType == 'qc'], target = e[,p$sampleType == 'validate'], num = num,batch. = factor(c(batch[p$sampleType=='qc'],batch[p$sampleType=='validate'])),time. = c(time[p$sampleType=='qc'],time[p$sampleType=='validate']),sampleType. = c(p$sampleType[p$sampleType=='qc'],p$sampleType[p$sampleType=='validate']),cl)
      e_norm = e
      e_norm[,p$sampleType=='qc'] = serrf_normalized[,p$sampleType == 'qc']
      e_norm[,p$sampleType=='sample'] = serrf_normalized[,p$sampleType == 'sample']
      e_norm[,p$sampleType=='validate'] = serrf_normalized_validate$normed_target
      
    }else{
      
      e_norm[,p$sampleType=='qc'] = serrf_normalized[,p$sampleType == 'qc']
      e_norm[,p$sampleType=='sample'] = serrf_normalized[,p$sampleType == 'sample']
    }
    
    showNotification("Aggregating Normalized Compounds...", duration = 15000)
    rownames(e_norm) = rownames(e)
    colnames(e_norm) = colnames(e)
    qc_RSDs[['SERRF']] = qc_RSD
    calculation_times[['SERRF']] = Sys.time() - start
    cat(paste0("Average QC RSD:",signif(median(qc_RSDs[['SERRF']],na.rm = TRUE),4)*100,"%.\n"))
    cat(paste0("Number of compounds less than 20% QC RSD:",sum(qc_RSDs[['SERRF']]<0.2,na.rm = TRUE),".\n"))
    if(with_validate){
      val_RSDs = RSD(e[,p$sampleType == 'validate'])
      
      val_RSDs[['SERRF']] = RSD(e_norm[,p$sampleType=='validate'])
      cat(paste0("Average Validate Sample RSD:",signif(median(val_RSDs[['SERRF']],na.rm = TRUE),4)*100,"%.\n"))
      cat(paste0("Number of compounds less than 20% Validate Sample RSD:",sum(val_RSDs[['SERRF']]<0.2,na.rm = TRUE),".\n"))
    }
    
    
    e_norm = e_norm[,sample_rank]
    
    cat(length(normalized_dataset))
    cat(class(normalized_dataset))
    
    normalized_dataset[["SERRF"]] = e_norm
    
    
    
    cat(length(2))
    
    showNotification("Preparing Result...", duration = 15000)
    # stopCluster(cl)
    normalized_dataset[['none']] = e
    
    
    sample_rank = dta$sample_rank
    p=p[sample_rank,]
    
    p_temp = rbind(p, dta$bad_p)
    p_temp[dta$good_index] = p
    p_temp[dta$bad_index] = dta$bad_p
    p = p_temp
    # e = cbind(e, e_other)
    
    if(ncol(normalized_dataset[['none']])==ncol(e)){
      # normalized_dataset[['none']] =  cbind(normalized_dataset[['none']], e_other)
      e_temp = cbind(normalized_dataset[['none']], dta$bad_data_matrix)
      e_temp[,dta$good_index] = normalized_dataset[['none']]
      e_temp[,dta$bad_index] = dta$bad_data_matrix
      normalized_dataset[['none']] = e_temp
      
    }
    
    for(i in 1:length(methods)){
      # normalized_dataset[[methods[i]]] = cbind(normalized_dataset[[methods[i]]], e_other)
      e_temp = cbind(normalized_dataset[[methods[i]]], dta$bad_data_matrix)
      e_temp[,dta$good_index] = normalized_dataset[[methods[i]]]
      e_temp[,dta$bad_index] = dta$bad_data_matrix
      colnames(e_temp) = p$label
      normalized_dataset[[methods[i]]] = e_temp
    }
    
    # stop("Unexpected error occurred.")
    }else{
      
      if(nrow(p) == 1299){
        
        
        example_normed = data.matrix(fread("normalized by - SERRF - with validate.csv")[,-1])
        
        
        
      }else{
        
        example_normed = data.matrix(fread("normalized by - SERRF - with validate.csv")[,-1])
        
      }
      
      normalized_dataset[['SERRF']] = example_normed
      
      qc_RSDs = fread("RSDs - with validate.csv")[,-c(1,4,5)]
      if(with_validate){
        val_RSDs = fread("RSDs - with validate.csv")[,c(4,5)]
      }
      
      
      
      withProgress(message = 'Normalization in Progress.', value = 0, {
        for(j in 1:nrow(example_normed)){
          
          Sys.sleep(1)
          incProgress(1/nrow(example_normed), detail = paste("Working on compound", j,"/", nrow(example_normed)))
          
        }
        
      })
      
      
    }
    
    
    
    
    png(filename="PCA plot.png", width = 1000, height = 1000 * ifelse(with_validate,2,2))
    qc_RSD_performance = sapply(qc_RSDs,median, na.rm = TRUE)
    qc_RSD_performance = sort(qc_RSD_performance,decreasing = TRUE)
    qc_RSD_performance_color = rep("grey",length(qc_RSD_performance))
    qc_RSD_performance_color[length(qc_RSD_performance_color)-1] = "red"
    qc_RSD_performance_color[length(qc_RSD_performance_color)] = "#ffbf00"
    qc_RSD_performance_color[names(qc_RSD_performance)=='none'] = 'black'
    if(with_validate){
      
      val_RSD_performance = sapply(val_RSDs,median, na.rm = TRUE)
      val_RSD_performance = sort(val_RSD_performance,decreasing = TRUE)
      val_RSD_performance_color = rep("grey",length(val_RSD_performance))
      val_RSD_performance_color[length(val_RSD_performance_color)-1] = "red"
      val_RSD_performance_color[length(val_RSD_performance_color)] = "#ffbf00"
      val_RSD_performance_color[names(val_RSD_performance)=='none'] = 'black'
      layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
    }else{
      layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
    }
    
    
    pca_color = factor(p$sampleType, levels = c('sample','qc','validate'))
    dots = c(1,16,16)[as.numeric(pca_color)]
    
    sds = apply(e,1,sd)
    
    pca_before = prcomp(t(e[!sds==0,]),scale. = TRUE)
    # par(mar=c(4,2,4,2)*3)
    plot(pca_before$x[,1],pca_before$x[,2], col = pca_color,main = 'Before',xlab='raw data',cex.lab=5,yaxt='n', cex.axis=5, cex.main=5, cex.sub=5,ylab="", xaxt='n',cex = 5,pch = dots)
    
    
    
    pca_after = prcomp(t(normalized_dataset[[names(qc_RSD_performance)[length(qc_RSD_performance)]]]),scale. = TRUE)
    plot(pca_after$x[,1],pca_after$x[,2], col = pca_color,main = 'After',xlab = names(qc_RSD_performance)[length(qc_RSD_performance)],cex.lab=5, cex.axis=5, cex.main=5, cex.sub=5,ylab="",yaxt='n', xaxt='n',cex = 5,pch = dots)
    #legend("topright", levels(pca_color),col=c("black","red","green"),pch=c(1,16,16),box.lwd=1,  box.col="black",box.lty=1,cex=5, inset=c(-0.2,0))
    dev.off()
    # dir.create(paste0(dir,"\\normalized datasets"))
    
    cat(592)
    for(i in 1:length(methods)){
      if(identical(class(normalized_dataset[[methods[i]]]),"matrix")){
        fwrite(data.table(label = f$label,normalized_dataset[[methods[i]]]),paste0("normalized by - ",methods[i],'.csv'))
      }
    }
    RSDs = data.table(label = f$label, do.call('cbind',qc_RSDs))
    fwrite(RSDs,"RSDs.csv")
    
    
    cat(600)
    cat("\n")
    print(with_validate)
    if(with_validate){
      RSDs = cbind(RSDs, do.call("cbind",val_RSDs))
      fwrite(RSDs,"RSDs.csv")
    }
    
    cat(length(qc_RSDs))
    
    #shinyjs::enable("go")
    showNotification("Ready to Download. Click Download Button.", duration = 15000)
    shinyjs::enable("downloadData")
    
    # RSD(e_norm[,p$sampleType=='validate'])
    # showNotification("Does this show? If shown, then error occurs before Aggregating Normalized Compounds...", duration = 15000)
    # return(paste0("The average QC cross-validated RSD changed from ",signif(median(qc_RSDs[[1]]*100),2),"% to ",signif(median(qc_RSDs[[2]]*100),2),"%. Now you can click Download Result button to save results."))
    return("Finished!")
    
    
  })
  output$text <- renderText({
    result_text()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "SERRF Result.zip"
    },
    content = function(fname) {
      zip(fname, c("PCA plot.png",paste0("normalized by - SERRF.csv"),"RSDs.csv"))
    },
    contentType = "application/zip"
  )
  
  

  
})

shinyApp(ui = ui, server = server)
