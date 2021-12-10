######################### Data panel #########################  sendDesign


###### Interface

output$dataip_ui <- renderUI({
  switch(input$dataiptype,
         "upload"= wellPanel(
           h4("From file(s)"),
           fileInput('uploadfile', 'Choose a File', multiple=TRUE, accept=c('text/csv', 'text/comma-separated-values,
        text/plain', '.csv', '.RData', '.rdata', '.rda')),
           radioButtons('filetype', 'File type:', c(csv='csv', delim='delim', 
                                                    RData='rdat'), inline=TRUE),
           conditionalPanel("input.filetype=='delim'",               
                            selectizeInput('datasep', 'Data fields seperated by:', choices = c(Tab="\t", 
                                                                                               Space=" ", Comma=",", "|", Empty=""), options = list(create = TRUE))),
           conditionalPanel("input.filetype!='rdat'",
                            selectizeInput('missing', 'Missing value shown as:', choices = c(Empty="", 
                                                                                             Dot=".", "*"), options = list(create = TRUE)))
         ),
         
         "examples"= wellPanel(
           selectInput("localdata",  h4("From example datasets:"), c("Choose"="", localdatanames))),
         
         "clipboard"= wellPanel(
           h4("From clipboard:"),
           checkboxInput("dataheader", "Data with header", TRUE),
           selectizeInput('datasep', 'Data fields seperated by:', choices = c(Tab="\t", Space=" ", Comma=",", "|", Empty=""), 
                          options = list(create = TRUE)),         
           selectizeInput('missing', 'In the data missing value is:', 
                          choices = c(Empty="", Dot=".", "*"), options = list(create = TRUE)),           
           textAreaInput(inputId="dataArea", label=NULL, value = "", placeholder = "Paste your data here."),
           textInput("dataname",  "Data name:", value = "ClipboardData"),
           br(),
           actionButton('loadClipData', 'Submit data', class = "btn-success"))
  )
})

output$datarenames_ui <- renderUI({
  
  if (!is_empty(input$selecteddata)) { #  && input$filetype!="rdat"
    wellPanel(	    
      h4("Variable rename/factorize:"),
      div(class="row",
          div(class="col-xs-6",
              selectInput("Year", label = "Year:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Season", label = "Season:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Location", label = "Location:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Replicates", label = "Replicates:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Row", label = "Row:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Column", label = "Column:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Sample", label = "Sample:", choices=c("NULL", names(r_data[[input$selecteddata]])))),
          div(class="col-xs-6",
              selectInput("Check", label = "Check:", choices=c("NULL", names(r_data[[input$selecteddata]])))),				  
          div(class="col-xs-6",
              selectInput("Line", label = "Line:", choices=c("NULL", names(r_data[[input$selecteddata]]))))			  
      ),	  
      actionButton("varrenamesrun", "Run", class = "btn-success")  
    )	
  }
})

output$datatypeChange_ui <- renderUI({
  if (!is_empty(input$selecteddata)) { 
    wellPanel(
      h4("Change variable type:"),
      selectizeInput('vars2Change', 'Select variables:', choices = names(r_data[[input$selecteddata]]), multiple = TRUE, options = list(plugins = list('remove_button', 'drag_drop'))),
      radioButtons('changetype', 'Change to:', c(Factor='factor', Chr="character", Num='numeric'), inline=TRUE),
      br(),
      actionButton("varTypeChangerun", "Run", class = "btn-success btn-sm")	
    ) 
  }
})

###### Process

r_data <- reactiveValues()

upload_error_handler <- function(objname, ret) {
  ## create an empty data.frame and return error message as description
  r_data[[paste0(objname,"_descr")]] <- ret
  r_data[[objname]] <- data.frame(matrix(rep("",12), nrow = 2))
}

loadUserData <- function(fname, uFile, filetype, sep="\t", na="") {
  
  filename <- basename(fname)
  ext <- tools::file_ext(filename)
  ## objname is used as the name of the data.frame
  objname <- sub(paste0(".", ext,"$"),"", filename)
  for (i in 1:20) objname <- sub(" ","", objname)
  
  if (filetype == 'rdat') {
    if (ext %in% c("RData", "rdata", "rda")) {
      ## objname will hold the name of the object(s) inside the R datafile
      robjname <- try(load(uFile), silent = TRUE)
      if (is(robjname, 'try-error')) {
        upload_error_handler(objname, "### There was an error loading the data. 
          Please make sure the data are in either rda or csv format.")
      } else if (length(robjname) > 1) {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      } else {
        r_data[[objname]] <- as.data.frame(get(robjname))
        r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
        rm(robjname)
      }
    }else upload_error_handler(objname,"### Plase select a *.RData file!")   
  }
  
  if (filetype == 'csv') {
    if (ext %in% c("csv")) {  
      robjname <- try(read_csv(uFile, na=input$missing), TRUE)
      if (is(robjname, 'try-error') | nrow(robjname)>0) {
        robjname <- try(read.csv(uFile, na=input$missing, stringsAsFactors=FALSE), TRUE)
        if (is(robjname, 'try-error')) upload_error_handler(objname, "Please select a csv file with a right format!")
        else r_data[[objname]] <- robjname		
      }else{
        r_data[[objname]] <- robjname
        r_data[[paste0(objname,"_descr")]] <- ""
      }
    }else upload_error_handler(objname,"### Plase select a *.csv file!") 
  }
  
  if (filetype == 'csv2') {
    if (ext %in% c("csv")) {  
      robjname <- try(read_csv2(uFile, na=input$missing), TRUE)
      if (is(robjname, 'try-error') | nrow(robjname)>0) 
        upload_error_handler(objname, "Please select a csv2 file with a right format!") 
      else{
        r_data[[objname]] <- robjname
        r_data[[paste0(objname,"_descr")]] <- ""
      }
    }else upload_error_handler(objname,"### Plase select a *.csv file!")
  }
  
  if (filetype == 'delim') { 
    robjname <- try(read.delim(uFile, sep=sep, na.strings=na, stringsAsFactors=FALSE), TRUE)
    if (is(robjname, 'try-error')) upload_error_handler(objname, "Please select a file with a right delim!") 
    else{
      r_data[[objname]] <- robjname
      r_data[[paste0(objname,"_descr")]] <- ""
    }
  }  
  
  r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique
}

# loading data
observeEvent(input$uploadfile, {
  # loading files from disk
  inFile <- input$uploadfile
  if (is.null(inFile)) return()
  # iterating through the files to upload
  for (i in 1:(dim(inFile)[1]))
    loadUserData(inFile[i,'name'], inFile[i,'datapath'], input$filetype,
                 sep = input$datasep, na = input$missing)
  updateSelectInput(session, "selecteddata", label = "Datasets:",
                    choices = r_data$datasetlist,
                    selected = r_data$datasetlist[1])
  #updateTextInput(session, "updtdataname", label = "Name of data file to save:", value = r_data$datasetlist[1])
}) 

# clipboard data
loadClipboardData <- function(objname = "ClipboardData", ret = "", header = TRUE, sep = "\t", na="") { 
  dat <- sshhr(try(
    {if (Sys.info()["sysname"] == "Windows") {
      read.table("clipboard", header = header, sep = sep, na.strings = na, 
                 comment.char = "", fill = TRUE,  as.is = TRUE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      read.table(pipe("pbpaste"), header = header, sep = sep, na.strings = na, 
                 comment.char = "", fill = TRUE,  as.is = TRUE)
    } else {
      if (!is_empty(input$dataArea))
        read.table(text = input$dataArea, header = header, sep = sep, na.strings = na, 
                   comment.char = "", fill = TRUE,  as.is = TRUE)
    }} %>% as.data.frame(check.names = FALSE), silent = TRUE))
  
  # dat <- sshhr(try(read.table(text = input$dataArea, header = header, sep = sep, na.strings = na, 
  # comment.char = "", fill = TRUE,  as.is = TRUE) %>% 
  # as.data.frame(check.names = FALSE), silent = TRUE))	
  
  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on ", lubridate::now())
    r_data[[objname]] <- dat
    r_data[[paste0(objname,"_descr")]] <- ret 
  }
  r_data[['datasetlist']] <- unique(c(objname, r_data[['datasetlist']]))
}

observeEvent(input$loadClipData, {
  # loading files from clipboard
  objname <- gsub(" ","", input$dataname)
  objname <- ifelse(is_empty(objname), "ClipboardData", objname)
  if (objname=="ClipboardData")  objname <- paste0("Clipboard", format(Sys.time(), "_%Y%m%d_%H%M%S"))
  
  loadClipboardData(objname = objname, ret = "", header = input$dataheader, sep = input$datasep, na=input$missing)
  updateSelectInput(session, "selecteddata", label = "Datasets:",
                    choices = r_data$datasetlist, selected = objname)
  # updateTextInput(session, "updtdataname", label = "Name of data file to save:", value = objname)
  updateTextAreaInput(session, inputId="dataArea", label=NULL, value = "", placeholder = "Paste your data here.")
}) 

# example data
observeEvent(input$localdata, {
  # loading examples data
  if (!is_empty(input$localdata)) {
    r_data[[input$localdata]] <- try(get(input$localdata), TRUE)
    # if (input$localdata %in% localdatainfo[,1]) {
    # r_data[[paste0(input$localdata,"_descr")]] <- localdatainfo[localdatainfo[,1]==input$localdata,2]
    # }else 
    r_data[[paste0(input$localdata,"_descr")]] <- ""
    r_data[['datasetlist']] <- unique(c(input$localdata, r_data[['datasetlist']]))
    updateSelectInput(session, "selecteddata", label = "Datasets:",
                      choices = r_data$datasetlist) #, selected = input$localdata)
    # updateTextInput(session, "updtdataname", label = "Name of data file to save:", value = input$localdata)
  }
})

# rename variables
observeEvent(input$varrenamesrun, {  #eventReactive
  if (!is_empty(input$selecteddata)){
    rninput <- c(input$Year, input$Season, input$Location, input$Replicates, input$Row, input$Column, input$Sample, input$Line)
    rninput <- rninput[rninput!="NULL"]
    if (all(table(rninput)==1)){
      if (input$Year !="NULL")  r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]]%>% dplyr::rename("Year"=input$Year) %>% dplyr::mutate(Year=factor(Year, levels=unique(Year))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Season !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]]%>% dplyr::rename("Season"=input$Season) %>% dplyr::mutate(Season=factor(Season, levels=unique(Season))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Location !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Location"=input$Location) %>% dplyr::mutate(Location=factor(Location, levels=unique(Location))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Replicates !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Replicates"=input$Replicates) %>% dplyr::mutate(Replicates=factor(Replicates, levels=unique(Replicates))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Row !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Row"=input$Row) %>% dplyr::mutate(Row=factor(Row, levels=unique(Row))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Column !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Column"=input$Column) %>% dplyr::mutate(Column=factor(Column, levels=unique(Column))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Sample !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Sample"=input$Sample) %>% dplyr::mutate(Sample=factor(Sample, levels=unique(Sample))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Check !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Check"=input$Check) %>% dplyr::mutate(Check=factor(Check, levels=unique(Check))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      if (input$Line !="NULL") r_data[[input$selecteddata]] <- try(r_data[[input$selecteddata]] %>% dplyr::rename("Line"=input$Line) %>% dplyr::mutate(Line=factor(Line, levels=unique(Line))), TRUE) %>% if(!is(., "try-error")) . else r_data[[input$selecteddata]]
      # r_data[['datasetlist']] <- unique(c("renamedData", r_data[['datasetlist']]))
      # updateSelectInput(session, "selecteddata", label = "Datasets:", choices = r_data$datasetlist, selected = "renamedData")
    }
  }
  
})

# Change data type

observeEvent(input$varTypeChangerun, { 
  if (length(input$vars2Change) > 0) {
    if (input$changetype=="factor") {
      r_data[[input$selecteddata]] <- dplyr::mutate_at(r_data[[input$selecteddata]], input$vars2Change, factor2)
      # for (i in input$vars2Change) {r_data[[i]] <- factor2(r_data[[i]]); print(class(r_data[[i]]))}  
    }else if (input$changetype=="character") {
      r_data[[input$selecteddata]] <- dplyr::mutate_at(r_data[[input$selecteddata]], input$vars2Change,  as.character)
      # for (i in input$vars2Change) r_data[[input$selecteddata]][[i]] <- as.character(r_data[[input$selecteddata]][[i]])
    }else if (input$changetype=="numeric") {
      for (i in input$vars2Change) {	
        if (is.factor(r_data[[input$selecteddata]][[i]])) r_data[[input$selecteddata]][[i]] <- as.character(r_data[[input$selecteddata]][[i]])
        r_data[[input$selecteddata]][[i]] <- as.numeric(r_data[[input$selecteddata]][[i]])
      }
    }else NULL    
  }else NULL	
})

###### Output  localdatainfo

output$datastr1 <- renderPrint({
  if (is_empty(input$selecteddata)) "Please upload a dataset!"
  else{
    if (is_df(r_data[[input$selecteddata]])) {
      cat(r_data[[paste0(input$selecteddata,"_descr")]], "\n\n")
      if (input$datainfo=="structure") print(str(r_data[[input$selecteddata]])) 
      else print(summary(r_data[[input$selecteddata]], maxsum=20))
    }
  }
})

# Show the first "n" observations
observeEvent(input$sendDesign, {	
  updateNavbarPage(session, "nav_DeltaGen", "abcde")
  updateTabsetPanel(session, "DataView_Input", selected = "10111")
  
  output$hotInputView <- renderRHandsontable({
    if (is.data.frame(designout$designdf) && "Line" %in% names(designout$designdf)){
      if (input$sendDesignOpt){
        
        dlayout <- paste(input$designstart, input$designdirct, sep="_")
        designdf_ini <- designout$designdf %>% 
          mutate_at(c("Replicates", "Row", "Column"), as.integer) 
        rodd <-  max(designdf_ini$Row) %% 2 == 0
        codd <-  max(designdf_ini$Column) %% 2 == 0
        
        if (dlayout=="TL_H") {
          ini_data_design1 <- designdf_ini %>%
            arrange(Replicates, Row, Column) %>% 
            dplyr::filter(Row%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            arrange(Replicates, Row, desc(Column)) %>% 
            dplyr::filter(Row%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, Row)
        }else if (dlayout=="TL_V"){
          ini_data_design1 <- designdf_ini %>%
            arrange(Replicates, Column, Row) %>% 
            dplyr::filter(Column%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            arrange(Replicates, Column, desc(Row)) %>% 
            dplyr::filter(Column%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, Column)
        }else if (dlayout=="BL_H"){
          ini_data_design1 <- designdf_ini %>%
            {
              if(rodd) arrange(., Replicates, desc(Row), desc(Column)) else arrange(., Replicates, desc(Row), Column)
            } %>% 
            dplyr::filter(Row%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            {
              if(rodd) arrange(., Replicates, desc(Row), Column) else arrange(., Replicates, desc(Row), desc(Column))
            }  %>%
            dplyr::filter(Row%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, desc(Row))
        }else if (dlayout=="BL_V"){
          ini_data_design1 <- designdf_ini %>%
            arrange(Replicates, Column, desc(Row)) %>% 
            dplyr::filter(Column%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            arrange(Replicates, Column, Row) %>% 
            dplyr::filter(Column%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, Column)
        }else if (dlayout=="TR_H"){
          ini_data_design1 <- designdf_ini %>%
            arrange(Replicates, Row, desc(Column)) %>% 
            dplyr::filter(Row%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            arrange(Replicates, Row,  Column) %>% 
            dplyr::filter(Row%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, Row)
        }else if (dlayout=="TR_V"){
          ini_data_design1 <- designdf_ini %>%
            {
              if(codd) arrange(., Replicates, desc(Column), desc(Row)) else arrange(., Replicates, desc(Column), Row)
            }  %>% 
            dplyr::filter(Column%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            {
              if(codd) arrange(., Replicates, desc(Column), Row) else arrange(., Replicates, desc(Column), desc(Row))
            }  %>%
            dplyr::filter(Column%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, desc(Column))
        }else if (dlayout=="BR_H"){
          ini_data_design1 <- designdf_ini %>%
            {
              if(rodd) arrange(., Replicates, desc(Row), Column) else arrange(., Replicates, desc(Row), desc(Column))
            }  %>%
            dplyr::filter(Row%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            {
              if(rodd) arrange(., Replicates, desc(Row), desc(Column)) else arrange(., Replicates, desc(Row), Column)
            }  %>% 
            dplyr::filter(Row%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, desc(Row))
        }else if (dlayout=="BR_V"){
          ini_data_design1 <- designdf_ini %>%
            {
              if(codd) arrange(., Replicates, desc(Column), Row) else arrange(., Replicates, desc(Column), desc(Row))
            }  %>% 
            dplyr::filter(Column%%2!=0)
          
          ini_data_design2 <- designdf_ini %>%
            {
              if(codd) arrange(., Replicates, desc(Column), desc(Row)) else arrange(., Replicates, desc(Column), Row)
            }  %>% 
            dplyr::filter(Column%%2==0)  
          
          ini_data_design <- bind_rows(ini_data_design1, ini_data_design2) %>% 
            arrange(Replicates, desc(Column))
        }
        
      }else{
        ini_data_design <- designout$designdf %>% 
          mutate_at(c("Replicates", "Row", "Column"), as.integer) %>%
          arrange(Replicates, Row, Column)
      }
      
      ini_dataV <- full_join(ini_data, ini_data_design) %>% 
        dplyr::mutate(Breeder=input$breederName,
               Season = as.character(lubridate::date(Sys.time())),
               Year = as.character(lubridate::year(Sys.time())),
               Location=input$locationPlace) %>% 
        dplyr::mutate_if(is.integer, as.character) 				
      if(all(is.na(ini_dataV$Sample))) ini_dataV$Sample <- NULL
      if(all(is.na(ini_dataV$Check))) ini_dataV$Check <- NULL
      
      # ini_dataV <- ini_data_design %>% 
      # mutate(Breeder=NA,
      # location=NA,
      # Season = as.character(lubridate::date(Sys.time())),
      # Year = as.character(lubridate::year(Sys.time())))
      
      NumericDf <- matrix(NA, nrow=nrow(ini_data_design), ncol=length(input$traitsName))
      colnames(NumericDf) <- input$traitsName
      
      ini_dataV <- cbind(ini_dataV, NumericDf) %>% 
        mutate_at(input$traitsName, as.numeric)
      rhandsontable(ini_dataV, height=600, width=800, rowHeaders = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  r_data$iniDataName <- "design"
})

observeEvent(input$editData, {
#  updateNavbarPage(session, "nav_DeltaGen", "abcde")
  updateTabsetPanel(session, "DataView_Input", selected = "10111")
  
  output$hotInputView <- renderRHandsontable({
    if (!is_empty(input$selecteddata)){
      r_data$iniDataName <- input$selecteddata
      rhandsontable(r_data[[input$selecteddata]], height=600, width=800, rowHeaders = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
})

observeEvent(input$updateBtn, {
  updateTabsetPanel(session, "DataView_Input", selected = "10110")
  if (!is_empty(input$hotInputView)) {
    inputDataName <- paste0(r_data$iniDataName, format(Sys.time(), "_%Y%m%d_%H%M%S"))
    
    r_data[[inputDataName]] <- hot_to_r(input$hotInputView) %>% 
      mutate_if(is.character, as.factor)
    
    r_data[['datasetlist']] <- c(inputDataName, r_data[['datasetlist']]) %>% unique
    r_data[[paste0(inputDataName,"_descr")]] <- ""
    updateSelectInput(session, "selecteddata", label = "Datasets:",
                      choices = r_data$datasetlist, selected = inputDataName)
    # updateTextInput(session, "updtdataname", label = "Name of data file to save:", value = inputDataName)
  }
})

observeEvent(input$selecteddata, {
  updateTextInput(session, "updtdataname", label = "Data name:", value = input$selecteddata)
})

output$view1 <- DT::renderDataTable({
  if (is_empty(input$selecteddata)) {
    #    if (!(is.data.frame(designout$designdf) && "Line" %in% names(designout$designdf)))
    data.frame(matrix(rep("",12), nrow = 2))
  }else{
    if (is_df(r_data[[input$selecteddata]])) DT::datatable(r_data[[input$selecteddata]], rownames = FALSE, extensions = c("Responsive", 'Buttons'), options = list(dom = 'Bfrtip', buttons = I('colvis')))
  }
})

# save updtdata
output$saveupdtdata <- downloadHandler(
  filename <- function(){
    paste(input$updtdataname, sep = '.', input$updtdataformat)
  },
  content = function(file) {
    assign(input$updtdataname, droplevels(data.frame(r_data[[input$selecteddata]])))
    if (input$updtdataformat=="RData") eval(parse(text=paste("save(", input$updtdataname, ", file = file)")))
    if (input$updtdataformat=="csv") eval(parse(text=paste("write.csv(", input$updtdataname, ", file = file, row.names=FALSE)")))
  }
)# end of save updtdata

