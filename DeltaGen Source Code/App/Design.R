# options(browser="C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
# runApp()

###### sever interface  savedesignLayout
output$designinput_ui <- renderUI({
  times <- input$reset_designinput
  div(id=letters[(times %% length(letters))+1],
      switch(input$designtype,
             "Completely Randomized"= wellPanel(
               div(class="row",
                   div(class="col-xs-5",
                       numericInput("crTrtno", "No. of Trt:", min=2, max=1000, value=3)),
                   div(class="col-xs-7",
                       textInput("ntrts", "No. of Rep/Trt:",value="5, 4, 3"))		  
               ),		   
               helpText("Enter number of repeats per treatment (Trt)."),
               h5("Plot layout:"),
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("crdrow", "Row:", min=2, max=1000, value=4)),
                   div(class="col-xs-6",
                       numericInput("crdcol", "Column:", min=2, max=1000, value=3))		  
               ),
               textAreaInput(inputId="crTrtName", label=NULL, value = "", placeholder = "Enter treatment names with no space here.")
             ),        
             "Randomized Complete Block"= wellPanel(
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("rcbnblocks", "No. of Block:", min=2, max=100, step=1, value=4)),
                   div(class="col-xs-6",
                       numericInput("rcbntrt", "No. of Trt:", min=2, max=1000, value=4))		  
               ),							  
               textInput("rcbntrts", "Trt_rep/block:",value="3, 2, 2, 2"),				  
               helpText("Enter number of repeats per treatment (Trt) per rep/block."),
               h5("Plot layout within block:"),
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("rcbrow", "Row:", min=2, max=1000, value=3)),
                   div(class="col-xs-6",
                       numericInput("rcbcol", "Column:", min=2, max=1000, value=3))		  
               ),
               textAreaInput(inputId="rcbTrtName", label=NULL, value = "", placeholder = "Enter treatment names with no space here.")
               
             ),
             "Factorial"= wellPanel(                          
               div(class="row",
                   div(class="col-xs-6",
                       textInput("fctstr", "No. of level of factors:",value="3, 2"),
                       helpText("Factorial 3 x 2.")),
                   div(class="col-xs-6",
                       numericInput("fctnblocks", "Replications or blocks:", min=2, max=100, step=1, value=4))
               ),
               
               h5("Plot layout within block:"),
               div(class="row",			  
                   div(class="col-xs-6",
                       numericInput("factrow", "Row:", min=2, max=1000, value=2)),
                   div(class="col-xs-6",
                       numericInput("factcol", "Column:", min=2, max=1000, value=3))		  
               ),
               checkboxInput("plusctrl", "Plus control?", value = FALSE)),                        
             # "Split Plot"= wellPanel(               
             # numericInput("spltpblk", "Number of Blocks:", min=6, max=100, step=1, value=6),
             # h5("Main plot layout within block:"),
             # div(class="row",
             # div(class="col-xs-6",
             # numericInput("mpltrow", "Row:", min=1, max=1000, value=3)),
             # div(class="col-xs-6",
             # numericInput("mpltcol", "Column:", min=1, max=1000, value=1))		  
             # ),
             # h5("Sub-plot plot layout within main plot:"),
             # div(class="row",
             # div(class="col-xs-6",
             # numericInput("subpltrow", "Row:", min=1, max=1000, value=2)),
             # div(class="col-xs-6",
             # numericInput("subpltcol", "Column:", min=1, max=1000, value=2))		  
             # ),
             # checkboxInput("spltspltck", "Sub-sub-plot plot layout within sub-plot:", FALSE),
             # conditionalPanel("input.spltspltck",
             # div(class="row",
             # div(class="col-xs-6",
             # numericInput("subsubpltrow", "Row:", min=1, max=1000, value=2)),
             # div(class="col-xs-6",
             # numericInput("subsubpltcol", "Column:", min=1, max=1000, value=2))		  
             # ))),
             "Row and Column"= wellPanel(
               textInput("expName", "Name of the Experiment:",value="Stage I trial"),
               textInput("locationPlace", "Location place:",value="Palmerston_North"),
               textInput("breederName", "Name of the breeder:",value="Zulfi"),
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("NTreatments", "Number of lines:", min=2, max=1000, value=20)),
                   div(class="col-xs-6",
                       numericInput("N_replicate", "Number of replicates:", min=1, max=100, value=3)),
                   div(class="col-xs-6",
                       numericInput("NRows", "Rows in design:", min=2, max=500, value=12)),
                   div(class="col-xs-6",
                       numericInput("NColumns", "Columns in design:", min=1, max=500, value=5)),
                   div(class="col-xs-6",
                       textInput("NChecks", "Number of check positions per replicate:", value="0"))		  
               ),
               radioButtons('replayout', 'Replicate layout:', c('Vertical'='ver', 'Horizontal '='hor'), inline=TRUE),
               textAreaInput(inputId="lineName", label=NULL, value = "", placeholder = "Enter line names with no space here, If there are some checks, put check names first.")			   
             ),
             "p-rep Design"= wellPanel(	
               h4("In each Trt group"),
               textInput("pDtrtno", "No. of Trt:",value="80, 10"),
               textInput("pDtrtrep", "No. of rep:",value="1, 2"),
               helpText("where 80 trts with 1-rep, labelled as 1 to 80, 10 trts with 2-rep, labelled these 81 to 90."),
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("pDrow", "Rows in design:", min=2, max=500, value=10)),
                   div(class="col-xs-6",
                       numericInput("pDcol", "Columns in design:", min=1, max=500, value=10))
               ),
               textInput("pDblkseq", "Block sequence:",value="5, 10"),
               helpText("where 10 2-rep trts (81:90) arranged in each block with 5 rows x 10 cols.")			   
             ) 			 
      )
  )  
})  

output$sendInput_ui <- renderUI({
  if(input$designtype == 'Row and Column' && is.data.frame(designout$designdf)) {
    wellPanel(
      checkboxInput("sendDesignOpt", "Serpentine", value = TRUE, width = NULL),
      conditionalPanel("input.sendDesignOpt",
                       div(class="row",
                           div(class="col-xs-4",
                               selectInput("designstart", h4("Start corner"), c("Top left"="TL", "Top right"="TR", "Bottom left"="BL", "Bottom right"="BR"))),		 
                           div(class="col-xs-4",
                               selectInput("designdirct", h4("Direction"), c("Horizontal"="H", "Vertical"="V"))))
      ),
      div(class="row", 
          div(class="col-xs-4",
              selectizeInput('traitsName', 'Names of traits:', c("Yield", "Growth", "Plant_Hight"),  multiple = TRUE, options = list(create = TRUE, plugins = list('remove_button', 'drag_drop'))))),
      div(class="row",
          div(class="col-xs-4",
              actionButton("sendDesign", "Send to input", class = "btn-success btn-sm"))			  
      )
    )
  }
})

###### Sever program

# setup initial values
designout <- reactiveValues()

observeEvent(input$designrun, {
  if(is.na(input$seed) || input$seed==0) rseed <- NULL else rseed <- c(input$seed, input$seed+111)
  if (input$designtype=='Completely Randomized') {
    if(!is.null(input$crTrtno) && input$crTrtno > 1) {
      replicates <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(input$ntrts, ",")))))
      if (length(replicates)==1) replicates <- rep(replicates, input$crTrtno)
      if (input$crTrtno==length(replicates)) {  
        withProgress(message = 'Running completely randomized design ...', value = 0.1, {
          des <- try(corDiGGer(numberOfTreatments = input$crTrtno,
                               rowsInDesign = input$crdrow,
                               columnsInDesign = input$crdcol, 
                               blockSequence = list(c(input$crdrow,1)),
                               rngSeeds = rseed, treatRep=replicates,
                               spatial=TRUE, runSearch = TRUE), TRUE)
        })
        if ("try-error" %in% class(des)) {
          designout$designinfo <- des[1]
          designout$designdf <- NULL
          designout$designtab <- NULL
          designout$designanova <- NULL
          designout$plotdf <- NULL
        }else{
          designout$designname <- paste("This is a completely randomized design with", input$crTrtno, "treatments, each has replicates of", input$ntrts, "\n\n")                        
          designout$designdf <- des$dlist %>%
            dplyr::select(ROW, RANGE, TRT) %>%
            dplyr::rename(Row=ROW, Col=RANGE, Trt=TRT) %>%
            dplyr::mutate(Plot=1000*Row+Col, Trt=factor(Trt)) %>%
            dplyr::select(Plot, everything())
          designout$designtab <- xtabs(~Trt, designout$designdf)
          designout$designanova <- summary(aov(rnorm(dim(designout$designdf)[1]) ~ Trt, data=designout$designdf))
          designout$plotdf <- designout$designdf %>%
            dplyr::mutate(x=Col,
                   y=max(Row)+1-Row,
                   block=1,
                   colTrt=Trt,
                   labelTrt=Trt)
          
          if (!is_empty(input$crTrtName)) {
            # LineLevels <- unique(unlist(strsplit(input$crTrtName, split="[^[:alnum:]]+")))
            LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$crTrtName), split="\\W+")))
            #LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$crTrtName), split="[[:space:]]+|,|\\n")))
            if (length(LineLevels)==nlevels(designout$designdf$Trt)){
              levels(designout$designdf$Trt) <- LineLevels
              designout$plotdf$labelTrt <- designout$plotdf$colTrt <- as.character(designout$designdf$Trt)
            }else{
              updateTextAreaInput(session, inputId="crTrtName", label = NULL, value = paste("Please enter", input$crTrtno, "treatment names."))
            }
          }
        }        
      }else{
        designout$designinfo <- "Check your replicates input!"
        designout$designdf <- NULL   
        designout$designtab <- NULL
        designout$designanova <- NULL
        designout$plotdf <- NULL		
      }
    }else{
      designout$designinfo <- "Select or enter at least two treatment names!"
      designout$designdf <- NULL
      designout$designtab <- NULL
      designout$designanova <- NULL
      designout$plotdf <- NULL	  
    }  
  }else if (input$designtype=='Randomized Complete Block') { 
    if(!is.null(input$rcbnblocks) && input$rcbnblocks > 1) {
      replicates <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(input$rcbntrts, ",")))))
      if (length(replicates)==1) replicates <- rep(replicates, input$rcbntrt)
      if (input$rcbntrt==length(replicates)) {
        withProgress(message = 'Running randomized complete block design ...', value = 0.1, {
          des <- try(rcDiGGer(numberOfTreatments=input$rcbntrt, 
                              rowsInDesign=input$rcbrow*input$rcbnblocks, 
                              columnsInDesign=input$rcbcol, 
                              rowsInReplicate = input$rcbrow, 
                              columnsInReplicate = input$rcbcol,
                              twoPhase = "rowThenCol",
                              fixedBlocks = TRUE, rngSeeds = rseed,
                              treatRepPerRep = replicates, runSearch = TRUE), TRUE)
        })
        if ("try-error" %in% class(des)) {
          designout$designinfo <- des[1]
          designout$designdf <- NULL
          designout$designtab <- NULL
          designout$designanova <- NULL
          designout$plotdf <- NULL
        }else{
          designout$designname <- paste("This is a completely randomized block design with", input$rcbntrt, "treatments each has replicates of", input$rcbntrts, "\n\n")                        	  
          designout$designdf <- des$dlist %>%
            dplyr::rename(Block=REP, Row=ROW, Col=RANGE, Treat=TRT) %>%
            dplyr::arrange(Block, Col, Row) %>%
            dplyr::mutate(Block=factor(Block), Row=rep(1:input$rcbrow, length=dim(des$dlist)[1]), Treat=factor(Treat)) %>%
            dplyr::select(Block, Row, Col, Treat)				
          designout$designtab <- xtabs(~Block+Treat, designout$designdf)
          designout$designanova <- summary(aov(rnorm(dim(designout$designdf)[1]) ~ Treat+Error(Block), data=designout$designdf))
          designout$plotdf <- designout$designdf %>%
            dplyr::mutate(x=Col,
                   y=max(Row)+1-Row,
                   block=Block,
                   colTrt=Treat,
                   labelTrt=Treat)
          if (!is_empty(input$rcbTrtName)) {
            
            # LineLevels <- unique(unlist(strsplit(input$rcbTrtName, split="[^[:alnum:]]+")))
            LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$rcbTrtName), split="\\W+")))
            #LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$rcbTrtName), split="[[:space:]]+|,|\\n")))
            if (length(LineLevels)==nlevels(designout$designdf$Treat)){
              levels(designout$designdf$Treat) <- LineLevels
              designout$plotdf$labelTrt <- designout$plotdf$colTrt <- as.character(designout$designdf$Treat)
            }else{
              updateTextAreaInput(session, inputId="rcbTrtName", label = NULL, value = paste("Please enter", input$rcbntrt, "treatment names."))
            }
          }		          
        }        
      }else{
        designout$designinfo <- "Check your replicates input!"
        designout$designdf <- NULL  
        designout$designtab <- NULL
        designout$designanova <- NULL
        designout$plotdf <- NULL		
      }
    }else{
      designout$designinfo <- "Select or enter at least two treatment names!"
      designout$designdf <- NULL  
      designout$designtab <- NULL
      designout$designanova <- NULL
      designout$plotdf <- NULL	  
    }  
  }else if (input$designtype=='Factorial') {
    if(!is.null(input$fctstr)) {
      fctstr <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(input$fctstr, ",")))))
      nfct <- prod(fctstr)
      if (input$plusctrl) {          
        DFfac <- createFactorialDF(c(2, fctstr+1))          
        DFfacN <- DFfac
        for (i in 1:(length(fctstr)+1)) DFfacN <- DFfacN[DFfacN[,paste("F",i, sep="")]!=1,]
        DFfac <- rbind(DFfac[1,], DFfacN) 
        DFfac$Repeats[1] <- input$factrow*input$factcol-nfct          
        fnames <- paste("F", 1:(length(fctstr)+1), sep="")
        withProgress(message = 'Running factorial + control design ...', value = 0.1, {
          des <- try(facDiGGer(
            factorNames = fnames,
            rowsInDesign = input$factrow*input$fctnblocks, columnsInDesign = input$factcol,
            rowsInRep = input$factrow, columnsInRep = input$factcol,
            # blockSequence = list(c(nfct%/%input$fctnblocks,1)),
            blockSequence = list(c(1,1)),
            chequerboard = TRUE,
            treatDataFrame = DFfac,
            # maxInt=500000, 
            rngSeeds = rseed), TRUE)
        })
      }else {
        DFfac <- createFactorialDF(fctstr)
        fnames <- paste("F", 1:length(fctstr), sep="")
        withProgress(message = 'Running factorial design ...', value = 0.1, {
          des <- try(facDiGGer(
            factorNames = fnames,#  c("F1", "F2", "F3"),
            rowsInDesign = input$factrow*input$fctnblocks, columnsInDesign = input$factcol,
            rowsInRep = input$factrow, columnsInRep = input$factcol,
            # blockSequence = list(c(nfct%/%input$fctnblocks,1)),
            treatDataFrame = DFfac,
            treatRepColumn = "Repeats",
            maxInt=500000, 
            rngSeeds = rseed), TRUE)
        })
      }        
      if ("try-error" %in% class(des)) {
        designout$designinfo <- des[1]
        designout$designdf <- NULL
        designout$designtab <- NULL
        designout$designanova <- NULL
        designout$plotdf <- NULL
      }else{
        designout$designname <- paste("This is a factorial design with treatment structure of", paste0(fctstr, collapse="x"), "each has replicates of", input$fctnblocks,
                                      "The design type is", input$fctdesigntype)                        
        designout$designdf <- des$dlist %>%
          dplyr::rename(Block=REP, Row=ROW, Col=RANGE) %>%
          dplyr::arrange(Block, Col, Row) %>%
          dplyr::mutate(Block=factor(Block), Row=rep(1:input$factrow, length=dim(des$dlist)[1])) %>%
          .[,c("Block", "Row", "Col", fnames)]	
        for (i in 4:dim(designout$designdf)[2]) designout$designdf[,i] <- factor(designout$designdf[,i])
        designout$designtab <- xtabs(~Block, designout$designdf)
        simy <- rnorm(dim(designout$designdf)[1])
        if (input$plusctrl) {
          fo <- formula(paste("simy ~ F1/(", paste(fnames[-1], collapse="*"), ")"))
          designout$designanova <- anova(lm(fo, data=designout$designdf))[,1, drop=FALSE]                       
        }else{            
          fo <- formula(paste("simy ~", paste(fnames, collapse="*"), "+Error(Block)"))
          designout$designanova <- summary(aov(fo, data=designout$designdf))           
        }
        designout$plotdf <- designout$designdf %>%
          dplyr::mutate(x=Col,
                 y=max(Row)+1-Row,
                 block=Block)
        designout$plotdf$colTrt <- designout$plotdf$labelTrt <- do.call("paste", c(designout$designdf[,fnames], sep = "_"))						 
      }
    }else{
      designout$designinfo <- "Enter a suitable factorial structure!"
      designout$designdf <- NULL
      designout$designtab <- NULL
      designout$designanova <- NULL
      designout$plotdf <- NULL
    }   
  }else if(input$designtype=='Row and Column') {
    
    # if (input$designtype=='Split Plot') {
    # input$designrun
    # isolate({
    # if(!is.null(input$mpltrow) && !is.null(input$mpltcol)) {
    # nF1 <- input$mpltrow*input$mpltcol
    # nF2 <- input$subpltrow*input$subpltcol
    # if (!input$spltspltck) {
    # dsgrow <- input$spltpblk*input$mpltrow*input$subpltrow
    # dsgcol <- input$mpltcol*input$subpltcol        
    # DF <- createFactorialDF(c(nF1, nF2))
    # withProgress(message = 'Running split plot design ...', value = 0.1, {
    # des <- try(facDiGGer(
    # factorNames = c("F1", "F2"),
    # rowsInDesign = dsgrow, columnsInDesign = dsgcol,
    # rowsInRep = input$mpltrow*input$subpltrow, columnsInRep = dsgcol,
    # mainPlotSizes = list(c(input$subpltrow, input$subpltcol), c(1,1)),
    # treatDataFrame = DF,
    # treatRepColumn = "Repeats",
    # maxInt=100000,
    # rngSeeds = rseed), TRUE)
    # })
    # }else{
    # nF3 <- input$subsubpltrow*input$subsubpltcol
    # dsgrow <- input$spltpblk*input$mpltrow*input$subpltrow*input$subsubpltrow
    # dsgcol <- input$mpltcol*input$subpltcol*input$subsubpltcol		  
    # DF <- createFactorialDF(c(nF1, nF2, nF3))
    # print(c(dsgrow, dsgcol))
    # withProgress(message = 'Running spilt-split plot design ...', value = 0.1, {
    # des <- try(facDiGGer(
    # factorNames = c("F1", "F2", "F3"),
    # rowsInDesign = dsgrow, columnsInDesign = dsgcol,
    # rowsInRep = input$mpltrow*input$subpltrow*input$subsubpltrow, columnsInRep = dsgcol,
    # mainPlotSizes = list(c(input$subpltrow*input$subsubpltrow, input$subpltcol*input$subsubpltcol), c(input$subsubpltrow, input$subsubpltcol), c(1,1)), # 
    # treatDataFrame = DF,
    # treatRepColumn = "Repeats",
    # maxInt=100000,
    # rngSeeds = rseed), TRUE)
    # })
    # }
    
    
    # if ("try-error" %in% class(des)) {
    # designout$designinfo <- des[1]
    # designout$designdf <- NULL
    # }else{
    
    # if(!input$spltspltck) {
    # designout$designdf <- des$dlist %>%
    # dplyr::rename(Block=REP, Row=ROW, Col=RANGE) %>%
    # dplyr::arrange(Block, Col, Row) %>%
    # dplyr::mutate(Block=factor(Block), Row=rep(1:(input$mpltrow*input$subpltrow), length=dim(des$dlist)[1])
    # ) %>% .[,c("Block", "Row", "Col", "F1", "F2")]
    
    # for (i in 4:dim(designout$designdf)[2]) designout$designdf[,i] <- factor(designout$designdf[,i])
    # designout$designtab <- xtabs(~Block+F1+F2, designout$designdf) 
    # simy <- rnorm(dim(designout$designdf)[1])		  
    # designout$designanova <- anova(lmer(simy ~ F1*F2+(1|Block/F1), data=designout$designdf))
    # designout$plotdf <- designout$designdf %>%
    # dplyr::mutate(x=Col,
    # y=max(Row)+1-Row,
    # block=Block,
    # colTrt=F1,
    # labelTrt=F2)
    # }else{
    # designout$designdf <- des$dlist %>%
    # dplyr::rename(Block=REP, Row=ROW, Col=RANGE) %>%
    # dplyr::arrange(Block, Col, Row) %>%
    # dplyr::mutate(Block=factor(Block), Row=rep(1:(input$mpltrow*input$subpltrow*input$subsubpltrow), length=dim(des$dlist)[1])
    # ) %>% .[,c("Block", "Row", "Col", "F1", "F2", "F3")]
    
    # for (i in 4:dim(designout$designdf)[2]) designout$designdf[,i] <- factor(designout$designdf[,i])
    # designout$designtab <- xtabs(~Block+F1+F2+F3, designout$designdf) 
    # simy <- rnorm(dim(designout$designdf)[1])		  
    # designout$designanova <- anova(lmer(simy ~ F1*F2*F3+(1|Block/F1/F2), data=designout$designdf))
    # designout$plotdf <- designout$designdf %>%
    # dplyr::mutate(x=Col,
    # y=max(Row)+1-Row,
    # block=Block,
    # colTrt=F1,
    # labelTrt=paste(F2, F3, sep="_"))
    # }
    # designout$designname <- NULL
    # }
    # }else{
    # designout$designinfo <- "Select or enter your treatment!"
    # designout$designdf <- NULL
    # }   
    # })
    # }
    
    if (!is.null(input$NTreatments)) {
      rowN <- ifelse(input$NRows %% input$N_replicate==0, input$NRows/input$N_replicate, input$NRows)
      colN <- ifelse(input$NColumns %% input$N_replicate==0, input$NColumns/input$N_replicate, input$NColumns)
      #		  if(input$seed==0) rseed <- NULL else rseed <- c(input$seed, input$seed+111)
      #		  if(input$replayout=='ver')  blkseq <- c(input$NRows, 1) else blkseq <- c(1, input$NColumns)
      if(input$replayout=='ver')  blkseq <- c(rowN, 1) else blkseq <- c(1, colN)
      # print(blkseq)
      withProgress(message = 'Running row and column design ...', value = 0.1, {
        if (is.null(input$NChecks) || input$NChecks=="0") {
          des <- try(corDiGGer(numberOfTreatments =input$NTreatments, rowsInDesign = input$NRows,
                               columnsInDesign = input$NColumns,
                               rowsInReplicate = ifelse(input$replayout=='ver', rowN, input$NRows), 
                               columnsInReplicate=ifelse(input$replayout=='ver', input$NColumns, colN),
                               blockSequence = list(blkseq), rngSeed=rseed,
                               spatial = TRUE, runSearch = TRUE
          ), TRUE)	
        }else{
          Nck <- as.numeric(unlist(strsplit(input$NChecks, ",")))
          des <- try(corDiGGer(numberOfTreatments =input$NTreatments, rowsInDesign = input$NRows,
                               columnsInDesign = input$NColumns,
                               rowsInReplicate = ifelse(input$replayout=='ver', rowN, input$NRows), 
                               columnsInReplicate=ifelse(input$replayout=='ver', input$NColumns, colN),
                               blockSequence = list(blkseq), 
                               # blockSequence = "default",
                               # blockSequence = list(c(1,1)),
                               rngSeed=rseed, treatRepPerRep=c(Nck, rep(1, input$NTreatments-length(Nck))),
                               spatial = FALSE, runSearch = TRUE), TRUE)			 
        }
      })
      
      if ("try-error" %in% class(des)) {
        designout$designinfo <- des[1]
        designout$designdf <- NULL
        designout$designtab <- NULL
        designout$designanova <- NULL
        designout$plotdf <- NULL
      }else{
        designout$designname <- paste("This is a row and column design for",  input$NTreatments, "lines with", input$N_replicate, "replicates. The design has", input$NRows, "rows and", input$NColumns, "columns,\n and", ifelse(input$replayout=='ver', rowN, input$NRows), "rows and", ifelse(input$replayout=='ver', input$NColumns, colN), "columns in each replicate.\n\n")
        designout$designdf <- des$dlist[, c("REP", "ROW", "RANGE", "TRT")]
        names(designout$designdf) <- c("Replicates", "RowD", "ColD", "Line")
        for (i in 1:dim(designout$designdf)[2]) designout$designdf[,i] <- factor(designout$designdf[,i])
        if(input$replayout=='ver')  {
          designout$designdf$Row <- droplevels(rep(designout$designdf$RowD[designout$designdf$Rep==1], length=dim(designout$designdf)[1]))
          designout$designdf$Column <- designout$designdf$ColD
        }else{
          designout$designdf$Row <- designout$designdf$RowD
          designout$designdf$Column <- droplevels(rep(designout$designdf$ColD[designout$designdf$Replicates==1], length=dim(designout$designdf)[1]))
        }
        
        if(input$replayout=='ver')  designout$designtab <- list("Line vs Col"=table(xtabs(~Column+Line, designout$designdf)), "Line vs Replicates"=table(xtabs(~Replicates+Line, designout$designdf)))
        else  designout$designtab <- list("Line vs Row"=table(xtabs(~Row+Line, designout$designdf)), "Line vs Rep"=table(xtabs(~Replicates+Line, designout$designdf)))
        
        if (nlevels(designout$designdf$Row)==1) designout$designanova <- summary(try(aov(rnorm(dim(designout$designdf)[1]) ~ Line + Error(Replicates/Column), data=designout$designdf), TRUE))
        else if (nlevels(designout$designdf$Column)==1) designout$designanova <- summary(try(aov(rnorm(dim(designout$designdf)[1]) ~ Line + Error(Replicates/Row), data=designout$designdf), TRUE))
        else designout$designanova <- summary(try(aov(rnorm(dim(designout$designdf)[1]) ~ Line + Error(Replicates/(Row*Column)), data=designout$designdf), TRUE))
        
        designout$plotdf <- data.frame(x=1:dim(designout$designdf)[1],y=1,block=1,colTrt=1,labelTrt=1)			  
        designout$plotdf$x <- as.numeric(designout$designdf$ColD)
        ploty <- as.numeric(designout$designdf$RowD)
        designout$plotdf$y <- max(ploty)+1-ploty
        designout$plotdf$block <- factor(1)
        designout$plotdf$colTrt <- paste("Rep", designout$designdf$Replicates)
        designout$plotdf$labelTrt <- as.character(designout$designdf$Line)
        if (!is.null(input$NChecks) && input$NChecks!="0") {
          for (i in 1:length(Nck)) designout$plotdf$colTrt[designout$plotdf$labelTrt ==as.character(i)] <- paste("Check",i)
        }				
        designout$designdf <- designout$designdf[, c("Replicates", "Row", "Column", "Line")]
        
        if (!is_empty(input$lineName)) {
          designout$designdf$Line <- as.factor(designout$designdf$Line)
          # LineLevels <- unique(unlist(strsplit(input$lineName, split="[^[:alnum:]]+")))
          LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$lineName), split="\\W+")))
          #LineLevels <- unique(unlist(strsplit(gsub("^\\s+", "", input$lineName), split="[[:space:]]+|,|\\n")))
          if (length(LineLevels)==nlevels(designout$designdf$Line)){
            levels(designout$designdf$Line) <- LineLevels
            designout$plotdf$labelTrt <- as.character(designout$designdf$Line)
          }else{
            updateTextAreaInput(session, inputId="lineName", label = NULL, value = paste("Please enter", input$NTreatments, "line names."))
          }
        }	
        if (!is.null(input$NChecks) && input$NChecks!="0") { 
          LineChr <- as.character(designout$designdf$Line)
          designout$designdf$Check <- ifelse(LineChr %in% levels(designout$designdf$Line)[1:length(Nck)], LineChr, "0")
          designout$designdf$Line <-ifelse(LineChr %in% levels(designout$designdf$Line)[1:length(Nck)], "0", LineChr)
        }        
        
      }
    }else{
      designout$designinfo <- "Setup your design!"
      designout$designdf <- NULL
      designout$designtab <- NULL
      designout$designanova <- NULL
      designout$plotdf <- NULL
    }
  }else if(input$designtype=='p-rep Design') {
    
    if (!is.null(input$pDtrtrep)) {
      trtNogrp <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(input$pDtrtno, ",")))))
      trtRepgrp <- na.omit(suppressWarnings(as.numeric(unlist(strsplit(input$pDtrtrep, ",")))))
      trtNopD <- sum(trtNogrp)
      blkseqpD <- lapply(unlist(strsplit(input$pDblkseq, ";")), function(x) na.omit(suppressWarnings(as.numeric(unlist(strsplit(x, ","))))))
      withProgress(message = 'Running p-repdesign ...', value = 0.1, {
        des <- try(prDiGGer(numberOfTreatments =trtNopD, rowsInDesign = input$pDrow,
                            columnsInDesign = input$pDcol,
                            treatRepPerRep = rep(trtRepgrp, trtNogrp),
                            treatGroup = rep(rep(1, length(trtNogrp)),trtNogrp),
                            blockSequence = blkseqpD,                            
                            rngSeed=rseed,
                            runSearch = TRUE), TRUE)
      })
      
      if ("try-error" %in% class(des)) {
        designout$designinfo <- des[1]
        designout$designdf <- NULL
        designout$designtab <- NULL
        designout$designanova <- NULL
        designout$plotdf <- NULL
      }else{
        designout$designname <- paste("This is a Partially replicated designs for",  trtNopD, "treatments. The design has",
                                      input$pDrow, "rows and", input$pDcol, "columns.\n\n")
        
        designout$designdf <- des$dlist[, c( "ROW", "RANGE", "TRT")]
        names(designout$designdf) <- c("Row", "Col", "Trt")
        designout$designtab <- NULL
        designout$designanova <- NULL
        
        designout$plotdf <- data.frame(x=1:dim(designout$designdf)[1],y=1,block=1,colTrt=1,labelTrt=1)			  
        designout$plotdf$x <- as.numeric(designout$designdf$Col)
        ploty <- as.numeric(designout$designdf$Row)
        designout$plotdf$y <- max(ploty)+1-ploty
        designout$plotdf$block <- factor(1)
        designout$plotdf$colTrt <- as.factor(as.numeric(cut(designout$designdf$Trt, breaks=c(0,cumsum(trtNogrp)+0.5))))
        #designout$plotdf$colTrt <- as.factor(ifelse(designout$designdf$Trt <= trtNogrp[1], 1, designout$designdf$Trt))
        designout$plotdf$labelTrt <- as.character(designout$designdf$Trt)
      }
    }else{
      designout$designinfo <- "Setup your design!"
      designout$designdf <- NULL
      designout$designtab <- NULL
      designout$designanova <- NULL
      designout$plotdf <- NULL
    }
  }  
  designresultnameUpdate <- ifelse(input$designtype=='Row and Column', paste(paste(gsub(" ", "_", input$expName),  gsub(" ", "_", input$locationPlace), sep="_"), gsub(" ", "_", input$breederName), sep="_"), gsub(" ", "_", input$designtype))
  updateTextInput(session, "designresultname",  "Design name:", 
                  value = paste(designresultnameUpdate, input$seed, sep="_"))
})  

observeEvent(input$designtype, {	 
  # Clear the previous output
  designout$designinfo <- "Setup your design!"
  designout$designdf <- NULL
  designout$designtab <- NULL
  designout$designanova <- NULL
  designout$plotdf <- NULL
})

# save design result
output$savedesignresult <- downloadHandler(  
  filename <- function(){
    design_name <- gsub("-", "_", input$designresultname)
    paste(design_name, sep = '.', input$designresultformat)
  },
  
  content = function(file) {
    design_name <- gsub("-", "_", input$designresultname)
    assign(design_name, designout$designdf)
    if (input$designresultformat=="RData") eval(parse(text=paste("save(", design_name, ", file = file)")))
    if (input$designresultformat=="csv") eval(parse(text=paste("write.csv(", design_name, ", file = file, row.names=FALSE)")))
  })# end of save design result

output$savedesignLayout <- downloadHandler(
  filename <- function(){
    paste(input$designresultname, "_layout.csv", sep="")
  },
  
  content = function(file) {
    if (input$designtype=='Row and Column') {
	  if ("Check" %in% colnames(designout$designdf)) {
	    designdfLayout <- designout$designdf %>% 
		  dplyr::mutate(LineN=as.numeric(Line)+as.numeric(Check)) %>%
		  dplyr::select(-Check, -Line) %>%
		  pivot_wider(names_from = Column, values_from = LineN, names_prefix ="Col_")		
	  }else designdfLayout <- pivot_wider(designout$designdf, names_from = Column, values_from = Line, names_prefix ="Col_")
	}else if (input$designtype=='p-rep Design') {
	  designdfLayout <- pivot_wider(designout$designdf, names_from = Col, values_from = Trt, names_prefix ="Col_")
	}else designdfLayout <- designout$designdf
    write.csv(designdfLayout, file, row.names=FALSE)
  })# end of save design result

###### Sever output
# show design info 
output$designoutinfo <- renderPrint({
  if (is.null(designout$designdf)) designout$designinfo 
  else{
    cat(designout$designname)
    str(designout$designdf)
  }
})  

# show design result 
output$designoutdf <- renderDataTable({
  designout$designdf
})

# show tabulate
output$designchktab <- renderPrint({
  validate(need(!is.null(designout$designtab), label="A design"))
  if (!input$hidetab) designout$designtab
})

# show anova
output$designchkanova <- renderPrint({
  validate(need(!is.null(designout$designanova), label="A design"))
  aovtable <- designout$designanova
  if (class(aovtable)[1]=="anova") aovtable <- aovtable[,1, drop=F]
  else{
    if (length(aovtable)==1)  aovtable <- aovtable[[1]]
    else{
      for (i in 1:length(aovtable)) {
        aovtable[[i]][[1]]$"Sum Sq" <- NULL
        aovtable[[i]][[1]]$"Mean Sq" <- NULL
        aovtable[[i]][[1]]$"F value" <- NULL
        aovtable[[i]][[1]]$"Pr(>F)" <- NULL
      }
    }
  }
  designout$designanovan <- aovtable
  if (!input$hideanova) {
    cat("A skeleton ANOVA of your design\n")
    print(aovtable)  
  }    
})

# show layout plot

output$layoutplot <- renderPlot({
  theme_layout <- function(){
    theme(line = element_blank(),
          panel.background = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_rect(fill = "wheat"))
  } 
  
  validate(need(!is.null(designout$plotdf), message="There is no layout plot for this design!"))
  
  pltout <- ggplot(data = designout$plotdf) +
    geom_tile(aes(x = x, y = y, fill = colTrt), colour = "white") +
    scale_fill_discrete(h = c(0, 240), c = 85, l = 80) +
    geom_text(aes(x = x, y = y, label = labelTrt)) 
  
  if (nlevels(designout$plotdf$block) > 1)   pltout <- pltout + facet_wrap(~ block, scales = "free_x")
  
  if (!is.null(designout$plotdf$colTrt1)) pltout <- pltout + geom_text(aes(x = x, y = y, label = labelTrt, colour = colTrt1)) +
    scale_colour_continuous(low = "yellow4", high = "black")
  designout$layoutplot <- pltout + theme_layout()
  print(designout$layoutplot)
})

# outputOptions(output, "layoutplot", suspendWhenHidden = FALSE)
# outputOptions(output, "designchkanova", suspendWhenHidden = FALSE)

######################### Down load report ######################### 

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('reportDesign.Rmd') 
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'reportDesign.Rmd')
    
    library(rmarkdown)
    out <- render('reportDesign.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
) # end of downloadHandler   
