
######################### Model panel ######################### 

rslt <- reactiveValues()

rv <- reactiveValues(
  deletedRows = NULL,
  deletedRowIndices = list()
)

###### sever interface

# output$subset_ui <- renderUI({
# if (input$cksubset && !is_empty(input$selecteddata)){
# wellPanel(
# selectizeInput('varselect', NULL, choices = names(r_data[[input$selecteddata]]),
# selected = names(r_data[[input$selecteddata]]), multiple = TRUE, options = list(plugins = list('remove_button','drag_drop')))				 
# )}
# }) # end of output$subset_ui

output$model_ui <- renderUI({
  if (!is_empty(input$selecteddata)) {
    # if (input$cksubset && !is_empty(input$varselect)){
    # rslt$modeldata <- r_data[[input$selecteddata]][, input$varselect, drop=FALSE]		
    # }else rslt$modeldata <- r_data[[input$selecteddata]] 
    
    rslt$modeldata <- r_data[[input$selecteddata]] 
    dfnames <- names(rslt$modeldata)
    setupterms <- c("Year", "Season", "Location", "Replicates", "Row", "Column", "Sample", "Check", "Line")
    realterms <- intersect(setupterms, dfnames)
    if (length(realterms) > 0 && "Line" %in% realterms) {      
      fterms <- try(attr(terms(formula(paste("~(", paste(realterms, collapse="+"), ")^3", sep=""))), "term.labels"), TRUE)
      if (!is(fterms, 'try-error')) {         
        if ("Check" %in% realterms) fterms <- fterms[!grepl("Check:Line", fterms)]
        rslt$fterms <- fterms
      }else rslt$fterms <- NULL
      
      if (any(c("Year", "Season", "Location", "Replicates", "Sample") %in% realterms)){
        lterms <- try(attr(terms(formula(paste("~Line/(", paste(realterms[!(realterms %in% c("Row", "Column",  "Line", "Check"))], collapse="+"), ")^2", sep=""))),"term.labels"), TRUE)
        if (is(lterms, 'try-error')) lterms <- NULL else lterms <- intersect(lterms, c("Line", "Line:Year", "Line:Season", "Line:Location", "Line:Replicates", "Line:Year:Season", "Line:Year:Location", "Line:Season:Location", "Line:Sample"))
      }else lterms <- "Line"
      
      if ("Sample" %in% realterms){
        rt2 <- try(attr(terms(formula(paste("~", paste(c(realterms[!( realterms %in% c("Row", "Column", "Sample", "Line", "Check"))], "Sample"), collapse="/"), sep=""))), "term.labels"), TRUE)
        if (is(rt2, 'try-error')) rt2 <- NULL
      }else rt2 <- NULL
      if ("Row" %in% realterms && "Column" %in% realterms) {
        rt0 <- try(attr(terms(formula(paste("~", paste(realterms[!(realterms %in% c("Column", "Sample", "Line", "Check"))], collapse="/"), sep=""))), "term.labels"), TRUE)
        if (is(rt0, 'try-error')) rt0 <- NULL       
        rt1 <- try(attr(terms(formula(paste("~", paste(realterms[!(realterms %in% c("Row", "Sample", "Line", "Check"))], collapse="/"), sep=""))), "term.labels"), TRUE)
        if (is(rt1, 'try-error')) rt1 <- NULL        
        rslt$rterms <- unique(c(rt0, rt1, rt2, lterms))       
      }else{
        rt0 <- try(attr(terms(formula(paste("~", paste(realterms[!(realterms %in% c("Sample", "Line", "Check"))], collapse="/"), sep=""))), "term.labels"), TRUE)
        if (is(rt0, 'try-error')) rt0 <- NULL
        rslt$rterms <- unique(c(rt0, rt2, lterms))       
      }	  
    }else rslt$fterms <- rslt$rterms <- NULL
  }else rslt$modeldata <- NULL
  
  setuptermsManova <- c("Year", "Season", "Location", "Replicates", "Sample", "Line")
  realtermsManova <- intersect(setuptermsManova, names(rslt$modeldata))
  if (length(realtermsManova) > 0) manovaTerms <- attr(terms(formula(paste("~(", paste(realtermsManova, collapse="+"), ")^2", sep=""))), "term.labels") else manovaTerms <- NULL
  wellPanel(
    checkboxInput("hideDataInfo2", "Hide data info?", value = FALSE),    
    selectizeInput('modelresponse1', 'Primary Trait', c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])), options = list(create = TRUE)),
    radioButtons('transtype', "Data transformation", c('none'='I(', 'log_e'='log(', 'sqrt'='sqrt(', 'arcsin'='asin('), inline=TRUE),
    selectizeInput('modelfixterm', 'Fixed terms:', c("NULL"=1, rslt$fterms),  multiple = TRUE,
                   options = list(create = TRUE, plugins = list('remove_button', 'drag_drop'))),
    selectizeInput('modelrandterm', 'Random terms:', c("NULL", rslt$rterms),  multiple = TRUE,
                   options = list(create = TRUE, plugins = list('remove_button', 'drag_drop'))), 
    checkboxInput("modcoefficients", "Hide model coefficients?", value = TRUE),
    checkboxInput("CaHeritability", "Estimate Line Mean Heritability?", value = FALSE), 
    uiOutput("Blupe_ui"),
    checkboxInput("ckSecTrait", "Analysis of secondary trait?", value = FALSE),
    conditionalPanel("input.ckSecTrait",
                     selectizeInput('modelresponse2', 'Secondary trait', c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])),	  
                                    options = list(create = TRUE)),
                     selectizeInput('multimodelterms2', 'MANOVA terms:', manovaTerms,  multiple = TRUE,
                                    options = list(create = TRUE, plugins = list('remove_button', 'drag_drop')))),
    br(),	
    actionButton("modelrun", "Run", class = "btn-success")
  ) 
})# end of output$model_ui

output$Blupe_ui <- renderUI({
  blupinputterm <- input$modelrandterm[sapply(input$modelrandterm, function(x) {splitx <- unlist(strsplit(x, ":")); "Line" %in% splitx})]  
  wellPanel(
    selectInput('blup', 'BLUP for:', unique(c("NULL", blupinputterm))),
    checkboxInput("ckPatternAnalysis", "Pattern analysis", value = FALSE),
    selectInput('blue', 'BLUE for:', c("NULL", input$modelfixterm[input$modelfixterm!="1"]))
  )	
}) # end of output$Blupe_ui  lmmsim_ui

output$lmmsim_ui <- renderUI({  # bredingtypeN
  if (input$abcd == 'R/delta_G' && !is.null(rslt$Heritability)){
    if(rslt$nY!=1) ychoise <- unique(c(rslt$nY, 1:10)) else ychoise <- 1 
    if(rslt$nS!=1) schoise <- unique(c(rslt$nS, 1:12)) else schoise <- 1 
    if(rslt$nLoc!=1) lchoise <- unique(c(rslt$nLoc, 1:50)) else lchoise <- 1 
    if(rslt$nR!=1) rchoise <- unique(c(rslt$nR, 1:10)) else rchoise <- 1 
    if(rslt$nSam!=1) nschoise <- unique(c(rslt$nSam, 1:1000)) else nschoise <- 1 
    if (input$ckSecTrait && !(input$modelresponse2 %in% c("NULL", input$modelresponse1))) {
      strategyV <- c("AWFX-HS", "Correlated response (HS)",  "Correlated response (FS)")
    }else{
      if (input$bredingtype=='hsf') strategyV <- c("HS", "HSPT", "AWF-HS", "CRyGS-HS", "ApWFgs-HS", "AgsWFgs-HS")
      else if (input$bredingtype=='fsf') strategyV <- c("FSF", "FSPT", "AWF-FS (both parents)", "AWF-FS (single parent)", "CRyGS-FS", "ApWFgs-FS", "AgsWFgs-FS") 
      else strategyV <- ifelse(!input$selfP_early, "R (Inbred lines)", "R (Early stage)")
    }       
    if (input$bredingtype=='selfP') p1_label <- "Selection pressure (line):" else p1_label <- "Selection pressure (family):"
    if (input$bredingtype=='selfP' && input$selfP_early) {
	  numLines_label <- "Number of Lines/Plots (p-rep):"
	  RepCos_value <- 0
	  min_max_value <- 0
	}else{
	  numLines_label <- "Number of Lines:"
	  RepCos_value <- 500
	  min_max_value <- NA
	}
	  
    wellPanel(
      tabsetPanel(
        tabPanel("variables:",
                 numericInput("IndustryStd", label = "Industry standard:", 100),
                 selectInput("Strategy", label = "Strategy:", choices=strategyV),	  
                 conditionalPanel("input.Strategy=='CRyGS-HS' | input.Strategy=='CRyGS-FS' | input.Strategy=='ApWFgs-HS' | input.Strategy=='ApWFgs-FS' | input.Strategy=='AgsWFgs-HS' | input.Strategy=='AgsWFgs-FS'", 
                                  div(class="row",		   
                                      div(class="col-xs-6",
                                          numericInput("GSaccuracy", "GS accuracy", value=0.8, min = 0, max = 1, step = 0.1, width = NA)),
                                      div(class="col-xs-6",
                                          numericInput("GSh_x2", "GS heritability", value=1, min = 0, max = 1, step = 0.1, width = NA)))         
                 ),
                 conditionalPanel("input.Strategy=='R (Inbred lines)' | input.Strategy=='R (Early stage)' | input.Strategy=='HS' | input.Strategy=='HSPT' | input.Strategy=='Correlated response (HS)' | input.Strategy=='CRyGS-HS' | input.Strategy=='Correlated response (FS)' | input.Strategy=='CRyGS-FS' | input.Strategy=='FSF' | input.Strategy=='FSPT'", 
                                  selectInput("p1", label = p1_label, 
                                              choices=as.character(c(0.2, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.15, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005, 0.001)))),
                 conditionalPanel("input.Strategy=='AWF-HS' | input.Strategy=='AWFX-HS' | input.Strategy=='AWF-HS' | input.Strategy=='AWF-FS (both parents)' | input.Strategy=='AWF-FS (single parent)' | input.Strategy=='ApWFgs-HS' | input.Strategy=='ApWFgs-FS' | input.Strategy=='AgsWFgs-HS' | input.Strategy=='AgsWFgs-FS'",
                                  div(class="row",		   
                                      div(class="col-xs-6",
                                          selectInput("p1n", label = "Selection pressure (family):", choices=as.character(c(0.2, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.15, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005, 0.001)))),
                                      div(class="col-xs-6",
                                          selectInput("p2", label = "Selection pressure (within family):", choices=as.character(c(0.2, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.15, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005, 0.001)))))),
                 
                 conditionalPanel("input.Strategy!='R (Early stage)'",
                                  div(class="row",
                                      div(class="col-xs-6",
                                          selectInput("Yearn", label = "Year (n):", choices=ychoise)),
                                      div(class="col-xs-6",
                                          selectInput("Sean", label = "Season (n):", choices=schoise)),		  
                                      div(class="col-xs-6",
                                          selectInput("Locn", label = "Location (n):", choices=lchoise)),
                                      div(class="col-xs-6",
                                          selectInput("Repn", label = "Rep (n):", choices=rchoise)),
                                      div(class="col-xs-6",
                                          selectInput("Samplen", label = "Sample (n):", choices=nschoise))			  
                                  )
                 )
        ),
        tabPanel("Cost",
                 conditionalPanel("input.Strategy!='CRyGS-HS' & input.Strategy!='CRyGS-FS' & input.Strategy!='AgsWFgs-HS' & input.Strategy!='AgsWFgs-FS'",
                                  h4("Field Trial Cost:"),
                                  numericInput("numLines", label = numLines_label,
                                               rslt$nLine),
                                  div(class="row",	  
                                      div(class="col-xs-6",
                                          numericInput("SampleCos", label = "Sample ($):", 10)),
                                      div(class="col-xs-6",
                                          numericInput("RepCos", label = "Rep ($):", RepCos_value, min = min_max_value, max = min_max_value)),
                                      div(class="col-xs-6",
                                          numericInput("LocCos", label = "Location per Year ($):", 25000)),
                                      div(class="col-xs-6",
                                          numericInput("OtherCos", label = "General costs ($):", 10000))			  
                                  )), 
                 conditionalPanel("input.Strategy=='AWFX-HS' | input.Strategy=='Correlated response (HS)' | input.Strategy=='Correlated response (FS)'",
                                  h4("Second Trait Cost:"),
                                  div(class="row",	  
                                      div(class="col-xs-6",
                                          numericInput("UnitCos", label = "Cost per Unit ($):", 5)),
                                      div(class="col-xs-6",
                                          numericInput("Nofamily", label = "No. Selected Families:", 10)),
                                      div(class="col-xs-6",
                                          numericInput("Nosample", label = "No. Samples perFamily per Replicate:", 1)),
                                      div(class="col-xs-6",
                                          numericInput("Norep", label = "No. Replicates:", 1))),
                                  numericInput("Nomeasure", label = "No. Measurements per Year:", 1),	 
                                  div(class="row",	
                                      div(class="col-xs-6",
                                          numericInput("Noyear", label = "No. Years:", 1)),
                                      div(class="col-xs-6",
                                          numericInput("Nolocation", label = "No. Locations:", 1))							 
                                  )),						 
                 conditionalPanel("input.Strategy=='CRyGS-HS' | input.Strategy=='CRyGS-FS' | input.Strategy=='ApWFgs-HS' | input.Strategy=='ApWFgs-FS' | input.Strategy=='AgsWFgs-HS' | input.Strategy=='AgsWFgs-FS'",
                                  h4("Genomic Selection Cost:"),
                                  h5("(Refer to example in Quick start guide)"),
                                  numericInput("numParents", label = "No. of Parents:", rslt$nLine),
                                  div(class="row",	  
                                      div(class="col-xs-6",
                                          numericInput("gfamilyno", label = "No. of Families:", 100)),
                                      div(class="col-xs-6",
                                          numericInput("gsampleno", label = "No. of Samples/Family:", 25)),
                                      div(class="col-xs-6",
                                          numericInput("gSampleCos", label = "Sample ($):", 50)),	
                                      div(class="col-xs-6",
                                          numericInput("gOtherCos", label = "General costs ($):", 2500))					
                                  )),
                 conditionalPanel("input.Strategy=='R (Inbred lines)' | input.Strategy=='R (Early stage)'",
                                  selectInput("CycleL", label = "Cycle length (year):", choices=1:15)
                 )
        )
      ),
      br(),	  
      actionButton("lmmsimrun", "Update", class = "btn-success"),
      actionButton("simuhelpBut", "Help", class = "btn-primary")
    )
  }
}) # end of output$lmmsim_ui

###### Sever programe

observeEvent(input$simuhelpBut, {
  toggleModal(session, "simulartionhelp", "open")
})

observeEvent(input$modelrun, {  
  rslt$lmsimdfN <- lmsimdfNN  
  
  withProgress(message = 'Fitting a LMM model ...', value = 0.1, {
    lmemod <- try(lmer(formula(paste(input$transtype, input$modelresponse1, paste(")~", paste(input$modelfixterm, collapse="+"), sep=""), '+ (', paste(paste0("1|", input$modelrandterm), collapse=") + ("), ')')), REML=TRUE, data=rslt$modeldata), TRUE)	
    
    LRT.p_value <- rep(-1, length(input$modelrandterm))
    names(LRT.p_value) <- input$modelrandterm
    for (i in 1:length(input$modelrandterm)) { 
      lmemod_1 <- try(lmer(formula(paste(input$transtype, input$modelresponse1, paste(")~", paste(input$modelfixterm, collapse="+"), sep=""), '+ (', paste(paste0("1|", input$modelrandterm[-i]), collapse=") + ("), ')')), REML=TRUE, data=rslt$modeldata), TRUE)	
      if (class(lmemod)[1]=="lmerMod" && class(lmemod_1)[1]=="lmerMod") LRT.p_value[i] <- anova(lmemod, lmemod_1, refit = FALSE)$'Pr(>Chisq)'[2]
    }
  })
  
  if (class(lmemod)[1]=="lmerMod") {
    rslt$mod <- lmemod
    rslt$summary <- summary(lmemod)
    rslt$rf <- ranef(lmemod, condVar = TRUE)
    if (!all(input$modelfixterm=="1")) {
      rslt$anova <- anova(lmemod)     #Anova(lmemod, type=2,  test.statistic="F")	     
    }else rslt$anova <- NULL 
    
    withProgress(message = 'Calculating variance components ...', value = 0.1, {
      lmemod.ML <- refitML(lmemod)
      varcomp <- data.frame(VarCorr(lmemod))[, c("grp", "vcov")]
      names(varcomp) <- c("Random.term", "Variance")
      Rtms <- varcomp$Random.term
      
      Hcoefn <- c("Line", "Line:Year", "Line:Season", "Line:Location", "Line:Replicates", "Line:Year:Season", "Line:Year:Location", "Line:Season:Location", "Residual")
      
      Vtms <- intersect(Hcoefn, Rtms)
      lmemod.MLvcov <- waldVar2(lmemod.ML)
      dimnames(lmemod.MLvcov) <- list(Rtms,Rtms)
      Hvcov <- lmemod.MLvcov[Vtms, Vtms]
      varcomp$Std.error <- 2*sqrt(varcomp$Variance)*sqrt(diag(lmemod.MLvcov))
      pars <- as.data.frame(VarCorr(lmemod),order="lower.tri")[,"sdcor"]
      names(pars) <- Rtms
      vhack2 <- list(coefficients=pars, vcov=lmemod.MLvcov)
      wci <- confintlmer(vhack2)
      
      wci[wci<0] <- 0
      varcomp <- cbind(varcomp, wci^2)
      
      LRT.p_value <- LRT.p_value[Rtms]
      varcomp <- cbind(varcomp, LRT.p_value)
      
      row.names(varcomp) <- Rtms
      rslt$varcomp <- round(varcomp[,-1],4)
      
      if ("Line" %in% Rtms) {
        Linedf <- rslt$modeldata %>%
          dplyr::filter(!is.na(rslt$modeldata[, input$modelresponse1])) %>% 
          dplyr::mutate(fixedfit=as.numeric(model.matrix(lmemod)%*%fixef(lmemod))) %>%
          group_by(Line) %>%
          summarise(mFix=mean(fixedfit))
        rslt$LineMean <- mean(Linedf$mFix+ranef(lmemod)[["Line"]][,1], na.rm=TRUE)
        
        varcompV <- varcomp$Variance
        names(varcompV) <- Rtms
        mdf <- sapply(model.frame(lmemod)[, intersect(c("Year", "Season", "Location", "Replicates", "Sample", "Line"), names(model.frame(lmemod))), drop=FALSE], nlevels)  	
        
        nY <- ifelse("Year" %in% names(mdf), mdf["Year"], 1)        
        nS <- ifelse("Season" %in% names(mdf), mdf["Season"], 1)        
        nLoc <- ifelse("Location" %in% names(mdf), mdf["Location"], 1)        
        nR <- ifelse("Replicates" %in% names(mdf), mdf["Replicates"], 1)        
        nSam <- ifelse("Sample" %in% names(mdf), mdf["Sample"], 1)        
        
        VLY <- ifelse("Line:Year" %in% Rtms, varcompV["Line:Year"], 0)        
        VLS <- ifelse("Line:Season" %in% Rtms, varcompV["Line:Season"], 0)        
        VLLoc <- ifelse("Line:Location" %in% Rtms, varcompV["Line:Location"], 0)        
        VLRep <- ifelse("Line:Replicates" %in% Rtms, varcompV["Line:Replicates"], 0)        
        VLYS <- ifelse("Line:Year:Season" %in% Rtms, varcompV["Line:Year:Season"], 0)        
        VLYL <- ifelse("Line:Year:Location" %in% Rtms, varcompV["Line:Year:Location"], 0)        
        VLSL <- ifelse("Line:Season:Location" %in% Rtms, varcompV["Line:Season:Location"], 0)
        
        Heritability  <- varcompV["Line"]/(varcompV["Line"]+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc)+varcompV["Residual"]/(nY*nS*nLoc*nR*nSam))	
        if (input$bredingtype=="selfP" && !input$ckSecTrait) Heritability  <- (1+Coefficient_inbreeding_F[input$selfP_F])*varcompV["Line"]/((1+Coefficient_inbreeding_F[input$selfP_F])*(varcompV["Line"]+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc))+varcompV["Residual"]/(nY*nS*nLoc*nR*nSam))	
        if (input$bredingtype=="selfP" && input$selfP_early && !input$ckSecTrait) Heritability  <- 1 - mean((se.ranef(lmemod)[["Line"]])^2)/varcompV["Line"]
        
        rslt$Heritability <- Heritability
        Hcoef <- c(1, nY, nS, nLoc, nR, nY*nS, nY*nLoc, nS*nLoc, (nY*nS*nLoc*nR*nSam))
        if (input$bredingtype=="selfP" && !input$ckSecTrait) Hcoef <- c((1+Coefficient_inbreeding_F[input$selfP_F])/c(1, nY, nS, nLoc, nR, nY*nS, nY*nLoc, nS*nLoc), (nY*nS*nLoc*nR*nSam)) 
        names(Hcoef) <- Hcoefn
        Hcoef <- Hcoef[Vtms]	   
        Hfo <- as.formula(paste("~x1^2/(", paste(do.call("paste", c(data.frame(paste(paste("x", 1:length(Hcoef), sep=""), "^2", sep=""), Hcoef),sep = "/")), collapse="+"), ")", sep=""))
        if (input$bredingtype=="selfP" && !input$ckSecTrait) Hfo <- as.formula(paste("~", paste(1+Coefficient_inbreeding_F[input$selfP_F], "*x1^2/(", sep=""), paste(do.call("paste", c(data.frame(paste(paste("x", 1:length(Hcoef), sep=""), "^2", sep=""), Hcoef),sep = "/")), collapse="+"), ")", sep=""))
        seH <- deltamethod(Hfo, pars[Vtms], Hvcov)   
        names(seH) <- names(Heritability) <- NULL
        attr(Heritability,"Std.error") <- seH
        rslt$HeritabilityMod <- Heritability	
        
        # sigmaPF <- sqrt(varcompV["Line"]+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc)+varcompV["Residual"]/(nY*nS*nLoc*nR*nSam))
        # if (input$bredingtype=="selfP" && !input$ckSecTrait) sigmaPF <- sqrt((1+Coefficient_inbreeding_F[input$selfP_F])*(varcompV["Line"]+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc))+varcompV["Residual"]/(nY*nS*nLoc*nR*nSam))  
        # sigmaPMSa <- sqrt(varcompV["Line"]+VLY+VLS+VLLoc+VLRep+VLYS+VLYL+VLSL+varcompV["Residual"]/nSam)		  
        # GC_1 <- kV["0.2"]*0.5*varcompV["Line"]
        # if (input$bredingtype=="selfP" && !input$ckSecTrait)  GC_1 <- kV["0.2"]*(1+Coefficient_inbreeding_F[input$selfP_F])*varcompV["Line"]
        
        if("Line:Sample" %in% Rtms) WFPVarV <- varcompV["Line:Sample"] + varcompV["Residual"] else WFPVarV <- NA
        # costV <- nSam*mdf["Line"]*nR*nS*nY*nLoc*10 + nR*nY*nLoc*5000 + nY*nLoc*25000 + 10000
        # costG <- 100*25*50+2500
        
        if (input$ckSecTrait && !(input$modelresponse2 %in% c("NULL", input$modelresponse1))){
          lmemod2 <- try(lmer(formula(paste(input$modelresponse2, paste("~", paste(input$modelfixterm, collapse="+"), sep=""), '+ (',paste(paste0("1|", input$modelrandterm), collapse=") + ("), ')')), REML=TRUE, data=rslt$modeldata), TRUE)
          varcompV2 <- data.frame(VarCorr(lmemod2))[, "vcov"]
          names(varcompV2) <- Rtms  		 
          if("Line:Year" %in% Rtms)  VLY2 <- varcompV2["Line:Year"] else  VLY2 <- 0	
          if("Line:Season" %in% Rtms) VLS2 <- varcompV2["Line:Season"] else VLS2 <- 0
          if("Line:Location" %in% Rtms) VLLoc2 <- varcompV2["Line:Location"] else VLLoc2 <- 0
          if("Line:Replicates" %in% Rtms)  VLRep2 <- varcompV2["Line:Replicates"] else  VLRep2 <- 0	
          if("Line:Year:Season" %in% Rtms) VLYS2 <- varcompV2["Line:Year:Season"] else VLYS2 <- 0
          if("Line:Year:Location" %in% Rtms) VLYL2 <- varcompV2["Line:Year:Location"] else VLYL2 <- 0
          if("Line:Season:Location" %in% Rtms) VLSL2 <- varcompV2["Line:Season:Location"] else VLSL2 <- 0          
          Heritability2  <- varcompV2["Line"]/(varcompV2["Line"]+VLY2/nY+VLS2/nS+VLLoc2/nLoc+VLRep2/nR+VLYS2/(nY*nS)+VLYL2/(nY*nLoc)+VLSL2/(nS*nLoc)+varcompV2["Residual"]/(nY*nS*nLoc*nR*nSam))
          names(Heritability2) <- NULL
          rslt$Heritability2 <- Heritability2
          
          manovaform <- paste("cbind(", paste(paste(input$transtype, input$modelresponse1, ")", sep=""), input$modelresponse2, sep=","), ") ~ ", paste(input$multimodelterms2, collapse="+"), sep="")
          manovamod2 <- try(lm(formula(manovaform), data=rslt$modeldata), TRUE)
          if (!is(try(Manova(manovamod2), TRUE), 'try-error') && "Line" %in% input$multimodelterms2) {
            mdfSI <- sapply(model.frame(manovamod2)[, intersect(c("Year", "Season", "Location", "Replicates"), names(model.frame(manovamod2))), drop=FALSE], nlevels)
            tmsSI <- attr(terms(manovamod2),"term.labels")
            nYSI <- ifelse("Year" %in% tmsSI, mdfSI["Year"], 1)
            nSSI <- ifelse("Season" %in% tmsSI, mdfSI["Season"], 1)
            nLocSI <- ifelse("Location" %in% tmsSI, mdfSI["Location"], 1)
            nRSI <- ifelse("Replicates" %in% tmsSI, mdfSI["Replicates"], 1)
            
            vcovout <- geneticCov(manovamod2)$`Mean sum of cross products`
            vcovLine <- vcovout[unlist(lapply(strsplit(names(vcovout), ":"), function(x) "Line" %in% x))]
            vcovLine_E <- lapply(vcovLine, function(x) x - geneticCov(manovamod2)$`Error Covariance`)
            vcovLineG <- vcovLine_E[[1]]
            if (length(vcovLine_E) > 1) for (i in 2:length(vcovLine_E)) vcovLineG <- vcovLineG - vcovLine_E[[i]]
            
            Linevocv <- vcovLineG/nYSI/nSSI/nLocSI/nRSI
            if (any(eigen(Linevocv)$values <= 0)) Linevocv <- nearPD(Linevocv)$mat
            rslt$r_g <- cov2cor(Linevocv)[1,2]
          }else rslt$r_g <- NA
          
          rslt$VLY2 <- VLY2
          rslt$VLS2 <- VLS2
          rslt$VLLoc2 <- VLLoc2 # rVLLocc
          rslt$GVar2 <- varcompV2["Line"]
          rslt$residvar2 <- varcompV2["Residual"]
        } # end of if !(input$modelresponse2 %in% c("NULL", input$modelresponse1))
        
        rslt$VLY <- VLY
        rslt$VLS <- VLS
        rslt$VLLoc <- VLLoc # rVLLocc
        rslt$GVar <- varcompV["Line"]
        
        rslt$VLRep <- VLRep
        rslt$VLYS <- VLYS
        rslt$VLYL <- VLYL
        rslt$VLSL <- VLSL
        rslt$residvar <- varcompV["Residual"]
        rslt$WFPVarV <- WFPVarV
        rslt$nY <- nY
        rslt$nS <- nS
        rslt$nLoc <- nLoc	
        
        rslt$nR <- nR
        rslt$nSam <- nSam
        rslt$nLine <- mdf["Line"]
        rslt$lmsimdfN <- lmsimdfNN
        rslt$reRun <- TRUE
      }else{ 
        rslt$Heritability <- rslt$Heritability2 <- NULL
        rslt$reRun <- FALSE
      }
    })
  }else{
    rslt$mod <- rslt$summary <- rslt$anova <- rslt$varcomp <- rslt$Heritability <- rslt$Heritability2 <- NULL
    rslt$reRun <- FALSE
  }  
}) # end of observe model rownames


observeEvent(input$runinfoSim, {
  
  updateCheckboxInput(session, "ckSecTrait", value = FALSE)
  
  nY <- as.numeric(input$YearninfoSim)  
  nS <- as.numeric(input$SeaninfoSim)  
  nLoc <- as.numeric(input$LocninfoSim)  
  nR <- as.numeric(input$RepninfoSim)  
  if(input$bredingtype=="selfP" && input$selfP_early) nSam <- input$SampleninfoSimSPE else nSam <- input$SampleninfoSim
  VLY <- input$VLYinfoSim  
  VLS <- input$VLSinfoSim  
  VLLoc <- input$VLLocinfoSim  
  VLRep <- input$VLRepinfoSim  
  VLYS <- input$VLYSinfoSim  
  VLYL <- input$VLYLinfoSim  
  VLSL <- input$VLSLinfoSim
  
  if (input$bredingtype=="selfP") {
    if (input$selfP_early)  Heritability  <- 1 - input$LineMvarinfoSimSPE/input$VLinfoSimSPE 
      else Heritability  <- (1+Coefficient_inbreeding_F[input$selfP_F])*input$VLinfoSim/((1+Coefficient_inbreeding_F[input$selfP_F])*(input$VLinfoSim+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc))+input$VEinfoSim/(nY*nS*nLoc*nR*nSam))
  }else  Heritability  <- input$VLinfoSim/(input$VLinfoSim+VLY/nY+VLS/nS+VLLoc/nLoc+VLRep/nR+VLYS/(nY*nS)+VLYL/(nY*nLoc)+VLSL/(nS*nLoc)+input$VEinfoSim/(nY*nS*nLoc*nR*nSam))	

  rslt$Heritability <- Heritability	
  rslt$HeritabilitySim <- Heritability
  
  if(input$VLSampleinfoSim > 0) WFPVarV <- input$VLSampleinfoSim + input$VEinfoSim else WFPVarV <- NA
  
  rslt$VLY <- VLY
  rslt$VLS <- VLS
  rslt$VLLoc <- VLLoc 
  
  if (input$bredingtype=="selfP" && input$selfP_early) rslt$GVar <- input$VLinfoSimSPE else rslt$GVar <- input$VLinfoSim
  
  rslt$VLRep <- VLRep
  rslt$VLYS <- VLYS
  rslt$VLYL <- VLYL
  rslt$VLSL <- VLSL
  rslt$residvar <- input$VEinfoSim
  rslt$WFPVarV <- WFPVarV
  rslt$nY <- nY
  rslt$nS <- nS
  rslt$nLoc <- nLoc	
  
  rslt$nR <- nR
  rslt$nSam <- nSam
  if (input$bredingtype=="selfP" && input$selfP_early) rslt$nLine <- input$LineninfoSimSPE else rslt$nLine <- input$LineninfoSim
  if (input$bredingtype=="selfP" && input$selfP_early) rslt$LineMean <- input$LineMinfoSimSPE else rslt$LineMean <- input$LineMinfoSim
  rslt$lmsimdfN <- lmsimdfNN 
  rslt$reRun <- TRUE
})
############################ 

observeEvent(input$lmmsimrun, {	
  #LbcnV <- as.numeric(input$Lbcn)
  YnV <- as.numeric(input$Yearn)
  SnV <- as.numeric(input$Sean)
  LnV <- as.numeric(input$Locn)
  RnV <- as.numeric(input$Repn)
  LYSnV <- YnV*SnV
  LYLnV <- YnV*LnV
  LSLnV <- SnV*LnV  
  
  SamnV <- as.numeric(input$Samplen)
  GC_1 <- kV[input$p1]*0.5*rslt$GVar
  GC_1n <- kV[input$p1n]*0.5*rslt$GVar
  if (input$bredingtype=="selfP" && !input$ckSecTrait) {
    GC_1 <- kV[input$p1]*(1+Coefficient_inbreeding_F[input$selfP_F])*rslt$GVar
  }
  
  if (input$bredingtype=="selfP" && !input$ckSecTrait && input$selfP_early) {
    GC_1 <- kV[input$p1]*sqrt(rslt$GVar)
  }
  
  GC_2 <- kV[input$p2]*0.5*rslt$GVar
  sigmaPF <- sqrt(rslt$GVar+rslt$VLY/YnV+rslt$VLS/SnV+rslt$VLLoc/LnV+rslt$VLRep/RnV+rslt$VLYS/LYSnV+rslt$VLYL/LYLnV+rslt$VLSL/LSLnV +rslt$residvar/(YnV*SnV*LnV*RnV*SamnV))
  if (input$bredingtype=="selfP" && !input$ckSecTrait) sigmaPF <- sqrt((1+Coefficient_inbreeding_F[input$selfP_F])*(rslt$GVar+rslt$VLY/YnV+rslt$VLS/SnV+rslt$VLLoc/LnV+rslt$VLRep/RnV+rslt$VLYS/LYSnV+rslt$VLYL/LYLnV+rslt$VLSL/LSLnV) +rslt$residvar/(YnV*SnV*LnV*RnV*SamnV))
  if (input$bredingtype=="selfP" && !input$ckSecTrait && input$selfP_early) {
    sigmaPF <- 1/sqrt(rslt$Heritability)
  }
  
  h_y <- sqrt(rslt$GVar)/sigmaPF
  
  if (input$Strategy %in% c('ApWFgs-HS', 'ApWFgs-FS')) simcostV <- SamnV*input$numLines*RnV*SnV*YnV*LnV*input$SampleCos + RnV*YnV*LnV*input$RepCos + YnV*LnV*input$LocCos + input$OtherCos+input$gfamilyno*input$gsampleno*input$gSampleCos + input$gOtherCos + input$numParents*input$gSampleCos
  else if (input$Strategy %in% c('CRyGS-HS', 'CRyGS-FS', 'AgsWFgs-HS', 'AgsWFgs-FS')) simcostV <- input$gfamilyno*input$gsampleno*input$gSampleCos + input$gOtherCos + input$numParents*input$gSampleCos
  else  simcostV <- SamnV*input$numLines*RnV*SnV*YnV*LnV*input$SampleCos + RnV*YnV*LnV*input$RepCos + YnV*LnV*input$LocCos + input$OtherCos
  
  varComp_rslt <- c(ifelse(rslt$VLY==0, NA, rslt$VLY), ifelse(rslt$VLS==0, NA, rslt$VLS), ifelse(rslt$VLLoc==0, NA, rslt$VLLoc), ifelse(rslt$VLRep==0, NA, rslt$VLRep), ifelse(rslt$VLYS==0, NA, rslt$VLYS), ifelse(rslt$VLYL==0, NA, rslt$VLYL), ifelse(rslt$VLSL==0, NA, rslt$VLSL), rslt$residvar)
  
  if (input$ckSecTrait && !(input$modelresponse2 %in% c("NULL", input$modelresponse1)) && rslt$reRun && input$Strategy %in% c("AWFX-HS", "Correlated response (HS)", "Correlated response (FS)")) {
    simcostV <- simcostV + input$UnitCos*input$Nofamily*input$Nosample*input$Norep*input$Nomeasure*input$Noyear*input$Nolocation 
    h_x <- sqrt(rslt$GVar2)/sqrt(rslt$GVar2+rslt$VLY2/YnV+rslt$VLS2/SnV+rslt$VLLoc2/LnV+rslt$residvar2/(YnV*SnV*LnV*RnV*SamnV))
    lmsimdfNN_1[1,1] <- input$Strategy
    lmsimdfNN_1[1,c(2:3, 6:10,21:25)] <- as.numeric(switch(input$Strategy, 
                                                           'AWFX-HS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), GC_1n/sigmaPF+kV[input$p2]*0.5*sqrt(3*rslt$GVar)*h_x*rslt$r_g, (GC_1n/sigmaPF+kV[input$p2]*0.5*sqrt(3*rslt$GVar)*h_x*rslt$r_g)/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                           'Correlated response (HS)'= c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), kV[input$p1]*0.5*h_y*h_x*rslt$r_g*sigmaPF, kV[input$p1]*0.5*h_y*h_x*rslt$r_g*sigmaPF/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                           'Correlated response (FS)'= c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), kV[input$p1]*h_y*h_x*rslt$r_g*sigmaPF, kV[input$p1]*h_y*h_x*rslt$r_g*sigmaPF/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2))         																		
    ))
    lmsimdfNN_1[1,c(4:5, 11:20)] <- as.numeric(switch(input$Strategy, 
                                                      'AWFX-HS'=c(0.5, 0.5, rslt$GVar, rslt$WFPVarV, varComp_rslt),
                                                      'Correlated response (HS)'= c(0.5, NA,  rslt$GVar, rslt$WFPVarV, varComp_rslt),
                                                      'Correlated response (FS)'= c(1, NA, rslt$GVar, rslt$WFPVarV, varComp_rslt)
    ))        
    rslt$lmsimdfN <- rbind(rslt$lmsimdfN, lmsimdfNN_1)	
  }else{
    if ((input$bredingtype== "hsf") && rslt$reRun && input$Strategy %in% c("HS", "HSPT", "AWF-HS", "CRyGS-HS", "ApWFgs-HS", "AgsWFgs-HS")) {
      lmsimdfNN_1[1,1] <- input$Strategy
      lmsimdfNN_1[1,c(2:3, 6:10,21:25)] <- as.numeric(switch(input$Strategy, 
                                                             'HS'= c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                     ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                     ifelse(input$Samplen==1, NA, input$Samplen), GC_1/sigmaPF, GC_1/sigmaPF/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),				
                                                             'HSPT'=c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                      ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                      ifelse(input$Samplen==1, NA, input$Samplen), 2*GC_1/sigmaPF, 2*GC_1/sigmaPF/(YnV+2), simcostV, simcostV/(YnV+2), GC_1/(sigmaPF^2)),				
                                                             'AWF-HS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                        ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                        ifelse(input$Samplen==1, NA, input$Samplen), GC_1n/sigmaPF+3*GC_2/sqrt(rslt$WFPVarV), 
                                                                        (GC_1n/sigmaPF+3*GC_2/sqrt(rslt$WFPVarV))/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             'CRyGS-HS'=c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn),ifelse(input$Samplen==1, NA, input$Samplen), kV[input$p1]*sqrt(input$GSh_x2)*0.5*input$GSaccuracy*sqrt(rslt$GVar), NA, simcostV, NA, GC_1/(sigmaPF^2)),
                                                             
                                                             'ApWFgs-HS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                           ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                           ifelse(input$Samplen==1, NA, input$Samplen), GC_1n/sigmaPF+kV[input$p2]*0.5*sqrt(input$GSh_x2)*sqrt(3*rslt$GVar)*input$GSaccuracy, 
                                                                           (GC_1n/sigmaPF+kV[input$p2]*0.5*sqrt(input$GSh_x2)*sqrt(3*rslt$GVar)*input$GSaccuracy)/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             
                                                             'AgsWFgs-HS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                            ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                            ifelse(input$Samplen==1, NA, input$Samplen), 
                                                                            kV[input$p1n]*sqrt(input$GSh_x2)*0.5*input$GSaccuracy*sqrt(rslt$GVar)+kV[input$p2]*0.5*sqrt(input$GSh_x2)*sqrt(3*rslt$GVar)*input$GSaccuracy, 
                                                                            NA, 
                                                                            simcostV, 
                                                                            NA, GC_1/(sigmaPF^2))                                                                                                                                        
      ))
      lmsimdfNN_1[1,c(4:5, 11:20)] <-  as.numeric(switch(input$Strategy, 
                                                         'HS'=c(0.5, NA, rslt$GVar, NA, varComp_rslt),
                                                         'HSPT'= c(1, NA,  rslt$GVar, NA, varComp_rslt),
                                                         'AWF-HS'= c(0.5, 0.5, rslt$GVar, rslt$WFPVarV, varComp_rslt),
                                                         'CRyGS-HS'=c(0.5, NA, rslt$GVar, NA, varComp_rslt),
                                                         'ApWFgs-HS'= c(0.5, 0.5,  rslt$GVar, NA, varComp_rslt),
                                                         'AgsWFgs-HS'= c(0.5, 0.5, rslt$GVar, NA, varComp_rslt)
      ))
      rslt$lmsimdfN <- rbind(rslt$lmsimdfN, lmsimdfNN_1)        
    }else if((input$bredingtype== "fsf") && rslt$reRun && input$Strategy %in% c("FSF", "FSPT", "AWF-FS (both parents)", "AWF-FS (single parent)", "CRyGS-FS", "ApWFgs-FS", "AgsWFgs-FS")){
      
      lmsimdfNN_1[1,1] <- input$Strategy
      lmsimdfNN_1[1,c(2:3, 6:10,21:25)] <- as.numeric(switch(input$Strategy, 
                                                             'FSF'= c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), GC_1/sigmaPF, GC_1/sigmaPF/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             'FSPT'=c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), 2*GC_1/sigmaPF, 2*GC_1/sigmaPF/(YnV+2), simcostV, simcostV/(YnV+2), GC_1/(sigmaPF^2)),
                                                             'AWF-FS (both parents)'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), 2*GC_1n/sigmaPF+2*GC_2/sqrt(rslt$WFPVarV), (2*GC_1n/sigmaPF+2*GC_2/sqrt(rslt$WFPVarV))/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             'AWF-FS (single parent)'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), GC_1n/sigmaPF+GC_2/sqrt(rslt$WFPVarV), (GC_1n/sigmaPF+GC_2/sqrt(rslt$WFPVarV))/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             'CRyGS-FS'=c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), ifelse(input$Samplen==1, NA, input$Samplen), kV[input$p1]*sqrt(input$GSh_x2)*input$GSaccuracy*sqrt(rslt$GVar), NA, simcostV, NA, GC_1/(sigmaPF^2)),
                                                             'ApWFgs-FS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                           ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                           ifelse(input$Samplen==1, NA, input$Samplen), GC_1n/sigmaPF+kV[input$p2]*sqrt(input$GSh_x2)*sqrt(rslt$GVar)*input$GSaccuracy, 
                                                                           (GC_1n/sigmaPF+kV[input$p2]*sqrt(input$GSh_x2)*sqrt(rslt$GVar)*input$GSaccuracy)/(YnV+1), simcostV, simcostV/(YnV+1), GC_1/(sigmaPF^2)),
                                                             'AgsWFgs-FS'=c(input$p1n, input$p2, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                            ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                            ifelse(input$Samplen==1, NA, input$Samplen), 
                                                                            kV[input$p1n]*sqrt(input$GSh_x2)*input$GSaccuracy*sqrt(rslt$GVar)+kV[input$p2]*sqrt(input$GSh_x2)*sqrt(rslt$GVar)*input$GSaccuracy, 
                                                                            NA, simcostV, NA, GC_1/(sigmaPF^2))		   
      ))
      lmsimdfNN_1[1,c(4:5, 11:20)] <-  as.numeric(switch(input$Strategy, 
                                                         'FSF'=c(0.5, NA, rslt$GVar, NA, varComp_rslt),
                                                         'FSPT'= c(1, NA,  rslt$GVar, NA, varComp_rslt),
                                                         'AWF-FS (both parents)'= c(1, 1, rslt$GVar, rslt$WFPVarV, varComp_rslt),
                                                         'AWF-FS (single parent)'=c(0.5,0.5, rslt$GVar,rslt$WFPVarV, varComp_rslt),
                                                         'CRyGS-FS'= c(1, NA,  rslt$GVar, NA, varComp_rslt),
                                                         'ApWFgs-FS'= c(1, 1, rslt$GVar, NA, varComp_rslt),
                                                         'AgsWFgs-FS'= c(1, 1, rslt$GVar, NA, varComp_rslt)
      ))
      rslt$lmsimdfN <- rbind(rslt$lmsimdfN, lmsimdfNN_1)       
    }else if(input$bredingtype== "selfP" && rslt$reRun && input$Strategy %in% "R (Inbred lines)") {        
      lmsimdfNN_1[1,1] <- input$Strategy
      lmsimdfNN_1[1,c(2:3, 6:10,21:25)] <- as.numeric(switch(input$Strategy,
                                                             "R (Inbred lines)"=c(input$p1, NA, ifelse(input$Yearn==1, NA, input$Yearn), ifelse(input$Sean==1, NA, input$Sean), 
                                                                                  ifelse(input$Locn==1, NA, input$Locn), ifelse(input$Repn==1, NA, input$Repn), 
                                                                                  ifelse(input$Samplen==1, NA, input$Samplen), GC_1/sigmaPF, GC_1/sigmaPF/as.numeric(input$CycleL), simcostV, simcostV/as.numeric(input$CycleL), GC_1/(sigmaPF^2)))
      )		
      
      lmsimdfNN_1[1,c(4:5, 11:20)] <-  as.numeric(switch(input$Strategy, 
                                                         "R (Inbred lines)"= c(1, NA, rslt$GVar, NA, varComp_rslt)
      ))
      rslt$lmsimdfN <- rbind(rslt$lmsimdfN, lmsimdfNN_1)       
    }else if(input$bredingtype== "selfP" && rslt$reRun && input$Strategy %in% "R (Early stage)") {        
      lmsimdfNN_1[1,1] <- input$Strategy
      lmsimdfNN_1[1,c(2:3, 6:10,21:25)] <- as.numeric(switch(input$Strategy,
                                                             "R (Early stage)"=c(input$p1, NA, NA, NA, NA, NA, NA, GC_1/sigmaPF, GC_1/sigmaPF/as.numeric(input$CycleL), simcostV, simcostV/as.numeric(input$CycleL), NA))
      )		
      
      lmsimdfNN_1[1,c(4:5, 11:20)] <-  as.numeric(switch(input$Strategy, 
                                                         "R (Early stage)"= c(1, NA, rslt$GVar, NA, varComp_rslt)
      ))
      rslt$lmsimdfN <- rbind(rslt$lmsimdfN, lmsimdfNN_1)       
    }else NULL	  
  }
  
})

# observeEvent(input$bredingtype, {	
# rslt$lmsimdfN <- lmsimdfNN  
# rslt$reRun <- FALSE

# # Clear the previous deletions
# rv$deletedRows <- NULL
# rv$deletedRowIndices = list()
# })

toListen <- reactive({
  list(input$bredingtype, input$selfP_early)
})

observeEvent(toListen(), {
  rslt$lmsimdfN <- lmsimdfNN  
  rslt$reRun <- FALSE
  
  # Clear the previous deletions
  rv$deletedRows <- NULL
  rv$deletedRowIndices = list()
  
  rslt$mod <- NULL
  rslt$rf <- NULL
  rslt$summary <- NULL
  rslt$anova <- NULL
  rslt$varcomp <- NULL
  rslt$HeritabilityMod <- NULL
  rslt$lmsimdfN <- lmsimdfNN
})

observeEvent(input$deletePressed, {
  rowNum <- parseDeleteEvent(input$deletePressed)
  dataRow <- rslt$lmsimdfN[rowNum,]
  
  # Put the deleted row into a data frame so we can undo
  # Last item deleted is in position 1
  rv$deletedRows <- rbind(dataRow, rv$deletedRows)
  rv$deletedRowIndices <- append(rv$deletedRowIndices, rowNum, after = 0)
  
  # Delete the row from the data frame
  rslt$lmsimdfN <- rslt$lmsimdfN[-rowNum,]
})

observeEvent(input$undo, {
  if(nrow(rv$deletedRows) > 0) {
    row <- rv$deletedRows[1, ]
    rslt$lmsimdfN <- addRowAt(rslt$lmsimdfN, row, rv$deletedRowIndices[[1]])
    
    # Remove row
    rv$deletedRows <- rv$deletedRows[-1,]
    # Remove index
    rv$deletedRowIndices <- rv$deletedRowIndices[-1]
  }
})

# Disable the undo button if we have not deleted anything
output$undoUI <- renderUI({
  if(!is.null(rv$deletedRows) && nrow(rv$deletedRows) > 0) {
    actionButton('undo', label = 'Undo delete', icon('undo'), class = "btn-info btn-xs")
  } else {
    actionButton('undo', label = 'Undo delete', icon('undo'), class = "btn-info btn-xs", disabled = TRUE)
  }
})


###### Output 

output$datastr3 <- renderPrint({
  if (!is.null(rslt$modeldata) && !is.null(input$hideDataInfo2)) list("No. of rows and columes of data"=dim(rslt$modeldata),
                                                                      "Data info"=datainfo(rslt$modeldata[, 1:min(dim(rslt$modeldata)[2], 50), drop=FALSE])) else return()
})   

output$modsummary <- renderPrint({
  if (!is.null(rslt$summary)) {
    modelsummary <- rslt$summary
    if (input$modcoefficients) modelsummary$coefficients <- NULL
    cat("Model Summary:\n\n")
    try(print(modelsummary, corr=FALSE), TRUE)          
  }else NULL
  
  if (!is.null(rslt$anova)) {
    cat("\nANOVA Table:\n\n")
    print(rslt$anova)    
  }else NULL
  
  if (!is.null(rslt$varcomp)) {    
    cat("\nVariance Components:\n\n")
    print(rslt$varcomp)  
    cat("\nNote:
	1. '2.5 %' and '97.5 %' are lower limit and upper limit of 95% confidence interval of the estimate.
	2. 'LRT.p_value' is the p-value of Likelihood Ratio Test(LRT) by droping the associated single term in the model.")	  
  }else NULL
  
  if (!is.null(rslt$HeritabilityMod) && input$CaHeritability) {
    if (input$bredingtype=="selfP" && input$selfP_early && !input$ckSecTrait)  cat(paste("\n\nLine Mean Generalized Heritability (modified Cullis, 2006) of trait ", input$modelresponse1, ": ", round(rslt$HeritabilityMod, 4), sep="")) 
    else if (input$bredingtype== "selfP" && !input$ckSecTrait)  cat(paste("\n\nLine Mean Heritability of trait ", input$modelresponse1, ": ", round(rslt$HeritabilityMod, 4), " (s.e. ", round(attr(rslt$HeritabilityMod, "Std.error"),4), ") at ", input$selfP_F, " generation of line derivation.\n\n", sep="")) 
    else cat(paste("\n\nLine Mean Heritability of trait ", input$modelresponse1, ": ", round(rslt$HeritabilityMod, 4), " (s.e. ", round(attr(rslt$HeritabilityMod, "Std.error"),4), ")\n\n", sep="")) 
  }
  
  if (!is.null(rslt$Heritability2) && !(input$modelresponse2 %in% c("NULL", input$modelresponse1) && input$ckSecTrait)){
    cat(paste("Line Mean Heritability of trait ", input$modelresponse2, ": ", round(rslt$Heritability2, 4), ".\n\n", sep="")) 
    cat(paste("The estimated genetic correlation between traits ",  input$modelresponse1, " and ", input$modelresponse2, " is ", round(rslt$r_g, 2), ".\n\n", sep=""))	
  }  
})

output$infoSimout <- renderPrint({
  if (!is.null(rslt$HeritabilitySim)) cat(round(rslt$HeritabilitySim, 4)) 
})

output$modblup <- renderPrint({ 
  if (!is.null(rslt$rf) && input$blup%in%names(rslt$rf)) {
    blupterms <- unlist(strsplit(input$blup, "\\:"))
    withProgress(message = 'Calculating BLUP ...', value = 0.1, {
      rfb <- rslt$rf[[input$blup]]
      serf <- se.ranef(rslt$mod)[[input$blup]]
      ranefout <- cbind(rownames(rfb), rfb, serf)
      colnames(ranefout)  <- c(input$blup, "Random effect", "Standard error")
      ndf <- rslt$modeldata %>%
        dplyr::filter(!is.na(rslt$modeldata[, input$modelresponse1])) %>%        
        dplyr::mutate(fixedfit=as.numeric(model.matrix(rslt$mod)%*%fixef(rslt$mod))) %>%
        group_by(1) %>%
        summarise(mFix=mean(fixedfit)) 
      ranefout$BLUP <- ndf$mFix+ranefout[, "Random effect"]
      rownames(ranefout) <- NULL
      rslt$ranefout <- ranefout %>% 
        separate(1, blupterms, ":")   
      # print(ranefout)	  
      # print(mean(ranefout$BLUP))			   	  
      cat(paste("\nThe BLUP mean of ", input$blup, "\n", sep=""))
      cat(mean(ranefout$BLUP))	
      cat(paste("\n\nError Coefficient of Variation for ", input$blup, ":\n", sep=""))
      cat(paste(round(sqrt(rslt$residvar)*100/mean(ranefout$BLUP), 2), "%", sep=""))
      cat(paste("\n\nBLUP of ", input$blup, ":\n\n", sep=""))	  
    })
    if (input$ckPatternAnalysis && ("Line" %in% blupterms) && length(blupterms)==2) {
      
      patternref <- rslt$rf[[input$blup]]
      patternref <- cbind(rownames(patternref), patternref)
      colnames(patternref)  <- c(input$blup, "BLUP")
      rownames(patternref) <- NULL
      
      patternref <-  patternref %>% 
        separate(input$blup, blupterms, ":") 
      
      LineBlup <- rslt$rf[["Line"]]
      LineBlup <- cbind(rownames(LineBlup), LineBlup)
      colnames(LineBlup)  <- c("Line", "LineBlup")
      rownames(LineBlup) <- NULL  
      
      blupm <-  left_join(patternref, LineBlup, by="Line") %>%
        dplyr::mutate(BLUP=BLUP+LineBlup)%>%
        # select_(.dots=c(blupterms, "BLUP")) %>% 
        dplyr::select(all_of(c(blupterms, "BLUP"))) %>%
        pivot_wider(names_from = blupterms[2], values_from = "BLUP") 
      # spread_(blupterms[2], "BLUP")      
      # rownames(blupm) <- blupm$Line
      
      blupm.std=as.matrix(scale(blupm[,-1], center=T, scale=T))
      rownames(blupm.std) <- blupm$Line
      blupm.std <- na.omit(blupm.std)
      if (ncol(blupm.std) > 2) {
        rslt$patterngroup <- cimN(blupm.std,
                                  clust.method = c("ward.D", "ward.D"),
                                  symkey=F,
                                  color=colorRampPalette(c("blue","white", "red"))(25))$hgroup
        rslt$blupm.std <- blupm.std
      }
    }
  }else NULL
})

output$hot = renderRHandsontable({
  if (!is.null(rslt$rf) && input$blup%in%names(rslt$rf) && !is.null(rslt$ranefout)) {
    
    blupTable <- rslt$ranefout
    blupTable <- blupTable[, c(1:(ncol(blupTable)-2), ncol(blupTable), ncol(blupTable)-1)]
    rhandsontable(blupTable, height=600, width=400, rowHeaders = NULL, readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(columnSorting = TRUE, format="0.000") %>%
      hot_rows(fixedRowsTop = 1)%>%
      hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                       var csv = csvString(this, sep=',', dec='.');
                       var link = document.createElement('a');
                       link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                       encodeURIComponent(csv));
                       link.setAttribute('download', 'blup_output.csv');
                       document.body.appendChild(link);
                       link.click();
                       document.body.removeChild(link);}"))))
  }					   
})

output$modblue <- renderPrint({
  if (inherits(rslt$mod, "lmerMod") && !is.null(input$blue) && input$blue!="NULL") {
    withProgress(message = 'Calculating BLUE ...', value = 0.1, {
      if (input$transtype=="I(") blueOut <- try(predictmeans(rslt$mod, input$blue, plot=FALSE, Df=21), TRUE)
      else blueOut <- try(predictmeans(rslt$mod, input$blue, plot=FALSE, Df=21, trans=transfun(x, input$transtype)), TRUE)
      if (!is(blueOut, 'try-error'))  rslt$blueOut <- blueOut else rslt$blueOut <- NULL
      cat(paste("\nBLUE of ", input$blue, ":\n\n", sep=""))
      # print(rslt$blueOut[-1:-2])
      print(rslt$blueOut[3:4])
    })
  }else NULL
})

output$hotBlue = renderRHandsontable({
  if (!is.null(input$blue) && input$blue!="NULL") {
    # pmDF <- as.data.frame(rslt$blueOut[[1]])
    # names(pmDF)[length(names(pmDF))] <- "BLUE"
    # if (length(unique(rslt$blueOut[[2]])) > 1) {
    # stderDF <- as.data.frame(rslt$blueOut[[2]])
    # pmDF$stder <- stderDF$Freq
    # }else pmDF$stder <- rslt$blueOut[[2]]
    
    pmDF <- rslt$blueOut$mean_table
    names(pmDF)[names(pmDF)=="Predicted means"] <- "BLUE"
    rhandsontable(pmDF, height=1000, width=600, rowHeaders = NULL, readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(columnSorting = TRUE, format="0.000") %>%
      hot_rows(fixedRowsTop = 1)%>%
      hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                       var csv = csvString(this, sep=',', dec='.');
                       var link = document.createElement('a');
                       link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                       encodeURIComponent(csv));
                       link.setAttribute('download', 'blue_output.csv');
                       document.body.appendChild(link);
                       link.click();
                       document.body.removeChild(link);}"))))
  }					   
})

output$patternheat <- renderPlot({
  if (is.matrix(rslt$blupm.std)) {
    cimN(rslt$blupm.std,
         clust.method = c("ward.D", "ward.D"),
         symkey=F,
         color=colorRampPalette(c("blue","white", "red"))(25))
  }
}, height = 600, width = 600)

output$patterngroup <- renderPrint({
  if (!is.null(rslt$patterngroup)) split(names(rslt$patterngroup), rslt$patterngroup)
})

output$patternPCA <- renderPrint({
  if (is.matrix(rslt$blupm.std)) {
    patternpca <- prcomp(rslt$blupm.std, center = TRUE, scale = TRUE)
    print(summary(patternpca))
    pcscores <- patternpca$x[, 1:2]
    rownames(pcscores) <- rownames(rslt$blupm.std)
    cat("\nPC scores:\n")
    print(pcscores)
  }
})

output$patternbiplot <- renderPlot({
  if (is.matrix(rslt$blupm.std)) 
    pcaTraits(rslt$blupm.std, grFactor=factor(rslt$patterngroup), labels=names(rslt$patterngroup), ellipse=FALSE, varname.size = 6, varname.adjust = -2, mtitle=NULL)
}, height = 600, width = 600)

output$residualplot <- renderPlot({
  residplot(model=rslt$mod, newwd=FALSE)
}, height = 600, width = 600)


output$simtable1 <- DT::renderDataTable({ #family;
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Delete'),
        th(rowspan = 2, 'Strategy'),
        th(rowspan = 2, 'Selection pressure (among family)'),
        th(rowspan = 2, 'Selection pressure (within family)'),
        th(rowspan = 2, 'Parental control (among family)'),
        th(rowspan = 2, 'Parental control (within family)'),
        th(colspan = 5, '       Number of')
      ),
      tr(
        lapply(c('Year', 'Sea', 'Loc', 'Rep', 'Sample'), th)
      )
    )
  ))
  if (input$bredingtype== "selfP") 
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Delete'),
          th(rowspan = 2, 'Strategy'),
          th(rowspan = 2, 'Selection pressure (among lines)'),
          th(rowspan = 2, 'Selection pressure (within lines)'),
          th(rowspan = 2, 'Parental control (among lines)'),
          th(rowspan = 2, 'Parental control (within lines)'),
          th(colspan = 5, '       Number of')
        ),
        tr(
          lapply(c('Year', 'Sea', 'Loc', 'Rep', 'Sample'), th)
        )
      )
    ))
  
  datadf <- rslt$lmsimdfN
  if (input$bredingtype== "hsf") captionw <- "HS, half sib family; HSPT, HS with progeny testing; AWF-HS, among & within HS, CRyGS-HS, 
			  correlated response to selection of trait Y based on genomic selection (GS) of HS families; ApWFgs-HS, among family phenotypic 
			  selection and within family GS of HS families; AgsWFgs-HS, among and within family GS of HS families, * Percentage of genetic 
			  gain relative to family population mean, ** Percentage of genetic gain relative to industy standard, *** Cost per percentage of 
			  genetic gain per cycle."
  else if (input$bredingtype== "fsf") captionw <- "FSF, full sib family; FSPT, FS with progeny testing; AWF-FS, among and within FS. CRyGS-FS, 
			  correlated response to selection of trait Y based on genomic selection (GS) of FS families; ApWFgs-FS, among family phenotypic 
			  selection and within family GS of FS families; AgsWFgs-FS, among and within family GS of FS families, * Percentage of genetic 
			  gain relative to family population mean, ** Percentage of genetic gain relative to industy standard, *** Cost per percentage of 
			  genetic gain per cycle."
  else if (input$bredingtype== "selfP") captionw <- "Self-pollination, * Percentage of genetic 
			  gain relative to family population mean, ** Percentage of genetic gain relative to industy standard, *** Cost per percentage of 
			  genetic gain per cycle."
  else captionw <- NULL		 
  
  if (input$ckSecTrait && !(input$modelresponse2 %in% c("NULL", input$modelresponse1))) captionw <- "AWFX-HS, among and within half sib family selection where within family selection 
		    is on the secondary trait X; CRy, correlated response to selection of Y based on selection for X. FS, full sib families, 
			* Percentage of genetic gain relative to family population mean, ** Percentage of genetic gain relative to industy standard, 
			*** Cost per percentage of genetic gain per cycle."    
  
  if (nrow(rslt$lmsimdfN) > 0){
    nvarn <- c('GVar', 'WFPVar', 'GYVar', 'GSVar', 'GLVar', 'GRVar', 'GYSVar', 'GYLVar', 'GSLVar', 'EVar', 'deltaGCycle', 'deltaGYear', 'CostCycle', 'CostYear', 'RSD')[(sapply(datadf, class)=="numeric")[-(1:10)]]
    datadf[, nvarn] <- round(datadf[, nvarn], 4)
    deltaGCycleV <- datadf$deltaGCycle
    # datadf$deltaGCycle <- paste(deltaGCycleV, paste("(",round(deltaGCycleV*100/rslt$LineMean, 2), "%)", sep=""))
    datadf$deltaGCycle <- paste(deltaGCycleV, paste("(",round(deltaGCycleV*100/rslt$LineMean, 2), "%)*", sep=""),
                                paste("(",round((deltaGCycleV+rslt$LineMean-input$IndustryStd)*100/input$IndustryStd, 2), "%)**", sep=""))
    datadf$deltaGCycle[is.na(deltaGCycleV)] <- NA
    
    CostCycleV <- datadf$CostCycle
    datadf$CostCycle <- paste(CostCycleV, paste("(", round(CostCycleV/(deltaGCycleV*100/rslt$LineMean), 2), "%)***", sep=""), sep="")
    datadf$CostCycle[is.na(CostCycleV)] <- NA	
  }else datadf <- matrix(NA, 0, 25)
  rslt$lmsimdfformat <- datadf   
  #  datatable(datadf[, 1:10], container = sketch, class = 'cell-border stripe', extensions = 'Buttons', options=list(paging=FALSE, ordering=FALSE, searching=FALSE, columnDefs = list(list(className = 'dt-head-center')), dom = 'Bfrtip', buttons = "excel"), rownames = FALSE, caption = captionw)
  
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste('delete_button', i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(datadf[, 1:10])), f))
  
  datatable(cbind(delete = deleteCol, datadf[, 1:10]), 
            container = sketch, class = 'cell-border stripe', 
            extensions = 'Buttons', 
            escape = FALSE,
            options=list(paging=FALSE, ordering=FALSE, searching=FALSE, columnDefs = list(list(className = 'dt-head-center')), dom = 'Bfrtip', buttons = "excel", list(targets = 1, sortable = FALSE)), rownames = FALSE, caption = captionw)
  
})	


output$simtable2 <- DT::renderDataTable({
  if (input$bredingtype== "hsf") aa <- 4 else aa <- 2 
  if (input$ckSecTrait && !(input$modelresponse2 %in% c("NULL", input$modelresponse1))) aa <- "4 or 2"
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Strategy'),
        th(rowspan = 2, HTML(paste("Genetic variance &sigma;<sup>2</sup><sub>A</sub>/", aa, sep=""))),
        th(rowspan = 2, 'Within family phenotypic variance'),
        th(colspan = 8, '                    Interactions')
      ),
      tr(
        th(HTML("&sigma;<sup>2</sup><sub>AxY</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxS</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxL</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxR</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxYxS</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxYxL</sub>")),
        th(HTML("&sigma;<sup>2</sup><sub>AxSxL</sub>")),
        th(HTML("&epsilon;<sup>2"))	
      )
    )
  ))
  
  if (input$bredingtype== "selfP") 
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Strategy'),
          th(rowspan = 2, HTML(paste("Genotypic variance &sigma;<sup>2</sup><sub>G</sub>", sep=""))),
          th(rowspan = 2, 'Within lines phenotypic variance'),
          th(colspan = 8, '                    Interactions')
        ),
        tr(
          th(HTML("&sigma;<sup>2</sup><sub>GxY</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxS</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxL</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxR</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxYxS</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxYxL</sub>")),
          th(HTML("&sigma;<sup>2</sup><sub>GxSxL</sub>")),
          th(HTML("&epsilon;<sup>2"))	
        )
      )
    ))
  
  datadf <- rslt$lmsimdfN
  if (nrow(rslt$lmsimdfN) > 0){
    nvarn <- c('GVar', 'WFPVar', 'GYVar', 'GSVar', 'GLVar', 'GRVar', 'GYSVar', 'GYLVar', 'GSLVar', 'EVar', 'deltaGCycle', 'deltaGYear', 'CostCycle', 'CostYear', 'RSD')[(sapply(datadf, class)=="numeric")[-(1:10)]]
    datadf[, nvarn] <- round(datadf[, nvarn], 4)
    deltaGCycleV <- datadf$deltaGCycle
    datadf$deltaGCycle <- paste(deltaGCycleV, paste("(",round(deltaGCycleV*100/rslt$LineMean, 2), "%)*", sep=""), paste("(",round((deltaGCycleV+rslt$LineMean-input$IndustryStd)*100/input$IndustryStd, 2), "%)**", sep=""))
    datadf$deltaGCycle[is.na(deltaGCycleV)] <- NA
    
    CostCycleV <- datadf$CostCycle
    datadf$CostCycle <- paste(CostCycleV, paste("(", round(CostCycleV/(deltaGCycleV*100/rslt$LineMean), 2), ")***", sep=""), sep="")
    datadf$CostCycle[is.na(CostCycleV)] <- NA	
  }else datadf <- matrix(NA, 0, 25)
  
  rslt$lmsimdfformat <- datadf 
  datatable(datadf[, c(1, 11:20)], container = sketch, class = 'cell-border stripe', extensions = 'Buttons', options=list(paging=FALSE, ordering=FALSE, searching=FALSE, columnDefs = list(list(className = 'dt-head-center')), dom = 'Bfrtip', buttons = "excel"), rownames = FALSE)
})

output$simtable3 <- DT::renderDataTable({
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th('Strategy'),
        th(HTML("&Delta;G<sub>c</sub> (product units)")), 
        th(HTML("&Delta;G<sub>a</sub> (product units)")),
        th(HTML("&Delta;G<sub>c</sub> ($)")),
        th(HTML("&Delta;G<sub>a</sub> ($)")),
        th(HTML("&Delta;G<sub>c</sub>/&sigma;<sub>P</sub>"))
      )
    )
  ))  
  
  if (input$bredingtype=='selfP')   sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th('Strategy'),
        th(HTML("R<sub>c</sub> (product units)")), 
        th(HTML("R<sub>a</sub> (product units)")),
        th(HTML("R<sub>c</sub> ($)")),
        th(HTML("R<sub>a</sub> ($)")),
        th(HTML("R<sub>c</sub>/&sigma;<sub>P</sub>"))
      )
    )
  ))  
  
  datadf <- rslt$lmsimdfN
  if (nrow(rslt$lmsimdfN) > 0){
    nvarn <- c('GVar', 'WFPVar', 'GYVar', 'GSVar', 'GLVar', 'GRVar', 'GYSVar', 'GYLVar', 'GSLVar', 'EVar', 'deltaGCycle', 'deltaGYear', 'CostCycle', 'CostYear', 'RSD')[(sapply(datadf, class)=="numeric")[-(1:10)]]
    datadf[, nvarn] <- round(datadf[, nvarn], 4)
    deltaGCycleV <- datadf$deltaGCycle
    datadf$deltaGCycle <- paste(deltaGCycleV, paste("(",round(deltaGCycleV*100/rslt$LineMean, 2), "%)*", sep=""), 
                                paste("(",round((deltaGCycleV+rslt$LineMean-input$IndustryStd)*100/input$IndustryStd, 2), "%)**", sep=""))
    datadf$deltaGCycle[is.na(deltaGCycleV)] <- NA
    yearN <- ifelse(is.na(datadf$Year), 1, datadf$Year)
    datadf$deltaGYear <- paste(datadf$deltaGYear, paste("(",round(datadf$deltaGYear*100/rslt$LineMean/yearN, 2), "%)*", sep=""))
    CostCycleV <- datadf$CostCycle
    datadf$CostCycle <- paste(CostCycleV, paste(" (", round(CostCycleV/(deltaGCycleV*100/rslt$LineMean), 2), ")***", sep=""), sep="")
    datadf$CostCycle[is.na(CostCycleV)] <- NA    
  }else datadf <- matrix(NA, 0, 25)
  
  rslt$lmsimdfformat <- datadf 
  datatable(datadf[, c(1, 21:25)], container = sketch, class = 'cell-border stripe', extensions = 'Buttons', options=list(paging=FALSE, ordering=FALSE, searching=FALSE, columnDefs = list(list(className = 'dt-head-center')), dom = 'Bfrtip', buttons = "excel"), rownames = FALSE)
})

output$formula_LMH <- renderUI({
if (input$bredingtype %in% c("hsf", "fsf")) {
  withMathJax(
    helpText('Narrow sense heritability \\(h_{n}^2\\) on a half-sib \\(HS\\) or full-sib \\(FS\\) family mean basis across locations \\(L\\), seasons \\(S\\) and years \\(Y\\):
               $$h_{n}^2=\\frac{\\sigma_{A}^2}{\\sigma_{A}^2+\\frac{\\sigma_{AL}^2}{n_L}+\\frac{\\sigma_{AS}^2}{n_S}+\\frac{\\sigma_{AY}^2}{n_{Y}}+\\frac{\\sigma_{AR}^2}{n_{R}}+\\frac{\\sigma_{ASY}^2}{n_{S}n_{Y}}+\\frac{\\sigma_{ALY}^2}{n_{L}n_{Y}}+\\frac{\\sigma_{ALS}^2}{n_{L}n_{S}}+\\frac{\\sigma_{\\epsilon}^2}{n_{L}n_{S}n_{Y}n_{R}n_{Samp}}}$$
where: \\(\\sigma_{A}^2\\), additive variance \\(\\frac{1}{4}\\sigma_{A}^2\\) estimated among \\(HS\\) families and \\(\\frac{1}{2}\\sigma_{A}^2\\) estimated among \\(FS\\) families); \\(\\sigma_{AL}^2\\), additive-by-location interaction; \\(\\sigma_{AS}^2\\), additive-by-season interaction; \\(\\sigma_{AY}^2\\), additive-by-year interaction; 
\\(\\sigma_{AR}^2\\), additive-by-replicate interaction; 
\\(\\sigma_{ASY}^2\\), additive-by-season-by-year interaction;  
\\(\\sigma_{ALY}^2\\), additive-by-location-by-year interaction; 
\\(\\sigma_{ALS}^2\\), additive-by-location-by-season interaction;
\\(\\sigma_{\\epsilon}^2\\), experimental error. 
\\(n_L\\), number of locations; \\(n_S\\), number of seasons; \\(n_Y\\), number of years; \\(n_R\\), number of replications or blocks; \\(n_{Samp}\\), number of samples within family. 
$$$$
'))
}else{
  if (input$selfP_early) {
    withMathJax(
    helpText('Generalized heritability (\\(H_{Cullis}^2\\))
	$$H_{Cullis}^2 = 1-\\frac{\\bar v_{\\Delta}^{BLUP}}{2\\sigma_{G}^2}$$
where \\(\\sigma_{G}^2\\), genotypic variance; 
\\(\\bar v_{\\Delta}^{BLUP}\\), average of squared standard errors of the genotypic BLUPs.
$$$$
In the "Modified Generalized Heritability" estimated in DeltaGen, "2" in the denominator of the equation is removed as the average of squared standard errors (\\(\\bar v_{\\Delta}^{BLUP}\\)) is based on the sum of individual BLUP squared standard errors divided by the total number of genotypic BLUP values. In the Cullis et al. (2006) equation, this is based on average pairwise prediction error variance of genotype effects.
$$$$
Predicted response to selection (\\(R\\))
$$R = \\frac{ir\\sigma_G}{L}$$
Where \\(i\\), selection intensity; 
      \\(r\\), accuracy (\\(\\sqrt{H_{Cullis}^2}\\)); 
	  \\(\\sigma_G\\), square root of genotypic variance; 
	  \\(L\\), length of cycle.
$$$$
	'))  
  }else{
     withMathJax(
    helpText('Heritability on a line mean basis across locations \\(L\\), seasons \\(S\\) and years \\(Y\\):
               $$h_{b}^2=\\frac{\\sigma_{G}^2}{\\sigma_{G}^2+\\frac{\\sigma_{GL}^2}{n_L}+\\frac{\\sigma_{GS}^2}{n_S}+\\frac{\\sigma_{GY}^2}{n_{Y}}+\\frac{\\sigma_{GR}^2}{n_{R}}+\\frac{\\sigma_{GSY}^2}{n_{S}n_{Y}}+\\frac{\\sigma_{GLY}^2}{n_{L}n_{Y}}+\\frac{\\sigma_{GLS}^2}{n_{L}n_{S}}+\\frac{\\sigma_{\\epsilon}^2}{n_{L}n_{S}n_{Y}n_{R}n_{Samp}}}$$
where: \\(\\sigma_{G}^2\\), genotypic variance estimated among lines; \\(\\sigma_{GL}^2\\), genotype-by-location interaction; \\(\\sigma_{GS}^2\\), genotype-by-season interaction; \\(\\sigma_{GY}^2\\), genotype-by-year interaction; 
\\(\\sigma_{GR}^2\\), genotype-by-replicate interaction; 
\\(\\sigma_{GSY}^2\\), genotype-by-season-by-year interaction;  
\\(\\sigma_{GLY}^2\\), genotype-by-location-by-year interaction; 
\\(\\sigma_{GLS}^2\\), genotype-by-location-by-season interaction;
\\(\\sigma_{\\epsilon}^2\\), experimental error. 
\\(n_L\\), number of locations; \\(n_S\\), number of seasons; \\(n_Y\\), number of years; \\(n_R\\), number of replications or blocks; \\(n_{Samp}\\), number of samples within line. 
$$$$
'))  
  }
}
})

######################### Multiple traits panel #########################  

###### sever interface

output$multitraits_ui <- renderUI({
  if (!is_empty(input$selecteddata)) {
    rslt$modeldata <- r_data[[input$selecteddata]] 
    setuptermsManova <- c("Year", "Season", "Location", "Replicates", "Sample", "Line")
    realtermsManova <- intersect(setuptermsManova, names(rslt$modeldata))
    if (length(realtermsManova) > 0) manovaTerms <- attr(terms(formula(paste("~(", paste(realtermsManova, collapse="+"), ")^2", sep=""))), "term.labels") else manovaTerms <- NULL
    wellPanel(
      h4("Multivariate Model"),
      radioButtons('multitype', '', c('Plot'="mplot", 'MANOVA'='manova', 'Selection Index'='selectIndex'), inline=TRUE),
      
      selectizeInput('multiresponse', 'Multiple traits:', choices =names(rslt$modeldata)[sapply(rslt$modeldata, is.numeric)], 
                     multiple = TRUE, options = list(minItems = 2, plugins = list('remove_button', 'drag_drop'))),
      conditionalPanel("input.multitype=='manova'",
                       selectizeInput('multimodelterms', 'MANOVA terms:', manovaTerms,  multiple = TRUE,
                                      options = list(create = TRUE, plugins = list('remove_button', 'drag_drop'))),
                       actionButton("multirun", "Run", class = "btn-success")),
      conditionalPanel("input.multitype=='mplot'",
	  				   selectizeInput('plot_of_pcs', 'Plot of PCs:', choices =c("PC1"=1, "PC2"=2, "PC3"=3, "PC4"=4), selected=c("PC1"=1, "PC2"=2), 
                         multiple = TRUE, options = list(minItems = 2, maxItems = 2, plugins = list('remove_button', 'drag_drop'))),
                       checkboxInput("mpcavar", "Show variable vectors?", TRUE),
                       conditionalPanel(condition="input.mpcavar",
                                        sliderInput("mpcavarnsize", "Variable name size:", min = 1, max = 20, value = 3, step = 0.5),
                                        sliderInput("mpcavarnadj", "Variable name adjust:", min = -5, max = 3, value = 2, step = 0.5)
                       ),					
                       selectInput('colgrp', 'Color by group', choices =c("NULL", names(rslt$modeldata)[sapply(rslt$modeldata, is.factor)])),
                       conditionalPanel("input.colgrp!='NULL'",
                                        checkboxInput("ellipse", "Add ellipse?", value = FALSE),
										conditionalPanel(condition="input.ellipse",
                                        sliderInput("mpcaellipseProb", "Confidence of ellipse:", min = 0.68, max = 0.95, value = 0.68, step=0.01)
                       )),                       					   
                       selectInput(inputId = "mpcalabels", label = "Label Variable:", choices =c("NULL", names(rslt$modeldata)[sapply(rslt$modeldata, function(x) !is.factor(x))])),
                       conditionalPanel(condition="input.mpcalabels!='NULL'",
                                        sliderInput("mpcalabelsize", "Label size:", min = 1, max = 15, value = 3, step = 0.5)),
                       conditionalPanel(condition="input.mpcalabels=='NULL'",				  
                                        sliderInput("mpcaalpha", "Dot transparency:", min = 0, max = 1, value = 1, step = 0.1))	
										),
      conditionalPanel("input.multitype=='selectIndex'",
                       textInput('indexweights', 'Index weights:', value = ""),
                       helpText('Enter your index weights separated by comma e.g. 0.25, 1, -0.01'),	
                       wellPanel(
                         selectizeInput('modelfixtermSI', 'LME fixed terms:', c("NULL"=1, rslt$fterms),  multiple = TRUE,
                                        options = list(create = TRUE, plugins = list('remove_button', 'drag_drop'))),
                         selectizeInput('modelrandtermSI', 'LME random terms:', c("NULL", rslt$rterms),  multiple = TRUE,
                                        options = list(create = TRUE, plugins = list('remove_button', 'drag_drop')))),
                       radioButtons('gVCOVopt', 'Additive genetic variance-covariance matrix from:', c('MANOVA', 'Molecular Marker based'), inline = TRUE),
                       conditionalPanel("input.gVCOVopt=='MANOVA'",  
                                        selectizeInput('multimodeltermsSI', 'MANOVA terms:', manovaTerms,  multiple = TRUE,
                                                       options = list(create = TRUE, plugins = list('remove_button', 'drag_drop')))
                       ),
                       conditionalPanel("input.gVCOVopt!='MANOVA'", 
                                        textAreaInput(inputId="matrixArea", label=NULL, value = "", placeholder = "Paste your matrix with the variables ordered as in 'Multiple traits'.")
                       ),
                       selectInput("pSI", label = "Selection pressure:", choices=as.character(c(0.2, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.15, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005, 0.001))),
                       checkboxInput("deltaGSI", HTML("Show &Delta;G?"), value = FALSE),
                       br(),
                       actionButton("selectIndexrun", "Run", class = "btn-success"),
                       actionButton("SHindexhelpBut", "Help", class = "btn-primary")
      ) 		
    )
  }else NULL
}) # end of output$pcabiplot_ui  

observeEvent(input$SHindexhelpBut, {
  toggleModal(session, "SHindexhelp", "open")
})

observeEvent(input$selectIndexrun, {
  withProgress(message = 'Calculating selection index ...', value = 0.1, {
    weightSI <- na.omit(as.numeric(unlist(strsplit(input$indexweights, split=","))))
    validate(need(length(input$multiresponse) == length(weightSI), "Please check your indes weights input!"))
    if (length(input$multiresponse) > 1 && length(input$multiresponse) < 7 && length(input$multiresponse) == length(weightSI) && "Line" %in% input$modelrandtermSI) {
      nameSI <- intersect(c("Year", "Season", "Location", "Replicates", "Row", "Column", "Sample", "Line", input$multiresponse), names(rslt$modeldata))
      dfSI <- droplevels(na.omit(rslt$modeldata[,nameSI]))
      BLUPSI <- matrix(NA, length(levels(dfSI$Line)), length(input$multiresponse))
      rownames(BLUPSI) <- levels(dfSI$Line)
      colnames(BLUPSI) <- input$multiresponse
      varanceSI <- rep(0, length(input$multiresponse))
      for (i in 1:length(input$multiresponse)){
        lmemodIndex <- try(lmer(formula(paste(input$multiresponse[i], paste("~", paste(input$modelfixtermSI, collapse="+"), sep=""), '+ (', paste(paste0("1|", input$modelrandtermSI), collapse=") + ("), ')')), REML=TRUE, data=dfSI), TRUE)
        ndf <- dfSI %>%        
          dplyr::mutate(fixedfit=as.numeric(model.matrix(lmemodIndex)%*%fixef(lmemodIndex))) %>%
          group_by(Line) %>%
          summarise(mFix=mean(fixedfit))
        BLUPSI[,i] <- ndf$mFix+ranef(lmemodIndex)[["Line"]][,1]  
        varcompdfIS <- data.frame(VarCorr(lmemodIndex))[, c("grp", "vcov")]
        varanceSI[i] <- varcompdfIS[varcompdfIS$grp=="Line", "vcov"]
      }
      LineM <- dfSI %>%
        select_(.dots=c("Line", input$multiresponse)) %>%
        group_by(Line) %>%
        summarise_all(mean)
      rslt$Pinv <- solve(cov(LineM[, input$multiresponse]))
      if (input$gVCOVopt=="MANOVA") {
        manovamodSI <- try(lm(formula(paste(paste("cbind(", paste(input$multiresponse, collapse=",")), 
                                            paste(") ~ ", paste(input$multimodeltermsSI, collapse="+"), sep=""))),data=dfSI), TRUE)	
        if (!is(try(Manova(manovamodSI), TRUE), 'try-error') && "Line" %in% input$multimodeltermsSI) {
          # rslt$Pinv <- solve(cov(LineM[, input$multiresponse]))
          manovaMSS <- geneticCov(manovamodSI)
          mdfSI <- sapply(model.frame(manovamodSI)[, intersect(c("Year", "Season", "Location", "Replicates"), names(model.frame(manovamodSI))), drop=FALSE], nlevels)
          tmsSI <- attr(terms(manovamodSI),"term.labels")
          nYSI <- ifelse("Year" %in% tmsSI, mdfSI["Year"], 1)
          nSSI <- ifelse("Season" %in% tmsSI, mdfSI["Season"], 1)
          nLocSI <- ifelse("Location" %in% tmsSI, mdfSI["Location"], 1)
          nRSI <- ifelse("Replicates" %in% tmsSI, mdfSI["Replicates"], 1)      
          vcovout <- manovaMSS$`Mean sum of cross products`
          vcovLine <- vcovout[unlist(lapply(strsplit(names(vcovout), ":"), function(x) "Line" %in% x))]
          vcovLine_E <- lapply(vcovLine, function(x) x - manovaMSS$`Error Covariance`)
          vcovLineG <- vcovLine_E[[1]]
          if (length(vcovLine_E) > 1) for (i in 2:length(vcovLine_E)) vcovLineG <- vcovLineG - vcovLine_E[[i]] 
          rslt$Amatrix <- vcovLineG/nYSI/nSSI/nLocSI/nRSI 
          if (any(eigen(rslt$Amatrix)$values <= 0)) {
            Amatrixpd <- nearPD(rslt$Amatrix)$mat
            rslt$Amatrix <- Amatrixpd@x
            dim(rslt$Amatrix) <- Amatrixpd@Dim
          }
          
          rslt$Amatrix <- cor2covSI(cov2cor(rslt$Amatrix), varanceSI)
        }
      }else{
        if (!is_empty(input$matrixArea))
          readInMatrix <- read.table(text = input$matrixArea, sep = "", as.is = TRUE, fill = TRUE) %>%
            as.matrix()
        if (nrow(readInMatrix)==length(input$multiresponse) && ncol(readInMatrix)==length(input$multiresponse))
          rslt$Amatrix <- readInMatrix		
      }
      
      rslt$coefSI <- rslt$Pinv%*%rslt$Amatrix%*%weightSI
      colnames(rslt$coefSI) <- "Coefficients"
      rslt$BLUPSIout <- as.data.frame(BLUPSI) %>%
        dplyr::mutate(Line=rownames(BLUPSI), I = as.vector(BLUPSI%*%rslt$coefSI)) %>%
        select_(.dots=c("Line", colnames(BLUPSI), "I")) %>%
        arrange(-I)
      FSdeltaG <- apply(rslt$BLUPSIout[, colnames(BLUPSI)], 2, function(x) kV[input$pSI]*cor(x, rslt$BLUPSIout$I)*sd(x))
      BLUPSIm <- apply(rslt$BLUPSIout[, colnames(BLUPSI)], 2, mean)
      FSdeltaGPct <- (FSdeltaG/BLUPSIm)*100
      HSdeltaG <- 0.5*FSdeltaG
      HSdeltaGPct <- 0.5*FSdeltaGPct
      # rslt$deltaG <- rbind(HS=HSdeltaG, FS=FSdeltaG)
      rslt$deltaG <- rbind(HS=paste(round(HSdeltaG, 3),paste("(", paste(round(HSdeltaGPct,2), "%", sep=""), ")", sep="")), 
                           Inbred_lines_and_FS=paste(round(FSdeltaG, 3),paste("(", paste(round(FSdeltaGPct,2), "%", sep=""), ")", sep="")))
      colnames(rslt$deltaG) <- names(FSdeltaG)
      
    }
  })
})

###### Output

output$pcabiplot <- renderPlot({
  if (length(input$multiresponse) > 2 && length(input$plot_of_pcs) == 2) {    
	pc_min <- min(as.numeric(input$plot_of_pcs))
	pc_max <- min(max(as.numeric(input$plot_of_pcs)), length(input$multiresponse))	
	if (input$mpcalabels %in% names(rslt$modeldata)) mpca_varlabel <- rslt$modeldata[, input$mpcalabels] else mpca_varlabel <- NULL
    if (input$colgrp=="NULL") {
      pcaTraits(Y=as.matrix(rslt$modeldata[, input$multiresponse]), choices = c(pc_min, pc_max), var.axes =input$mpcavar, alpha=input$mpcaalpha, labels = mpca_varlabel, labels.size = input$mpcalabelsize, varname.size = input$mpcavarnsize, varname.adjust = input$mpcavarnadj)
    }else{
      pcaTraits(Y=as.matrix(rslt$modeldata[, input$multiresponse]), grFactor = rslt$modeldata[, input$colgrp], choices = c(pc_min, pc_max), labels = mpca_varlabel, labels.size = input$mpcalabelsize, varname.size = input$mpcavarnsize, varname.adjust = input$mpcavarnadj, ellipse=input$ellipse, ellipse.prob=input$mpcaellipseProb, var.axes =input$mpcavar, alpha=input$mpcaalpha)
    }
  }
}, height = 600, width = 600)

output$matrixplot <- renderPlot({
  if (length(input$multiresponse)>2) {
    if (input$colgrp=="NULL") pairs.panels(rslt$modeldata[, input$multiresponse], pch=21)
    else pairs.panels(rslt$modeldata[, input$multiresponse], pch=21, bg=colors_pc[rslt$modeldata[, input$colgrp]])      
  }
}, height = 600, width = 600)

output$cormatrix <- renderPrint({
  if (length(input$multiresponse)>2) {
    rcorrOut <- rcorr(as.matrix(rslt$modeldata[, input$multiresponse]))
    if (length(unique(as.vector(rcorrOut$n)))==1) rcorrOutN <- unique(as.vector(rcorrOut$n)) else rcorrOutN <- rcorrOut$n
    cat("Pearson Correlation Matrix:\n")
    print(round(rcorrOut$r, 2))
    cat("\np-value Matrix:\n")
    print(as.table(round(rcorrOut$P, 4)))
    cat("\nNumber of Observations for Calculation:\n")
    print(rcorrOutN)
  }
})

output$vcovdecompose <- renderPrint({
  if (is.null(input$multirun) || input$multirun == 0)
    return()
  isolate({
    if (length(input$multiresponse)>=2) {              
      withProgress(message = 'Fitting a MANOVA ...', value = 0.1, {
        mod <- try(lm(formula(paste(paste("cbind(", paste(input$multiresponse, collapse=",")), 
                                    paste(") ~ ", paste(input$multimodelterms, collapse="+"), sep=""))),data=rslt$modeldata), TRUE)										
        if (!is(try(Manova(mod), TRUE), 'try-error')) {
          
          rslt$manovaout <- geneticCov(mod)
          if ("Line" %in% input$multimodelterms){          
            mdfSI <- sapply(model.frame(mod)[, intersect(c("Year", "Season", "Location", "Replicates"), names(model.frame(mod))), drop=FALSE], nlevels)
            tmsSI <- attr(terms(mod),"term.labels")
            nYSI <- ifelse("Year" %in% tmsSI, mdfSI["Year"], 1)
            nSSI <- ifelse("Season" %in% tmsSI, mdfSI["Season"], 1)
            nLocSI <- ifelse("Location" %in% tmsSI, mdfSI["Location"], 1)
            nRSI <- ifelse("Replicates" %in% tmsSI, mdfSI["Replicates"], 1)          
            vcovout <- rslt$manovaout$`Mean sum of cross products`
            vcovLine <- vcovout[unlist(lapply(strsplit(names(vcovout), ":"), function(x) "Line" %in% x))]
            vcovLine_E <- lapply(vcovLine, function(x) x - rslt$manovaout$`Error Covariance`)
            vcovLineG <- vcovLine_E[[1]]
            if (length(vcovLine_E) > 1) for (i in 2:length(vcovLine_E)) vcovLineG <- vcovLineG - vcovLine_E[[i]]
            Linevocv <- vcovLineG/nYSI/nSSI/nLocSI/nRSI
            if (any(eigen(Linevocv)$values <= 0)) Linevocv <- nearPD(Linevocv)$mat
            rslt$manovaout$'Variance-covariance matrix of Line' <- Linevocv
            rslt$manovaout$'Correlation matrix of Line' <- cov2cor(Linevocv)
          }
          print(rslt$manovaout)
        }
      })
    }
  })
})

output$ISsummary <- renderPrint({
  if (is.matrix(rslt$Pinv)) {
    cat("Inverse of phenotypic variance-covariance matrix:\n\n")
    print(rslt$Pinv)
  }else NULL
  
  if (is.matrix(rslt$Amatrix)) {
    cat("\nAdditive genetic variance-covariance matrix:\n\n")	
    print(rslt$Amatrix)    
  }else NULL
  
  if (!is.null(rslt$coefSI)) {
    cat("\nSmith-Hazel selection index:\n\n")
    print(rslt$coefSI)  
  }else NULL
  
  if (!is.null(rslt$deltaG) && input$deltaGSI) {
    cat("\nPredicted Gains of Individual Traits:\n\n")
    print(rslt$deltaG)  
  }else NULL
  
  if (is.data.frame(rslt$BLUPSIout)) {
    cat("\nThe genetic worth (I) of an individual:\n\n")
    print(rslt$BLUPSIout)  
  }else NULL  
})

######################### Pattern analysis panel #########################  

###### sever interface

output$pattern_ui <- renderUI({
  if (!is_empty(input$selecteddata)) rslt$patterndata <- r_data[[input$selecteddata]] else rslt$patterndata <- NULL 
  if (all(unique(table(rslt$patterndata$Line))==1)) { 
    wellPanel(
      h4("Pattern Analysis"),  			   
      selectInput(inputId = "patternVariables", label = "Select Variables:", choices = names(rslt$patterndata)[sapply(rslt$patterndata, is.numeric)],
                  multiple = TRUE, selectize = FALSE), 
      checkboxInput('patternstd', "Using standardized data?", value=TRUE),
      br(),
      actionButton("patternRun", "Run", class = "btn-success btn-sm"),
      conditionalPanel(condition="input.nav_DeltaGen=='Pattern Analysis' & input.patternAnalysisPanel==1113",
                       h4("Cluster output options"),
                       checkboxInput("pclustergrp", "Show cluster groups?", TRUE),
                       checkboxInput("prowlab", "Show row label in heatmap?", FALSE),
                       checkboxInput("pcollab", "Show column label in heatmap?", FALSE),
                       sliderInput("cutTree", "Setup cutting tree point:", min = 0, max = 1, value = 0)
      ), 
      conditionalPanel(condition="input.nav_DeltaGen=='Pattern Analysis' & input.patternAnalysisPanel==1114",
                       h4("PCA biplot options"),  
                       checkboxInput("ppcasumry", "Show PCA summary?", FALSE),
					   selectizeInput('plot_of_pattern_pcs', 'Plot of PCs:', choices =c("PC1"=1, "PC2"=2, "PC3"=3, "PC4"=4), selected=c("PC1"=1, "PC2"=2), 
                     multiple = TRUE, options = list(minItems = 2, maxItems = 2, plugins = list('remove_button', 'drag_drop'))), 					 
                       selectInput(inputId = "patternlabels", label = "Label Variable:", choices = c("NULL", "RowNames", names(rslt$patterndata))),
                       conditionalPanel(condition="input.patternlabels!='NULL'",
                                        sliderInput("ppcalabelsize", "Label size:", min = 1, max = 15, value = 3, step = 0.5)),
                       conditionalPanel(condition="input.patternlabels=='NULL'",				  
                                        sliderInput("ppcaalpha", "Dot transparency:", min = 0, max = 1, value = 1, step = 0.1)),
                       checkboxInput("ppcavar", "Show variable vectors?", TRUE),
                       conditionalPanel(condition="input.ppcavar",
                                        sliderInput("ppcavarnsize", "Variable name size:", min = 1, max = 20, value = 3, step = 0.5),
                                        sliderInput("ppcavarnadj", "Variable name adjust:", min = -5, max = 3, value = 2, step = 0.5)
                       ),
                       checkboxInput("ppcaellipse", "Show ellipses?", TRUE),
                       conditionalPanel(condition="input.ppcaellipse",
                                        sliderInput("ppcaellipseProb", "Confidence of ellipse:", min = 0.68, max = 0.95, value = 0.68, step=0.01)
                       )					 
      )      
    ) 
  }else{
    wellPanel(
      h5("The data must be one observation per Line for pattern analysis!")
    )
    
  }
}) # end of output$pcabiplot_ui 

observeEvent(input$patternRun, {
  withProgress(message = 'Running pattern analysis ...', value = 0.1, {
    if (length(input$patternVariables) > 2 && all(unique(table(rslt$patterndata$Line))==1)) {
      mat <- as.matrix(rslt$patterndata[, input$patternVariables])
      if (any(is.na(mat))) mat <- naclsImpute(mat, bnclass=40)    
      if (input$patternstd) rslt$mat=scale(mat, center=T, scale=T)  
      rslt$grp <- cimN(rslt$mat, clust.method = c("ward.D", "ward.D"), color=colorRampPalette(c("blue","white", "red"))(25))$hgroup
    }
  })
})

output$mpatternheat <- renderPlot({
  if (is.matrix(rslt$mat)) {
    withProgress(message = 'Drawing heatmap ...', value = 0.1, {
      cimN(rslt$mat, row.names = input$prowlab, col.names = input$pcollab,
           clust.method = c("ward.D", "ward.D"),
           symkey=F, cut.tree = c(input$cutTree, 0),
           color=colorRampPalette(c("blue","white", "red"))(25))
      
    })
  }
}, height = 600, width = 600)


output$mpatterngroupTB = renderRHandsontable({
  if (is.vector(rslt$grp) && input$pclustergrp) {
    #groupTB <- distinct(data.frame(Cluster=rslt$grp, Line=rslt$patterndata$Line)) 
    
    groupTB <- distinct(cbind(Cluster=rslt$grp, rslt$patterndata)) %>%
      dplyr::select(c("Cluster", "Line", input$patternVariables))
    
    rhandsontable(groupTB, height = 500, width=300, rowHeaders = NULL, readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(columnSorting = TRUE, format="0") %>%
      hot_rows(fixedRowsTop = 1)%>%
      hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                                             encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))	   					   
  }					   
})

output$mpatterngroupSumry = renderPrint({
  if (is.vector(rslt$grp) && input$pclustergrp) {
    rslt$groupTBM <- distinct(cbind(Cluster=rslt$grp, rslt$patterndata)) %>%
      dplyr::select(c("Cluster", input$patternVariables)) %>%			   
      group_by(Cluster) %>%
      summarise_all(list(~mean(.), ~sd(.)), na.rm = TRUE) %>%
      ungroup() %>%
      data.frame() 
    cat("\nCluster mean and sd for each variable:\n\n")	
    print(rslt$groupTBM)    					   
  }					   
})

observeEvent(input$plt_cluster_meanRun, {
  if (is.data.frame(rslt$groupTBM)) {   
    rslt$cluster_meanPlotOut <- rslt$groupTBM %>% 
      dplyr::select(c("Cluster", ends_with("_mean"))) %>% 
      pivot_longer(!Cluster, names_to = "Location", values_to = "Cluster_Means") %>%
      group_by(Location) %>%
      dplyr::mutate(Location_mean = mean(Cluster_Means, na.rm = TRUE)) %>% 
      ungroup() %>% 
      dplyr::mutate(Location=sub("_mean$", "", Location)) %>% 
      arrange(Location_mean) %>% 
      dplyr::mutate(Location=factor2(Location), Cluster=factor(Cluster)) %>%
      qplot(x=Location, y=Cluster_Means, data=., group=Cluster, col=Cluster, 
            main=paste("\n", input$cluster_meanM, "\n", sep=""), 
            xlab = paste("\n", input$cluster_meanX, sep=""),
            ylab = paste(input$cluster_meanY, "\n", sep="") 
      )+
	  geom_point(size=1.2)+
      geom_line(size=0.96)
     #  geom_point(aes(x=Location, y=Location_mean), size=3, color="blue")+
      # scale_color_viridis(discrete = TRUE)+
    #  blanck_theme %>%	  
  }
})

output$cluster_meanPlot <- renderPlot({
    blanck_theme <- theme(plot.title = element_text(hjust = 0.5),
                          axis.text = element_text(size = 12, color = "black"), 
                          axis.title = element_text(size = 14), panel.background = element_rect(fill = "white"), 
                          panel.grid = element_line(linetype = "blank"), axis.line = element_line(linetype = "solid"))
						  
  if (inherits( rslt$cluster_meanPlotOut, "ggplot")) {
    	if (input$cluster_meanEnI) rslt$cluster_meanPlotOutN <- rslt$cluster_meanPlotOut + 
		                           geom_point(aes(x=Location, y=Location_mean), size=3, color="blue") +
                                   blanck_theme								   
		else rslt$cluster_meanPlotOutN <- rslt$cluster_meanPlotOut +
		     blanck_theme
    print(rslt$cluster_meanPlotOutN)			 
  }
}, height = 450, width = 800)

output$mpatternbiplot <- renderPlot({
  if (is.matrix(rslt$mat) && length(input$plot_of_pattern_pcs)==2) {
    withProgress(message = 'Drawing biplot ...', value = 0.1, {
      if (input$patternlabels %in% names(rslt$patterndata)) varlabel <- rslt$patterndata[, input$patternlabels] 
      else if (input$patternlabels=="RowNames") varlabel <- 1:nrow(rslt$mat)
      else varlabel <- NULL
	  patternpc_min <- min(as.numeric(input$plot_of_pattern_pcs))
	  patternpc_max <- min(max(as.numeric(input$plot_of_pattern_pcs)), ncol(rslt$mat))
      rslt$mpatternbiplotOut <- pcaTraits(rslt$mat, grFactor=factor(rslt$grp), choices=c(patternpc_min, patternpc_max), ellipse=input$ppcaellipse, ellipse.prob=input$ppcaellipseProb, labels =varlabel, labels.size = input$ppcalabelsize, var.axes =input$ppcavar, varname.size = input$ppcavarnsize,
                varname.adjust = input$ppcavarnadj, alpha=input$ppcaalpha, mtitle=NULL)
	  print(rslt$mpatternbiplotOut[[3]])
    })
  }
}, height = 600, width = 600)

output$mpatternPCA <- renderPrint({
  if (is.list(rslt$mpatternbiplotOut) && input$ppcasumry) {
    cat("\nPCA summary:\n")
    print(rslt$mpatternbiplotOut[[1]])
    cat("\nPC scores:\n")
    print(rslt$mpatternbiplotOut[[2]])
  }
})

observeEvent(input$selecteddata, {
  rslt$mat <- NULL
  rslt$groupTBM <- NULL
  rslt$grp <- NULL
  rslt$cluster_meanPlotOut <- NULL
  rslt$plt <- NULL
})
