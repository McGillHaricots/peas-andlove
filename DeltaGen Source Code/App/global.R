rm(list=ls(all=TRUE))

# Loading packages       
# library(DiGGer)
# library(agricolae)
# library(ggplot2)
# library(NbClust)
# library(Matrix)
# library(lme4)
# library(predictmeans)
# library(htmltools)
# library(tidyr)
# library(car)
# library(rmarkdown)
# library(lattice)
# library(lubridate)
# library(dplyr)
# library(Hmisc)
# library(numDeriv)
# library(psych)
# library(FactoMineR)
# library(readr)
# library(DT)
# library(gridExtra)
# library(magrittr)
# library(rpivotTable)
# library(shinythemes)
# library(shinyBS)
# library(ape)
# library(rhandsontable)

if (!require("pacman")) {
  install.packages("pacman", repos='https://cloud.r-project.org/') 
  library("pacman")
}
list_of_packages = c("remotes", "R.oo", "predictmeans",	 "agricolae",	"ape",	"car", "plyr", "dplyr",	"DT",	"FactoMineR",	"ggplot2",	"gridExtra",	"Hmisc",	"htmltools",	"lattice",	"lme4",	"lubridate",	"magrittr",	"Matrix",	"NbClust",	"numDeriv",	"psych",	"readr",	"rhandsontable",	"rmarkdown",	"rpivotTable",	"scales", "shinyBS",	"shinythemes",	"tidyr")
pacman::p_load(list_of_packages, character.only = TRUE)

# if (!require("ggbiplot")) {
  # remotes::install_github("vqv/ggbiplot")
  # library("ggbiplot")
# }

if (!require("DiGGer")) {
  stop("You should dowload 'DiGGer' from http://www.nswdpibiom.org/austatgen/software/download.php?digger=T ")
  library("DiGGer")
}

# Loading example data sets and chinese character setting  
# Sys.setlocale(category = "LC_CTYPE", locale = "chs")
enableBookmarking(store = "url")
load("Examples.RData")

ini_data <- data.frame(
  Breeder=character(),
  Year=character(),
  Season=character(),
  Location=character(),
  Replicates=integer(),
  Row=integer(),
  Column=integer(),
  Sample=character(),
  Check=character(),
  Line=character(),
  stringsAsFactors=FALSE
)

Coefficient_inbreeding_F <- c(0, 0.5, 0.75, 0.875, 0.938, 0.969, 0.984, 0.992, 0.996, 0.998, 0.999, 1)
names(Coefficient_inbreeding_F) <- c("F1 = S0", paste("F", 2:11, sep=""), "DH")

lmsimdfNN <- data.frame(Strategy=character(), 
                        p1=double(),
                        p2=double(),		 
                        c1=double(), 
                        c2=double(), 
                        Year=integer(),	 
                        Season=integer(),		 
                        Location=integer(),
                        Rep=integer(),
                        Sample=integer(), 
                        GVar=double(), 
                        WFPVar=double(), 
                        GYVar=double(), 
                        GSVar=double(), 
                        GLVar=double(),
                        GRVar=double(),
                        GYSVar=double(),
                        GYLVar=double(),
                        GSLVar=double(),
                        EVar=double(),											 
                        deltaGCycle=double(),
                        deltaGYear=double(),
                        CostCycle=double(),
                        CostYear=double(),
                        RSD=double(),
                        stringsAsFactors=FALSE)
lmsimdfNN_1 <- lmsimdfNN

#' Adds a row at a specified index
#'
#' @param df a data frame
#' @param row a row with the same columns as \code{df}
#' @param i the index we want to add row at.
#' @return the data frame with \code{row} added to \code{df} at index \code{i}
addRowAt <- function(df, row, i) {
  # Slow but easy to understand
  if (i > 1) {
    rbind(df[1:(i - 1), ], row, df[-(1:(i - 1)), ])
  } else {
    rbind(row, df)
  }
  
}

#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}

#' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

sshhr <- function (...) suppressWarnings(suppressMessages(...))

is_empty <- function (x, empty = "") 
  if (is.null(x) || x == empty) TRUE else FALSE

is_df <- function (x) 
  if (is.null(x) || !is.data.frame(x)) FALSE else TRUE


is_string <- function (x) 
  if (is.character(x) && length(x) == 1 && !is_empty(x)) TRUE else FALSE

## drop elements from .._args variables obtained using formals
r_drop <- function(x, drop = c("dataset","data_filter")) x[-which(x %in% drop)]

## check if a variable is null or not in the selected data.frame
# not_available <- function(x)
#  if (any(is.null(x)) || (sum(x %in% varnames()) < length(x))) TRUE else FALSE
not_available <- function(x)
  if (any(is.null(x))) TRUE else FALSE

## check if a variable is null or not in the selected data.frame
available <- function(x) not_available(x) == FALSE

is_not <- function(x) is.null(x) || is.na(x)

## check if a button was NOT pressed
not_pressed <- function(x) if (is.null(x) || x == 0) TRUE else FALSE

getclass <- function (dat) 
{
  sapply(dat, function(x) class(x)[1]) %>% 
    sub("ordered", "factor", .) %>% 
    sub("POSIXct", "date", .) %>% 
    sub("POSIXlt", "date", .) %>% 
    sub("Date", "date", .) %>% 
    sub("Period", "period", .)
}

getdata <- function (dataset, vars = "", filt = "", rows = NULL, na.rm = TRUE) 
{
  filt %<>% gsub("\\n", "", .) %>% gsub("\"", "'", .)
  {
    if (!is_string(dataset)) {
      dataset
    }
    else if (exists("r_env")) {
      r_env$r_data[[dataset]]
    }
    else if (exists("r_data") && !is.null(r_data[[dataset]])) {
      if (exists("r_local")) {
        if (r_local) 
          message("Dataset ", dataset, " loaded from r_data list\n")
      }
      r_data[[dataset]]
    }
    else if (exists(dataset)) {
      d_env <- pryr::where(dataset)
      d_env[[dataset]]
    }
    else {
      message("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>% 
        stop %>% return
    }
  } %>% {
    if ("grouped_df" %in% class(.)) 
      ungroup(.)
    else .
  } %>% {
    if (filt == "") 
      .
    else filterdata(., filt)
  } %>% {
    if (is.null(rows)) 
      .
    else slice(., rows)
  } %>% 
    as.data.frame() %>% {
      if (vars[1] == "" || is.null(vars)) 
        .
      else dplyr::select(., all_of(vars))
    } %>% {
      if (na.rm) 
        na.omit(.)
      else .
    }
}

as_factor <- function (x) 
  as.factor(x)

factor2 <- function(x) factor(x, levels=unique(as.character(x)))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

visualize <- function (dataset, xvar, yvar = "", type = "hist", facet_row = ".", 
                       facet_col = ".", color = "none", fill = "none", bins = 10, 
                       smooth = 1, check = "", axes = "", alpha = 0.5, data_filter = "", 
                       shiny = FALSE, custom = FALSE)
{
  vars <- xvar
  if (!type %in% c("scatter", "line")) 
    color <- "none"
  if (type != "scatter") 
    check %<>% sub("line", "", .) %>% sub("loess", "", .)
  if (!type %in% c("scatter", "box")) 
    check %<>% sub("jitter", "", .)
  if (!type %in% c("scatter", "line")) 
    color <- "none"
  if (!type %in% c("bar", "hist", "density")) 
    fill <- "none"
  byvar <- NULL
  if (identical(yvar, "")) {
    if (!type %in% c("hist", "density")) {
      message("No yvar provided for a plot that requires a yvar")
      return(invisible())
    }
  }
  else {
    if (type %in% c("hist", "density")) {
      yvar <- ""
    }
    else {
      vars %<>% c(., yvar)
    }
  }
  if (color != "none") {
    vars %<>% c(., color)
    if (type == "line") 
      byvar <- color
  }
  if (facet_row != ".") {
    vars %<>% c(., facet_row)
    byvar <- if (is.null(byvar)) 
      facet_row
    else unique(c(byvar, facet_row))
  }
  if (facet_col != ".") {
    vars %<>% c(., facet_col)
    byvar <- if (is.null(byvar)) 
      facet_col
    else unique(c(byvar, facet_col))
  }
  if (fill != "none") {
    vars %<>% c(., fill)
    if (type == "bar") 
      byvar <- if (is.null(byvar)) 
        fill
    else unique(c(byvar, fill))
  }
  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) 
    dataset <- "-----"
  if (length(vars) < ncol(dat)) {
    fl <- strsplit(xvar, ":") %>% unlist
    cn <- colnames(dat)
    xvar <- cn[which(fl[1] == cn):which(fl[2] == cn)]
  }
  isChar <- sapply(dat, class) == "character"
  if (sum(isChar) > 0) {
    if (type == "density") 
      dat[, isChar] %<>% data.frame %>% mutate_each(funs(as.numeric))
    else dat[, isChar] %<>% data.frame %>% mutate_each(funs(as.factor))
  }
  plot_list <- list()
  
  if (type == "hist") {
    for (i in xvar) {
      hist_par <- list(alpha = alpha, position = "dodge")
      plot_list[[i]] <- ggplot(dat, aes_string(x = i))
      if ("density" %in% axes) {
        hist_par <- list(aes(y = ..density..), alpha = alpha, 
                         position = "dodge")
        plot_list[[i]] <- plot_list[[i]] + geom_density(color = "blue", 
                                                        size = 0.5)
      }
      if (!"factor" %in% class(dat[[i]])) {
        hist_par[["binwidth"]] <- dat[[i]] %>% #dplyr::select(dat, i)
          range %>% {
            diff(.)/bins
          }
      }
      plot_list[[i]] <- plot_list[[i]] + do.call(geom_histogram, 
                                                 hist_par)									 
    }
  }
  else if (type == "density") {
    for (i in xvar) {
      plot_list[[i]] <- ggplot(dat, aes_string(x = i)) + 
        if (fill == "none") 
          geom_density(adjust = smooth, color = "blue", 
                       fill = "blue", alpha = alpha, size = 1)
      else geom_density(adjust = smooth, alpha = alpha, 
                        size = 1)
    }
  }
  else if (type == "scatter") {
    itt <- 1
    gs <- if ("jitter" %in% check) 
      geom_blank()
    else geom_point(alpha = alpha)
    for (i in xvar) {
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x = i, 
                                                   y = j)) + gs
        if ("log_x" %in% axes) 
          plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", 
                                                            i))
        if ("log_y" %in% axes) 
          plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", 
                                                            j))
        if ("factor" %in% class(dat[[i]])) {
          ymax <- max(dat[[j]]) %>% {
            if (. < 0) 
              0
            else .
          }
          ymin <- min(dat[[j]]) %>% {
            if (. > 0) 
              0
            else .
          }
          plot_list[[itt]] <- plot_list[[itt]] + ylim(ymin, 
                                                      ymax)
          plot_list[[itt]] <- plot_list[[itt]] + geom_errorbar(stat = "hline", 
                                                               yintercept = "mean", width = 0.8, size = 1, 
                                                               color = "blue", aes(ymax = ..y.., ymin = ..y..))
        }
        itt <- itt + 1
      }
    }
  }
  else if (type == "line") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        if (color == "none") {
          if (is.factor(dat[[i]])) {
            tbv <- if (is.null(byvar)) 
              i
            else c(i, byvar)
            tmp <- dat %>% group_by(across(all_of(tbv))) %>% 
              dplyr::select(all_of(j)) %>% summarise_all(mean)
            plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, 
                                                       y = j)) + geom_point() + geom_line() # aes(group = 1)
          }
          else {
            plot_list[[itt]] <- ggplot(dat, aes_string(x = i, 
                                                       y = j)) + geom_line()
          }
        }
        else {
          if (is.factor(dat[[i]])) {
            tbv <- if (is.null(byvar)) 
              i
            else c(i, byvar)
            tmp <- dat %>% group_by(across(all_of(tbv))) %>% 
              dplyr::select(all_of(c(j, color))) %>% summarise_all(mean)
            plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, 
                                                       y = j, color = color)) + geom_point() + 
              geom_line()
          }
          else {
            plot_list[[itt]] <- ggplot(dat, aes_string(x = i, 
                                                       y = j, color = color)) + geom_line()
          }
        }
        if ("log_x" %in% axes) 
          plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", 
                                                            i))
        if ("log_y" %in% axes) 
          plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", 
                                                            j))
        itt <- itt + 1
      }
    }
  }
  else if (type == "bar") {
    itt <- 1
    for (i in xvar) {
      dat[, i] %<>% as_factor
      for (j in yvar) {
        tbv <- if (is.null(byvar)) 
          i
        else c(i, byvar)
        tmp <- dat %>% group_by(across(all_of(tbv))) %>% dplyr::select(all_of(j)) %>% 
          summarise_all(mean)
        if ("sort" %in% axes && facet_row == "." && facet_col == 
            ".") {
          tmp <- arrange(ungroup(tmp), all_of(j))
          tmp[[i]] %<>% factor(., levels = unique(.))
        }
        plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, 
                                                   y = j)) + geom_bar(stat = "identity", position = "dodge", 
                                                                      alpha = alpha)
        itt <- itt + 1
      }
    }
  }
  else if (type == "box") {
    itt <- 1
    for (i in xvar) {
      dat[, i] %<>% as_factor
      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dat, aes_string(x = i, 
                                                   y = j, fill = i)) + geom_boxplot(alpha = alpha) + 
          theme(legend.position = "none")
        itt <- itt + 1
      }
    }
  }
  if (facet_row != "." || facet_col != ".") {
    facets <- if (facet_row == ".") 
      paste("~", facet_col)
    else paste(facet_row, "~", facet_col)
    scales <- if ("scale_y" %in% axes) 
      "free_y"
    else "fixed"
    facet_fun <- if (facet_row == ".") 
      facet_wrap
    else facet_grid
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
      facet_fun(as.formula(facets), scales = scales)
  }
  if (color != "none") {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        aes_string(color = color)
  }
  if (fill != "none") {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        aes_string(fill = fill)
  }
  if ("jitter" %in% check) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, 
                                                              height = 0.1))
  }
  if ("line" %in% check) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        sshhr(geom_smooth(method = "lm", fill = "blue", alpha = 0.1, 
                          size = 0.75, linetype = "dashed"))
  }
  if ("loess" %in% check) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        sshhr(geom_smooth(span = smooth, size = 0.75, linetype = "dotdash"))
  }
  if ("flip" %in% axes) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        coord_flip()
  }
  if ("log_y" %in% axes) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        scale_y_continuous(trans = "log")
  }
  if ("log_x" %in% axes) {
    for (i in 1:length(plot_list)) plot_list[[i]] <- plot_list[[i]] + 
        scale_x_continuous(trans = "log")
  }
  
  if (custom) 
    if (length(plot_list) == 1) 
      return(plot_list[[1]])
  else return(plot_list)
  
  sshhr(multiplot(plotlist=plot_list, cols = min(length(plot_list), 2)))
}

plot_downloader <- function(plot_name, width = plot_width(),
                            height = plot_height(), pre = ".plot_", po = "dl_") {
  
  ## link and output name
  lnm <- paste0(po, plot_name)
  
  # psize <- . %>% {7 * ./650} %>% round(2)
  # fext <- . %>% tools::file_ext(.) %>% tolower
  
  ## create an output
  output[[lnm]] <- downloadHandler(
    filename = function() { paste0(plot_name, ".png") },
    content = function(file) {
      # if (fext(file) == "svg") svg(file=file, width = psize(width), height = psize(height))
      # if (fext(file) == "pdf") pdf(file=file, width = psize(width), height = psize(height))
      
      ## needed to get the image quality at the same level as shiny
      pr <- session$clientData$pixelratio
      if (is.null(pr) || pr < 1) pr <- 1
      png(file=file, width = width*pr, height = height*pr, res=72*pr)
      print(get(paste0(pre, plot_name))())
      dev.off()
    }
  )
  downloadLink(lnm, "", class = "fa fa-download alignright")
}

# Function for get info from data
datainfo <- function(x) {
  Variable <- names(x)
  Class <- sapply(x, function(y) class(y)[1])
  No.of.level.or.unique.values<- sapply(x, function(x) length(unique(na.omit(x))))
  datainfo <- data.frame(Variable, Class, No.of.level.or.unique.values)
  row.names(datainfo) <- NULL
  return(datainfo)
} 

# Function for Stats

No.Missing <- function (x) sum(is.na(x))
FirstQuartile <- function(x) quantile(x, 0.25)
ThirdQuartile <- function(x) quantile(x, 0.75)
se <- function(x) sqrt(var(x)/length(x))

# Bypass the checks to let lmer fit overparametrized models

options(lmerControl=list(check.nobs.vs.rankZ = "warning",
                         check.nobs.vs.nlev = "warning",
                         check.nobs.vs.nRE = "warning",
                         check.nlev.gtreq.5 = "warning",
                         check.nlev.gtr.1 = "warning"))  

## Standard of random effects
## package arm
## https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/

se.ranef <- function(object) 
{
  se.bygroup <- ranef(object, condVar = TRUE)
  n.groupings <- length(se.bygroup)
  for (m in 1:n.groupings) {
    vars.m <- attr(se.bygroup[[m]], "postVar")
    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]
    names.full <- dimnames(se.bygroup[[m]])
    se.bygroup[[m]] <- array(NA, c(J, K))
    for (j in 1:J) {
      se.bygroup[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
    }
    dimnames(se.bygroup[[m]]) <- list(names.full[[1]], names.full[[2]])
  }
  return(se.bygroup)
}

## Varcomp for lmer

format.perc <- function (probs, digits) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE,
               digits = digits), "%")
}

bootFun <- function(x) {
  tnames <- function (object, diag.only = FALSE, old = TRUE, prefix = NULL) {
    pfun <- mkPfun(diag.only = diag.only, old = old, prefix = prefix)
    c(unlist(mapply(pfun, names(object@cnms), object@cnms)))
  }
  
  mkPfun <- function (diag.only = FALSE, old = TRUE, prefix = NULL) {
    local({
      function(g, e) {
        mm <- outer(e, e, paste, sep = ".")
        if (old) {
          diag(mm) <- e
        }
        else {
          mm[] <- paste(mm, g, sep = "|")
          if (!is.null(prefix))
            mm[] <- paste(prefix[2], mm, sep = "_")
          diag(mm) <- paste(e, g, sep = "|")
          if (!is.null(prefix))
            diag(mm) <- paste(prefix[1], diag(mm), sep = "_")
        }
        mm <- if (diag.only)
          diag(mm)
        else mm[lower.tri(mm, diag = TRUE)]
        if (old)
          paste(g, mm, sep = ".")
        else mm
      }
    })
  }
  
  Cv_to_Sv <- function (v, n = NULL, s = 1) {
    r <- mlist2vec(lapply(vec2mlist(v, n, symm = FALSE), function(m) cov2sdcor(tcrossprod(m)*s^2)))
    if (!missing(s))
      r <- c(r, s)
    attr(r, "clen") <- get_clen(v, n)
    r
  }
  
  mlist2vec <- function (L) {
    if (is.atomic(L))
      L <- list(L)
    n <- sapply(L, nrow)
    ff <- function(x) {
      if (all(x[upper.tri(x)] == 0))
        t(x[lower.tri(x, diag = TRUE)])
      else t(x)[lower.tri(x, diag = TRUE)]
    }
    r <- unlist(lapply(L, ff))
    attr(r, "clen") <- n
    r
  }
  
  vec2mlist <- function (v, n = NULL, symm = TRUE) {
    n <- get_clen(v, n)
    s <- split(v, rep.int(seq_along(n), n * (n + 1)/2))
    m <- mapply(function(x, n0) {
      m0 <- diag(nrow = n0)
      m0[lower.tri(m0, diag = TRUE)] <- x
      if (symm)
        m0[upper.tri(m0)] <- t(m0)[upper.tri(m0)]
      m0
    }, s, n, SIMPLIFY = FALSE)
    m
  }
  
  cov2sdcor <-  function (V) {
    p <- (d <- dim(V))[1L]
    if (!is.numeric(V) || length(d) != 2L || p != d[2L])
      stop("'V' is not a square numeric matrix")
    sd <- sqrt(diag(V))
    Is <- 1/sd
    r <- V
    r[] <- Is * V * rep(Is, each = p)
    diag(r) <- sd
    r
  }
  
  get_clen <- function (v, n = NULL) {
    if (is.null(n)) {
      if (is.null(n <- attr(v, "clen"))) {
        n <- (sqrt(8 * length(v) + 1) - 1)/2
      }
    }
    n
  }
  
  th <- getME(x, "theta")
  nvec <- sapply(getME(x, "cnms"), length)
  scaleTh <- (isLMM(x) || isNLMM(x))
  useSc <- as.logical(x@devcomp$dims["useSc"])
  tn <- tnames(x, old = FALSE, prefix = c("var", "cor"))
  if (scaleTh) {
    ss <- setNames(Cv_to_Sv(th, n = nvec, s = sigma(x)),
                   c(tn, "Residual"))
  } else if (useSc) {
    ss <- setNames(c(Cv_to_Sv(th, n = nvec), sigma(x)),
                   c(tn, "Residual"))
  } else {
    ss <- setNames(Cv_to_Sv(th, n = nvec), tn)
  }
  anycor <- grep("^cor_", names(ss))
  if (length(anycor)!=0) varcomp <- (ss[-anycor])^2 else varcomp <- ss^2
  names(varcomp) <- sub("var_\\(Intercept\\)\\|", "", names(varcomp))
  attr(varcomp, "clen") <- NULL
  return(varcomp)  
}

varcomplmer <- function(model, level = 0.95, hnumerator=NULL, hcdenumerator=NULL, multiplier=1, nsim = 500, ncore=3)  {
  
  ss <- simulate(model, nsim = nsim, na.action = na.exclude)
  
  ffun <- function(x) {
    foo <- try(bootFun(refit(model, x)), silent = TRUE)
    if (inherits(foo, "try-error"))
      rep(NA, length.t0)
    else foo
  }
  
  t0 <- bootFun(model)
  length.t0 <- length(t0)
  
  require(parallel)
  cl <- makeCluster(ncore)
  clusterExport(cl, c(getNamespaceExports("lme4"), "bootFun", "model", "length.t0"), envir = environment())
  res <- parLapply(cl, ss, ffun)
  stopCluster(cl)
  
  t.star <- do.call(cbind, res) 
  if ((numFail <- sum(apply(is.na(t.star), 2, all))) > 0) {
    warning("some bootstrap runs failed (", numFail, "/", 
            nsim, ")")
  }
  
  if (!is.null(hnumerator) && !is.null(hcdenumerator)) {
    t0 <- c(t0, Heritability=(multiplier*t0[hnumerator])/(sum(t0*hcdenumerator)))
    t.star <- rbind(t.star, Heritability=(multiplier*t.star[hnumerator,])/apply(t.star, 2, function(x) sum(x*hcdenumerator)))   
  }
  
  perc.ci <- function (x, conf = level) {
    
    norm.inter <- function (t, alpha) {
      t <- t[is.finite(t)]
      R <- length(t)
      rk <- (R + 1) * alpha
      if (!all(rk > 1 & rk < R))
        warning("extreme order statistics used as endpoints")
      k <- trunc(rk)
      inds <- seq_along(k)
      out <- inds
      kvs <- k[k > 0 & k < R]
      tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs +
                                                         1))))
      ints <- (k == rk)
      if (any(ints))
        out[inds[ints]] <- tstar[k[inds[ints]]]
      out[k == 0] <- tstar[1L]
      out[k == R] <- tstar[R]
      not <- function(v) xor(rep(TRUE, length(v)), v)
      temp <- inds[not(ints) & k != 0 & k != R]
      temp1 <- qnorm(alpha[temp])
      temp2 <- qnorm(k[temp]/(R + 1))
      temp3 <- qnorm((k[temp] + 1)/(R + 1))
      tk <- tstar[k[temp]]
      tk1 <- tstar[k[temp] + 1L]
      out[temp] <- tk + (temp1 - temp2)/(temp3 - temp2) * (tk1 - tk)
      cbind(round(rk, 2), out)
    }
    alpha <- (1 + c(-conf, conf))/2
    qq <- norm.inter(na.omit(x), alpha)
    matrix(qq[, 2L], ncol = 2L)
  }
  
  citab <- t(apply(t.star, 1, perc.ci, conf= level))
  
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  colnames(citab) <- format.perc(a, 3)
  
  std.error <- apply(t.star, 1, sd)
  ncitab <- cbind(varcomp=t0, std.error, citab)
  return(ncitab)
}

# pcaTraits <- function(Y, grFactor=NULL, ellipse=TRUE, plab=NULL, mtitle="Biplot of multiple traits\n"){
#   require(ggbiplot)
#   if(any(is.na(Y))){
#     Y=apply(Y, 2, function(x){replace(x, is.na(x), min(x, na.rm=T))})
#   }    
#   d.pca=prcomp(Y, scale=TRUE); 
#   pvar=(d.pca$sdev)^2 / sum(d.pca$sdev^2)
#   pvar=sprintf("%1.1f%%", pvar[1:2]*100)
#   if(is.null(grFactor) || grFactor%in%c("", "NULL")){
#     # biplot(d.pca, xlab=paste("PC1:", pvar[1]), ylab=paste("PC2:", pvar[2]),
#     # scale=0, cex=.7, col=c("black","blue"), 
#     # sub="Biplot of mutilple traits")
#     ggbiplot(d.pca, choices=c(1,2), pc.biplot=T, labels=plab, labels.size=3, varname.size=5, 
#              varname.adjust=1.5, varname.abbrev=F, scale=0, obs.scale=1, var.scale=1) +
#       ggtitle(mtitle) +
#       theme_bw(base_size=15) 
#     
#   }else{ 
#     # par(mfrow=c(1,2))
#     # plot(d.pca$x, xlab=paste("PC1:", pvar[1]), ylab=paste("PC2:", pvar[2]), 
#     # col=grFactor, pch=as.integer(grFactor))
#     # biplot(d.pca, xlab=paste("PC1:", pvar[1]), ylab=paste("PC2:", pvar[2]),
#     # scale=0, cex=.7, col=c("black","blue"), 
#     # sub="Biplot of mutilple traits")		
#     ggbiplot(d.pca, choices=c(1,2),  groups=grFactor, pc.biplot=T, labels=plab, labels.size=3,
#              varname.size=5, varname.adjust=1.5, varname.abbrev=F, scale=0, obs.scale=1, 
#              var.scale=1, ellipse=ellipse, circle=F) +
#       scale_color_discrete(name = '') +
#       ggtitle(mtitle) +	
#       theme_bw(base_size=15) +
#       theme(legend.direction = 'horizontal', legend.position = 'top')
#   }
# }
# 

geneticCov=function(model){
  require(car)
  fit=Manova(model)
  #sources=c(fit$terms, "Error")
  DF=c(fit$df, Error=fit$error.df)
  #check
  #if(!all(names(DF)==sources)) stop ("model terms did not match") 
  
  matrices.name=names(fit$SSP)
  SSP=fit$SSP
  MSP=lapply(1:length(SSP), function(i){
    idx=which(names(DF)==matrices.name[i])
    SSP[[i]]/DF[idx]})
  names(MSP)=names(SSP)
  # COR=lapply(1:length(MSP), function(i) cov2cor(MSP[[i]]))
  # names(COR)=names(SSP)
  
  MSE=fit$SSPE/DF[which(names(DF)=="Error")]
  
  return(list("Sum of cross products"=SSP, "Mean sum of cross products"=MSP, "Error Covariance"=MSE, "Degree of freedom"=DF))
}#>>>

# ggplot2 Colour and theme like highchart
# http://www.r-bloggers.com/ggplot-with-a-highcharts-taste/

colors_hc <- c("#7CB5EC", "#313131", "#F7A35C", "#90EE7E", 
               "#7798BF", "#AAEEEE", "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353")

colors_pc <- rep(c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3",
                   "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") , 10)

theme_hc <- function(){
  theme(
    text                = element_text(size = 12),
    title               = element_text(hjust=0),
    axis.title.x        = element_text(hjust=.5),
    axis.title.y        = element_text(hjust=.5),
    panel.grid.major.y  = element_line(color='gray', size = .3),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_blank()
  )
}

tn <- function(object) {
  c(names(getME(object,"theta")),"sigma")
}

waldVar2 <- function(object) {
  ## test for/warn if ML fit?
  dd <- lme4:::devfun2(object,useSc=TRUE,signames=FALSE)
  nvp <- length(attr(dd,"thopt"))+1 ## variance parameters (+1 for sigma)
  pars <- attr(dd,"optimum")[seq(nvp)] ## var params come first
  hh <- hessian(dd,pars)
  ## factor of 2: deviance -> negative log-likelihood
  vv <- 2*solve(hh)
  nn <- tn(object)
  dimnames(vv) <- list(nn,nn)
  return(vv)
}

confintlmer <- function (object, parm, level = 0.95, ...) 
{
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(object$vcov))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


deltamethod <- function(g, mean, cov, ses = TRUE) 
{
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g)) 
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) 
    stop(paste("Covariances should be a ", n, " by ", n, " matrix"))
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}

transfun <- function(x, type) {
  switch(type,
         "log(" = function(x) exp(x),
         "sqrt(" = function (x) x^2,
         "asin(" = function(x) sin(x))
}

kV=c(0.2, 0.35, 0.5, 0.64, 0.8, 0.97, 1.16, 1.27, 1.4, 1.55, 1.76, 1.8, 1.86, 1.92, 1.99, 2.06, 2.15, 2.27, 2.42, 2.67, 2.89, 3.37)
names(kV) <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.2, 0.15, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 
               0.005, 0.001)

cor2covSI <- function (C, var = NULL) 
{
  if (is.null(var)) 
    stop("Cannot calculate covariance matrix without variances")
  if (ncol(C) != nrow(C)) 
    stop("'C' is not a square numeric matrix!")
  if (length(var) != ncol(C)) 
    stop("Length of 'var' and dimension of 'C' are not equal!")
  if (any(!is.finite(var))) 
    warning("'var' had 0 or NA entries; result is doubtful!")
  d <- sqrt(var)
  V <- outer(d, d) * C
  return(V)
}

##############################################################################
## function for pattern analysis

group.clustN <- function (cluster, k = NULL, h = NULL) 
{
  if (is.null(h) && is.null(k)) 
    return(cluster$order)
  if (!is.null(h) && h > max(cluster$height)) 
    stop("group.clust: h > max (height)")
  if (!is.null(k) && (k == 1 || k > length(cluster$height))) 
    stop("group.clust: k == 1 || k => nobs")
  if ("hclust" %in% class(cluster)) 
    clust <- cluster
  else if (inherits(cluster, "twins")) 
    clust <- as.hclust(cluster)
  else stop("group.clust: input not hclust or twins")
  merg <- clust$merge
  hite <- clust$height
  ordr <- clust$order
  nmerg <- nrow(merg)
  group <- rep(0, nmerg + 1)
  if (!is.null(k)) 
    keep <- rev(order(hite))[1:(k - 1)]
  else keep <- seq(nmerg)[hite > h]
  mark.group <- function(node, grup) {
    a <- merg[node, 1]
    b <- merg[node, 2]
    if (a < 0) 
      group[-a] <<- grup
    else mark.group(a, grup)
    if (b < 0) 
      group[-b] <<- grup
    else mark.group(b, grup)
    invisible()
  }
  grup <- 0
  find.group <- function(node) {
    a <- merg[node, 1]
    b <- merg[node, 2]
    if (match(a, keep, 0) != 0) 
      find.group(a)
    else {
      grup <<- grup + 1
      if (a > 0) 
        mark.group(a, grup)
      else group[-a] <<- grup
    }
    if (match(b, keep, 0) != 0) 
      find.group(b)
    else {
      grup <<- grup + 1
      if (b > 0) 
        mark.group(b, grup)
      else group[-b] <<- grup
    }
    invisible()
  }
  find.group(length(hite))
  grup <- match(grup, unique(grup[clust$order]))
  invisible(group)
}

reorder.hclustN <- function (x, dis, ...) 
{
  if (!is.matrix(dis)) 
    dis <- as.matrix(dis)
  merges <- x$merge
  n <- nrow(merges)
  endpoints <- matrix(0, n, 2)
  dir <- matrix(1, n, 2)
  for (i in 1:n) {
    j <- merges[i, 1]
    k <- merges[i, 2]
    if ((j < 0) && (k < 0)) {
      endpoints[i, 1] <- -j
      endpoints[i, 2] <- -k
    }
    else if (j < 0) {
      j <- -j
      endpoints[i, 1] <- j
      if (dis[j, endpoints[k, 1]] < dis[j, endpoints[k, 
                                                     2]]) 
        endpoints[i, 2] <- endpoints[k, 2]
      else {
        endpoints[i, 2] <- endpoints[k, 1]
        dir[i, 2] <- -1
      }
    }
    else if (k < 0) {
      k <- -k
      endpoints[i, 2] <- k
      if (dis[k, endpoints[j, 1]] < dis[k, endpoints[j, 2]]) {
        endpoints[i, 1] <- endpoints[j, 2]
        dir[i, 1] <- -1
      }
      else {
        endpoints[i, 1] <- endpoints[j, 1]
      }
    }
    else {
      d11 <- dis[endpoints[j, 1], endpoints[k, 1]]
      d12 <- dis[endpoints[j, 1], endpoints[k, 2]]
      d21 <- dis[endpoints[j, 2], endpoints[k, 1]]
      d22 <- dis[endpoints[j, 2], endpoints[k, 2]]
      dmin <- min(d11, d12, d21, d22)
      if (dmin == d21) {
        endpoints[i, 1] <- endpoints[j, 1]
        endpoints[i, 2] <- endpoints[k, 2]
      }
      else if (dmin == d11) {
        endpoints[i, 1] <- endpoints[j, 2]
        endpoints[i, 2] <- endpoints[k, 2]
        dir[i, 1] <- -1
      }
      else if (dmin == d12) {
        endpoints[i, 1] <- endpoints[j, 2]
        endpoints[i, 2] <- endpoints[k, 1]
        dir[i, 1] <- -1
        dir[i, 2] <- -1
      }
      else {
        endpoints[i, 1] <- endpoints[j, 1]
        endpoints[i, 2] <- endpoints[k, 1]
        dir[i, 2] <- -1
      }
    }
  }
  for (i in n:2) {
    if (dir[i, 1] == -1) {
      m <- merges[i, 1]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) 
          dir[m, ] <- -dir[m, ]
      }
    }
    if (dir[i, 2] == -1) {
      m <- merges[i, 2]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) 
          dir[m, ] <- -dir[m, ]
      }
    }
  }
  clusters <- as.list(1:n)
  for (i in 1:n) {
    j <- merges[i, 1]
    k <- merges[i, 2]
    if ((j < 0) && (k < 0)) 
      clusters[[i]] <- c(-j, -k)
    else if (j < 0) 
      clusters[[i]] <- c(-j, clusters[[k]])
    else if (k < 0) 
      clusters[[i]] <- c(clusters[[j]], -k)
    else clusters[[i]] <- c(clusters[[j]], clusters[[k]])
  }
  x1 <- x
  x1$merge <- merges
  x1$order <- clusters[[n]]
  x1
}

imageMapN <- function (mat, color, row.names, col.names, row.sideColors, col.sideColors, 
                       row.cex = NULL, col.cex = NULL, cluster, ddr, ddc, cut.tree = c(0, 0), transpose = FALSE, symkey = TRUE, keysize = c(1, 1), zoom = FALSE, title = NULL, xlab = NULL, ylab = NULL, margins = c(5, 5), lhei = NULL, lwid = NULL) 
{
  if (isTRUE(symkey)) {
    max.mat = max(abs(mat), na.rm = TRUE)
    min.mat = -max.mat
  }
  else {
    max.mat = max(mat, na.rm = TRUE)
    min.mat = min(mat, na.rm = TRUE)
  }
  if (isTRUE(transpose)) {
    mat = t(mat)
    temp = col.sideColors
    col.sideColors = row.sideColors
    row.sideColors = temp
    temp = col.names
    col.names = row.names
    row.names = temp
    if (cluster == "both") {
      temp = ddc
      ddc = ddr
      ddr = temp
    }
    else if (cluster == "column") 
      ddr = ddc
    else if (cluster == "row") 
      ddc = ddr
    lhei = NULL
    lwid = NULL
  }
  nr = nrow(mat)
  nc = ncol(mat)
  if (is.null(row.cex)) 
    row.cex = min(1, 0.2 + 1/log10(nr))
  if (is.null(col.cex)) 
    col.cex = min(1, 0.2 + 1/log10(nc))
  breaks = length(color) + 1
  breaks = seq(min.mat, max.mat, length = breaks)
  nbr = length(breaks)
  ncol = nbr - 1
  min.breaks = min(breaks)
  max.breaks = max(breaks)
  mat[mat < min.breaks] = min.breaks
  mat[mat > max.breaks] = max.breaks
  mat = t(mat)
  lmat = matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
  csc = rsc = FALSE
  if (!is.null(col.sideColors)) {
    lmat = rbind(lmat[1, ], c(NA, 3), lmat[2, ] + 1)
    if (is.null(lhei)) {
      n.csc = ncol(col.sideColors)
      lhei = c(keysize[2], 0.15 + 0.1 * (n.csc - 1), 4)
    }
    csc = TRUE
  }
  if (!is.null(row.sideColors)) {
    lmat = cbind(lmat[, 1], c(rep(NA, nrow(lmat) - 1), nrow(lmat) + 
                                2), lmat[, 2] + c(rep(0, nrow(lmat) - 1), 1))
    if (is.null(lwid)) {
      n.rsc = ncol(row.sideColors)
      lwid = c(keysize[2], 0.15 + 0.1 * (n.rsc - 1), 4)
    }
    rsc = TRUE
  }
  lmat[is.na(lmat)] = 0
  if (is.null(lhei)) 
    lhei = c(keysize[2], 4)
  if (is.null(lwid)) 
    lwid = c(keysize[1], 4)
  if (isTRUE(zoom)) {
    graphics.off()
    dev.new(pos = -1)
  }
  op = par(no.readonly = TRUE)
  on.exit(par(op))
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  #   par(mar = c(5, 2, 2, 1), cex = 0.75)
  par(mar = c(1,1,1,1), cex = 0.75)
  z = seq(0, 1, length = length(color))
  z = matrix(z, ncol = 1)
  image(z, col = color, xaxt = "n", yaxt = "n")
  box()
  par(usr = c(0, 1, 0, 1))
  lv = c(min.breaks, (3 * min.breaks + max.breaks)/4, (min.breaks + max.breaks)/2, (3 * max.breaks + min.breaks)/4, max.breaks)
  xv = (as.numeric(lv) - min.mat)/(max.mat - min.mat)
  axis(1, at = xv, labels = round(lv, 2))
  title("Color key", font.main = 1)
  par(mar = c(ifelse(cut.tree[2] != 0, 0.5, 0), 0, ifelse(!is.null(title), 5, 0), margins[2]))
  if ((cluster == "both") || (!transpose && cluster == "column") || 
      (transpose && cluster == "row")) {
    h = attr(ddc, "height")
    plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none", 
         ylim = c(cut.tree[2] * h, h))
  }
  else {
    plot(0, 0, axes = FALSE, type = "n", xlab = "", ylab = "")
  }
  if (!is.null(title)) 
    title(title, cex.main = 1.5 * op[["cex.main"]])
  if (isTRUE(csc)) {
    par(mar = c(0.5, 0, 0, margins[2]))
    sideColors = as.vector(col.sideColors)
    img = matrix(c(1:(n.csc * nc)), ncol = n.csc, byrow = FALSE)
    image(1:nc, 1:n.csc, img, col = sideColors, axes = FALSE, 
          xlab = "", ylab = "")
    abline(h = 1:(n.csc - 1) + 0.5, lwd = 2, col = ifelse(par("bg") == 
                                                            "transparent", "white", par("bg")))
  }
  par(mar = c(margins[1], 0, 0, ifelse(cut.tree[1] != 0, 0.5, 
                                       0)))
  if ((cluster == "both") || (cluster == "row" & !transpose) || 
      (cluster == "column" & transpose)) {
    h = attr(ddr, "height")
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none", 
         xlim = c(h, cut.tree[1] * h))
  }
  else {
    plot(0, 0, axes = FALSE, type = "n", xlab = "", ylab = "")
  }
  if (isTRUE(rsc)) {
    par(mar = c(margins[1], 0, 0, 0.5))
    n.rsc = ncol(row.sideColors)
    r.sideColors = row.sideColors[, n.rsc:1]
    sideColors = as.vector(r.sideColors)
    img = matrix(1:(n.rsc * nr), nrow = n.rsc, byrow = TRUE)
    image(1:n.rsc, 1:nr, img, col = sideColors, axes = FALSE, 
          xlab = "", ylab = "")
    abline(v = 1:(n.rsc - 1) + 0.5, lwd = 2, col = ifelse(par("bg") == 
                                                            "transparent", "white", par("bg")))
  }
  par(mar = c(margins[1], 0, 0, margins[2]))
  image(1:nc, 1:nr, mat, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
          c(0, nr), axes = FALSE, xlab = "", ylab = "", col = color, 
        breaks = breaks)
  axis(1, 1:nc, labels = col.names, las = 2, line = -0.5, tick = 0, 
       cex.axis = col.cex)
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1] - 1.25)
  axis(4, 1:nr, labels = row.names, las = 2, line = -0.5, tick = 0, 
       cex.axis = row.cex)
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2] - 1.25)
  flag1 = flag2 = FALSE
  if (isTRUE(zoom)) {
    nD = dev.cur()
    zone = FALSE
    repeat {
      dev.set(nD)
      repeat {
        loc = locator(1, type = "n")
        if (is.null(loc) & zone == TRUE) 
          break
        if (is.null(loc) & !isTRUE(flag1)) 
          break
        flag1 = TRUE
        x1 = round(loc[[1]] - 0.5) + 0.5
        y1 = round(loc[[2]] - 0.5) + 0.5
        if (!(x1 < 0 | x1 > nc + 0.5 | y1 < 0 | y1 > 
              nr + 0.5)) 
          break
      }
      if (is.null(loc) & zone == TRUE) 
        break
      if (!is.null(loc) & zone == TRUE) {
        rect(xleft.old, ybottom.old, xright.old, ytop.old, 
             border = "white")
        points(x1.old, y1.old, type = "p", pch = 3, cex = 2, 
               col = "white")
      }
      if (is.null(loc) & zone == FALSE) {
        break
      }
      else {
        x1.old = x1
        y1.old = y1
        points(x1, y1, type = "p", pch = 3, cex = 2)
        repeat {
          loc = locator(1, type = "n")
          if (is.null(loc) & zone == TRUE) 
            break
          if (is.null(loc) & !isTRUE(flag2)) 
            break
          flag2 = TRUE
          x2 = round(loc[[1]] - 0.5) + 0.5
          y2 = round(loc[[2]] - 0.5) + 0.5
          if (!(x2 < 0 | x2 > nc + 0.5 | y2 < 0 | y2 > 
                nr + 0.5)) {
            zone = TRUE
            break
          }
        }
        if (is.null(loc) & zone == TRUE) 
          break
        if (is.null(loc) & !isTRUE(flag2)) 
          break
        xleft.old = min(x1, x2)
        xright.old = max(x1, x2)
        ybottom.old = min(y1, y2)
        ytop.old = max(y1, y2)
        rect(xleft.old, ybottom.old, xright.old, ytop.old)
      }
      dev.new()
      plot.par = par(no.readonly = TRUE)
      if (isTRUE(zone)) {
        xleft = xleft.old + 0.5
        ybottom = ybottom.old + 0.5
        xright = xright.old - 0.5
        ytop = ytop.old - 0.5
        nr.zoom = length(xleft:xright)
        nc.zoom = length(ybottom:ytop)
        mat.zoom = matrix(mat[xleft:xright, ybottom:ytop], 
                          nrow = nr.zoom, ncol = nc.zoom)
        rlab.zoom = col.names[xleft:xright]
        clab.zoom = row.names[ybottom:ytop]
        r.cex = min(1.2, 0.2 + 1/log10(nr.zoom))
        c.cex = min(1.2, 0.2 + 1/log10(nc.zoom))
        layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
        par(mar = c(5, 2, 2, 1), cex = 0.75)
        image(z, col = color, xaxt = "n", yaxt = "n")
        box()
        par(usr = c(0, 1, 0, 1))
        lv = c(min.breaks, (3 * min.breaks + max.breaks)/4, 
               (min.breaks + max.breaks)/2, (3 * max.breaks + 
                                               min.breaks)/4, max.breaks)
        xv = (as.numeric(lv) - min.mat)/(max.mat - min.mat)
        axis(1, at = xv, labels = round(lv, 2))
        title("Color key", font.main = 1)
        par(mar = c(ifelse(cut.tree[2] != 0, 0.5, 0), 
                    0, ifelse(!is.null(title), 5, 0), margins[2]))
        if ((cluster == "both") || (cluster == "column" & 
                                    !transpose) || (cluster == "row" & transpose)) {
          plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none", 
               xlim = c(xleft - 0.5, xright + 0.5))
        }
        else {
          plot(0, 0, axes = FALSE, type = "n", xlab = "", 
               ylab = "")
        }
        if (!is.null(title)) 
          title(title, cex.main = 1.5 * op[["cex.main"]])
        if (isTRUE(csc)) {
          par(mar = c(0.5, 0, 0, margins[2]))
          sideColors = as.vector(col.sideColors[xleft:xright, 
          ])
          img = matrix(c(1:(n.csc * nr.zoom)), ncol = n.csc, 
                       byrow = FALSE)
          image(1:nr.zoom, 1:n.csc, img, col = sideColors, 
                axes = FALSE, xlab = "", ylab = "")
          abline(h = 1:(n.csc - 1) + 0.5, lwd = 2, col = ifelse(par("bg") == 
                                                                  "transparent", "white", par("bg")))
        }
        par(mar = c(margins[1], 0, 0, ifelse(cut.tree[1] != 
                                               0, 0.5, 0)))
        if ((cluster == "both") || (cluster == "row" & 
                                    !transpose) || (cluster == "column" & transpose)) {
          plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", 
               leaflab = "none", ylim = c(ybottom - 0.5, 
                                          ytop + 0.5))
        }
        else {
          plot(0, 0, axes = FALSE, type = "n", xlab = "", 
               ylab = "")
        }
        if (isTRUE(rsc)) {
          par(mar = c(margins[1], 0, 0, 0.5))
          r.sideColors = row.sideColors[ybottom:ytop, 
                                        n.rsc:1]
          sideColors = as.vector(r.sideColors)
          img = matrix(1:(n.rsc * nc.zoom), nrow = n.rsc, 
                       byrow = TRUE)
          image(1:n.rsc, 1:nc.zoom, img, col = sideColors, 
                axes = FALSE, xlab = "", ylab = "")
          abline(v = 1:(n.rsc - 1) + 0.5, lwd = 2, col = ifelse(par("bg") == 
                                                                  "transparent", "white", par("bg")))
        }
        par(mar = c(margins[1], 0, 0, margins[2]))
        image(1:nr.zoom, 1:nc.zoom, mat.zoom, col = color, 
              breaks = breaks, axes = FALSE, xlab = "", ylab = "")
        axis(1, 1:nr.zoom, labels = rlab.zoom, las = 2, 
             line = -0.5, tick = 0, cex.axis = r.cex)
        if (!is.null(xlab)) 
          mtext(xlab, side = 1, line = margins[1] - 1.25)
        axis(4, 1:nc.zoom, labels = clab.zoom, las = 2, 
             line = -0.5, tick = 0, cex.axis = c.cex)
        if (!is.null(ylab)) 
          mtext(ylab, side = 4, line = margins[2] - 1.25)
      }
      par(plot.par)
    }
  }
  par(op)
}

cimN <- function (mat, color = NULL, row.names = TRUE, col.names = TRUE, 
                  row.sideColors = NULL, col.sideColors = NULL, row.cex = NULL, row.grpn=NULL,
                  col.cex = NULL, threshold = 0, cluster = "both", 
                  dist.method = c("euclidean", "euclidean"), 
                  clust.method = c("complete", "complete"), 
                  cut.tree = c(0, 0), transpose = FALSE, symkey = TRUE, 
                  keysize = c(1, 0.6), zoom = FALSE, title = NULL, xlab = NULL, ylab = NULL, 
                  margins = c(6, 5), lhei = NULL, lwid = NULL, comp = NULL, 
                  center = TRUE, scale = FALSE, mapping = "XY", legend = NULL, 
                  save = NULL, name.save = NULL) 
  
{
  
  require(NbClust)
  class.object = class(mat)
  arg.call = match.call()
  user.arg = names(arg.call)[-1]
  err = tryCatch(mget(names(formals()), sys.frame(sys.nframe())), 
                 error = function(e) e)
  if ("simpleError" %in% class(err)) 
    stop(err[[1]], ".", call. = FALSE)
  if (is.null(color)) 
    color = color.spectral(25)
  choices = c("both", "row", "column", "none")
  cluster = choices[pmatch(cluster, choices)]
  if (is.na(cluster)) 
    stop("'cluster' should be one of 'both', 'row', 'column' or 'none'.", 
         call. = FALSE)
  if (!is.character(clust.method) | length(as.vector(clust.method)) != 
      2) 
    stop("'clust.method' must be a character vector of length 2.", 
         call. = FALSE)
  choices = c("ward.D", "single", "complete", "average", "mcquitty", 
              "median", "centroid")
  clust.method = choices[c(pmatch(clust.method[1], choices), 
                           pmatch(clust.method[2], choices))]
  if (any(is.na(clust.method))) 
    stop("invalid clustering method.", call. = FALSE)
  if (!is.character(dist.method) | length(as.vector(dist.method)) != 
      2) 
    stop("'dist.method' must be a character vector of length 2.", 
         call. = FALSE)
  choices = c("euclidean", "correlation", "maximum", "manhattan", 
              "canberra", "binary", "minkowski")
  dist.method = choices[c(pmatch(dist.method[1], choices), 
                          pmatch(dist.method[2], choices))]
  if (any(is.na(dist.method))) 
    stop("invalid distance method.", call. = FALSE)
  if (any(!sapply(color, function(color) {
    tryCatch(is.matrix(col2rgb(color)), error = function(e) FALSE)
  }))) 
    stop("'color' must be a character vector of recognized colors.", 
         call. = FALSE)
  if (any(!sapply(row.sideColors, function(row.sideColors) {
    tryCatch(is.matrix(col2rgb(row.sideColors)), error = function(e) FALSE)
  }))) 
    stop("color names for vertical side bar must be a character vector of recognized colors.", 
         call. = FALSE)
  if (any(!sapply(col.sideColors, function(col.sideColors) {
    tryCatch(is.matrix(col2rgb(col.sideColors)), error = function(e) FALSE)
  }))) 
    stop("color names for horizontal side bar must be a character vector of recognized colors.", 
         call. = FALSE)
  if (!is.null(row.cex)) {
    if (!is.numeric(row.cex) || length(row.cex) != 1) 
      stop("'row.cex' must be a numerical value.", call. = FALSE)
  }
  if (!is.null(col.cex)) {
    if (!is.numeric(col.cex) || length(col.cex) != 1) 
      stop("'col.cex' must be a numerical value.", call. = FALSE)
  }
  if (!is.logical(transpose)) 
    stop("'transpose' must be a logical constant (TRUE or FALSE).", 
         call. = FALSE)
  if (!is.numeric(cut.tree) || length(cut.tree) != 2) 
    stop("'cut.tree' must be a numeric vector of length 2.", 
         call. = FALSE)
  else {
    if (!(all(0 <= cut.tree & cut.tree <= 1))) 
      stop("Components of 'cut.tree' must be between 0 and 1.", 
           call. = FALSE)
  }
  if (length(keysize) != 2 || any(!is.finite(keysize))) 
    stop("'keysize' must be a numeric vector of length 2.", 
         call. = FALSE)
  if (!is.logical(zoom)) 
    stop("'zoom' must be a logical constant (TRUE or FALSE).", 
         call. = FALSE)
  if (!is.numeric(margins) || length(margins) != 2) 
    stop("'margins' must be a numeric vector of length 2.", 
         call. = FALSE)
  if (!is.logical(symkey)) 
    stop("'symkey' must be a logical constant (TRUE or FALSE).", 
         call. = FALSE)
  if (!is.null(lhei)) {
    if (is.null(col.sideColors)) {
      if (length(lhei) != 2 | !is.numeric(lhei) | any(is.na(lhei))) 
        stop("'lhei' must be a numeric vector of length 2.", 
             call. = FALSE)
    }
    else {
      if (length(lhei) != 3 | !is.numeric(lhei) | any(is.na(lhei))) 
        stop("'lhei' must be a numeric vector of length 3.", 
             call. = FALSE)
    }
  }
  if (!is.null(lwid)) {
    if (is.null(row.sideColors)) {
      if (length(lwid) != 2 | !is.numeric(lwid) | any(is.na(lwid))) 
        stop("'lwid' must be a numeric vector of length 2.", 
             call. = FALSE)
    }
    else {
      if (length(lwid) != 3 | !is.numeric(lwid) | any(is.na(lwid))) 
        stop("'lwid' must be a numeric vector of length 3.", 
             call. = FALSE)
    }
  }
  xlab = as.graphicsAnnot(xlab)
  ylab = as.graphicsAnnot(ylab)
  title = as.graphicsAnnot(title)
  if (!is.numeric(threshold) | (threshold > 1) | (threshold < 
                                                  0)) 
    stop("The value taken by 'threshold' must be between 0 and 1", 
         call. = FALSE)
  if (!is.null(save)) {
    if (!save %in% c("jpeg", "tiff", "png", "pdf")) 
      stop("'save' must be one of 'jpeg', 'png', 'tiff' or 'pdf'.", 
           call. = FALSE)
  }
  if (!is.null(name.save)) {
    if (!is.character(name.save) || length(name.save) > 1) 
      stop("'name.save' must be a character.", call. = FALSE)
  }
  else {
    if (!is.null(save)) 
      name.save = paste0("cim_", gsub(".", "_", deparse(substitute(mat)), 
                                      fixed = T))
  }
  if (!is.null(save)) {
    while (dev.cur() > 2) dev.off()
    if (save == "jpeg") 
      jpeg(filename = paste0(name.save, ".jpeg"), res = 600, 
           width = 4000, height = 4000)
    if (save == "png") 
      jpeg(filename = paste0(name.save, ".png"), res = 600, 
           width = 4000, height = 4000)
    if (save == "tiff") 
      tiff(filename = paste0(name.save, ".tiff"), res = 600, 
           width = 4000, height = 4000)
    if (save == "pdf") 
      pdf(file = paste0(name.save, ".pdf"))
  }
  object.pca = c("pca", "spca", "ipca", "sipca", "mlsplsda", 
                 "splsda", "plsda")
  object.rcc = c("rcc")
  object.pls = c("pls", "spls", "mlspls")
  object.list = c("pca", "spca", "ipca", "sipca", "mlsplsda", 
                  "splsda", "plsda", "rcc", "pls", "spls", "mlspls")
  if (any(class.object == "block.splsda")) 
    stop("Please call the 'cimDiablo' function on your 'block.splsda' object", 
         call. = FALSE)
  if (!any(class.object %in% c(object.list, "matrix"))) 
    stop("'mat' has to be a matrix or one of the following object: ", 
         paste(object.list, collapse = ", "), ".", call. = FALSE)
  
  isMat = tryCatch(is.matrix(mat), error = function(e) e)
  if ("simpleError" %in% class(isMat)) 
    stop(isMat[[1]], ".", call. = FALSE)
  if (!is.matrix(mat) || !is.numeric(mat)) 
    stop("'mat' must be a numeric matrix.", call. = FALSE)
  p = nrow(mat)
  q = ncol(mat)
  if (is.logical(row.names)) {
    if (isTRUE(row.names)) 
      row.names = rownames(mat)
    else row.names = rep("", p)
  }
  else {
    row.names = as.vector(row.names)
    if (length(row.names) != p) 
      stop("'row.names' must be a character vector of length ", 
           p, ".", call. = FALSE)
  }
  if (is.logical(col.names)) {
    if (isTRUE(col.names)) 
      col.names = colnames(mat)
    else col.names = rep("", q)
  }
  else {
    col.names = as.vector(col.names)
    if (length(col.names) != q) 
      stop("'col.names' must be a character vector of length ", 
           q, ".", call. = FALSE)
  }
  if (!is.null(row.sideColors)) {
    row.sideColors = as.matrix(row.sideColors)
    if (nrow(row.sideColors) != p) 
      stop("'row.sideColors' must be a colors character vector (matrix) of length (nrow) ", 
           p, ".", call. = FALSE)
  }
  if (!is.null(col.sideColors)) {
    col.sideColors = as.matrix(col.sideColors)
    if (nrow(col.sideColors) != q) 
      stop("'col.sideColors' must be a colors character vector (matrix) of length (nrow) ", 
           q, ".", call. = FALSE)
  }
  object = mat
  if ((cluster == "both") || (cluster == "row")) {
    Rowv = rowMeans(mat)
    if (dist.method[1] == "correlation") 
      dist.mat = as.dist(1 - cor(t(as.matrix(mat)), 
                                 method = "pearson"))
    else dist.mat = dist(mat, method = dist.method[1])
    # dist.mat <- dist(mat, method="euclidean",diag=T,upper=T)
    dist.mat <- as.dist(as.matrix(dist.mat^2/ncol(mat)))
    hcr = hclust(dist.mat, method = clust.method[1])
    hcr <- reorder.hclustN(hcr, dist.mat)  # nogrp
    #rnogrp <- NbClust(diss=dist.mat, distance = NULL, min.nc = 2, max.nc = min(c(nrow(mat)-2,20)), method = "ward.D", index = "hartigan")$Best.nc[1]
    if (is.null(row.grpn=NULL))  rnogrp <- NbClust(mat, distance = "euclidean", min.nc = 2, max.nc = min(c(nrow(mat)-2,20)), method = "ward.D", index = "hartigan")$Best.nc[1]
    else rnogrp <- row.grpn
    g.mem=group.clustN(hcr, k=rnogrp)
    names(g.mem)=hcr$labels
    ddr = as.dendrogram(hcr)
    # ddr = reorder(ddr, Rowv)
    rowInd = order.dendrogram(ddr)
    object = mat[rowInd, ]
    row.names = row.names[rowInd]
    if (!is.null(row.sideColors)) 
      row.sideColors = as.matrix(row.sideColors[rowInd, ])
  }
  if ((cluster == "both") || (cluster == "column")) {
    Colv = colMeans(mat)
    if (dist.method[2] == "correlation") 
      dist.mat = as.dist(1 - cor(as.matrix(mat), method = "pearson"))
    else dist.mat = dist(t(mat), method = dist.method[2])
    
    # dist.mat <- dist(t(mat), method="euclidean",diag=T,upper=T)
    dist.mat <- as.dist(as.matrix(dist.mat^2/ncol(t(mat)))) 
    hcc = hclust(dist.mat, method = clust.method[2])
    hcc <- reorder.hclustN(hcc, dist.mat)
    ddc = as.dendrogram(hcc)
    # ddc = reorder(ddc, Colv)
    colInd = order.dendrogram(ddc)
    object = object[, colInd]
    col.names = col.names[colInd]
    if (!is.null(col.sideColors)) 
      col.sideColors = as.matrix(col.sideColors[colInd,])
  }
  res = list(mat = object, row.names = row.names, col.names = col.names, 
             row.sideColors = row.sideColors, col.sideColors = col.sideColors)
  if ((cluster == "both") || (cluster == "row")) {
    res$rowInd = rowInd
    res$ddr = ddr
    res$hgroup = g.mem
  }
  if ((cluster == "both") || (cluster == "column")) {
    res$colInd = colInd
    res$ddc = ddc
  }
  # class(res) = "cim_default"
  
  opar = par(no.readonly = TRUE)
  imageMapN(object, color = color, row.names = row.names, col.names = col.names, 
            row.sideColors = row.sideColors, col.sideColors = col.sideColors, 
            row.cex = row.cex, col.cex = col.cex, cluster = cluster, 
            ddr = ddr, ddc = ddc, cut.tree = cut.tree, transpose = transpose, 
            symkey = symkey, keysize = keysize, zoom = zoom, title = title, 
            xlab = xlab, ylab = ylab, margins = margins, lhei = lhei, 
            lwid = lwid)
  if (!is.null(legend)) {
    if (is.null(legend$x)) 
      legend$x = "topright"
    if (is.null(legend$bty)) 
      legend$bty = "n"
    if (is.null(legend$cex)) 
      legend$cex = 0.8
    
    if (is.null(legend$legend)) 
      stop("argument \"legend$legend\" is missing, with no default")
    if (is.null(legend$fill)) 
      legend$fill = legend$col
    par(mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, axes = FALSE, type = "n", xlab = "", ylab = "")
    if (!is.null(legend$title)) {
      legend(x = legend$x, y = legend$y, legend = legend$legend, 
             col = legend$col, fill = legend$fill, bty = legend$bty, 
             title = legend$title, cex = legend$cex)
    }
    else {
      legend(x = legend$x, y = legend$y, legend = legend$legend, 
             col = legend$col, fill = legend$fill, bty = legend$bty, 
             cex = legend$cex)
    }
  }
  if (any(class.object %in% object.list) & !any(class.object %in% object.pca) & mapping == "XY") res$mat.cor = object
  par(opar)
  if (!is.null(save)) 
    dev.off()
  return(invisible(res))
}

pcaTraits <- function(Y, grFactor=NULL, mtitle="Biplot of multiple traits\n", ...){
 # require(ggbiplot)
  
  if(any(is.na(Y))){
    Y=apply(Y, 2, function(x){replace(x, is.na(x), min(x, na.rm=T))})
  }    
  d.pca=prcomp(Y, center = TRUE, scale = TRUE)
  
  if(is.null(grFactor) || grFactor%in%c("", "NULL")){
    plt <- ggbiplot(pcobj=d.pca, ...) +
      ggtitle(mtitle) +
      theme_bw(base_size=15) 
  }else{ 
    plt <- ggbiplot(pcobj=d.pca, group=grFactor, ...) +
      scale_color_discrete(name = '') +
      ggtitle(mtitle) +	
      theme_bw(base_size=15) +
      theme(legend.direction = 'horizontal', legend.position = 'top')
  }
  plt <- plt+
          xlim(c(max(plt$data[,1:2]), min(plt$data[,1:2])))+
          ylim(c(max(plt$data[,1:2]), min(plt$data[,1:2])))+
          geom_vline(xintercept = 0)+
          geom_hline(yintercept = 0)
  print(plt)
  invisible(list("PCA summary"=summary(d.pca), "PCs scores"=d.pca$x[, 1:3], plt))
}

naclsImpute <- function(mat, bnclass = nrow(mat)-1){
  if(!is.matrix(mat)) stop("Input must be a matrix!")
  
  if(any(is.na(mat))) {
    dist.mat = dist(mat, method = "euclidean")
    dist.mat <- as.dist(as.matrix(dist.mat^2/ncol(mat)))
    
    if(any(is.na(dist.mat))) {
      require(ape)
      dist.mat <- as.dist(ultrametric(dist.mat)) 
    }
    #    bnclass <- NbClust(diss=dist.mat, distance = NULL, min.nc = 2, max.nc = min(c(nrow(mat)-2, 40)), method = "ward.D", index = "cindex")$Best.nc[1]
    
    hcc = hclust(dist.mat, method = "ward.D")
    hcc <- reorder.hclustN(hcc, dist.mat)
    # bnclass <- min(c(nrow(mat)-2, 40))
    
    grpm <- cutree(hcc, k = 1:bnclass)
    
    df.mat <- df.matN <- mat
    no.na <- sum(is.na(df.mat))
    bnclassN <- bnclass
    while (no.na > 0) {
      df.matNN <- as.data.frame(df.mat) %>% 
        dplyr::mutate(clustgrp=grpm[,bnclassN]) %>% 
        group_by(clustgrp) %>% 
        mutate_each(funs(ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>%
        ungroup(.) %>% 
        dplyr::select(-clustgrp) %>% 
        as.matrix(.)
      df.matN[is.na(df.matN) & !is.na(df.matNN)] <- df.matNN[is.na(df.matN) & !is.na(df.matNN)]
      bnclassN <- bnclassN - 1
      no.na <- sum(is.na(df.matN))
    }
    mat <- df.matN
    
  }else mat
  return(mat)
}

####################################
# https://github.com/vqv/ggbiplot

ggbiplot <- function(pcobj, choices = c(1, 2), scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, arrow.size=0.75, ...)
{
  # library(ggplot2)
  # library(plyr)
  # library(scales)
  # library(grid)
  
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'),
                   size=arrow.size)
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}