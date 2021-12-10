######################### Data panel #########################  ui_savePlot
###### Interface 

r_dataplot <- reactiveValues()
r_dataplot$plot_num <- 1

output$ui_viz_axes <- renderUI({
  if (is.null(input$viz_type)) return()
  ind <- 1
  viz_axes <-  c("Flip" = "flip", "Log X" = "log_x", "Log Y" = "log_y",
                 "Scale-y" = "scale_y", "Density" = "density", "Sort" = "sort")
  if (input$viz_type %in% c("line","scatter")) ind <- 1:3
  if (input$viz_type == "hist") ind <- c(ind, 5)
  if (!is_empty(input$viz_facet_row, ".") || !is_empty(input$viz_facet_col, "."))  ind <- c(ind, 4)
  if (input$viz_type == "bar" && input$viz_facet_row == "." && input$viz_facet_col == ".") ind <- c(ind, 6)
  checkboxGroupInput("viz_axes", NULL, viz_axes[ind],
                     selected = "",
                     inline = TRUE)
})

output$ui_viz_check <- renderUI({
  if (is.null(input$viz_type)) return()
  ind <- 1:3
  viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")
  checkboxGroupInput("viz_check", NULL, viz_check[ind],
                     selected = "viz_check",
                     inline = TRUE)
})

output$ui_xvar <- renderUI({
  if (input$viz_type %in% c('hist', 'density', 'scatter', 'line')) {
    xvarselect <- names(r_dataplot$plotdata)[sapply(r_dataplot$plotdata, is.numeric)]
  }else if (input$viz_type=='box') {
    xvarselect <- names(r_dataplot$plotdata)[!sapply(r_dataplot$plotdata, is.numeric)] 
  }else xvarselect <-  names(r_dataplot$plotdata)
  
  selectInput(inputId = "viz_xvar", label = "X-variable:", choices = xvarselect, multiple = TRUE, size = min(3, length(xvarselect)), selectize = FALSE)
})

output$ui_Visualize <- renderUI({
  times <- input$reset_plotinput
  div(id=letters[(times %% length(letters))+100],
      if (is_empty(input$selecteddata)) NULL
      else{
        r_dataplot$plotdata <- r_data[[input$selecteddata]] 
        viz_type <- c("Histogram" = "hist", "Density" = "density", "Scatter" = "scatter",
                      "Line" = "line", "Bar" = "bar", "Box-plot" = "box")      
        vars <- names(r_dataplot$plotdata)
        nuvars <- names(r_dataplot$plotdata)[sapply(r_dataplot$plotdata, is.numeric)]
        catvars <- names(r_dataplot$plotdata)[!sapply(r_dataplot$plotdata, is.numeric)]
        tagList(
          wellPanel(
            selectInput(inputId = "viz_type", label = "Plot-type:", choices = viz_type),                    
            conditionalPanel(condition = "input.viz_type != 'hist' & input.viz_type != 'density'",
                             selectInput(inputId = "viz_yvar", label = "Y-variable:",
                                         choices = nuvars, multiple = TRUE, size = min(3, length(nuvars)), selectize=FALSE)
            ),
            uiOutput("ui_xvar"),
            selectizeInput("viz_facet_row", "Grid by row", c(None=".", catvars)),
            selectizeInput("viz_facet_col", 'Grid by column', c(None=".", catvars)),
            conditionalPanel(condition = "input.viz_type == 'bar' |
                                      input.viz_type == 'hist' |
                                      input.viz_type == 'density'",
                             selectizeInput("viz_fill", 'Colored by', c(None="none", catvars))
            ),
            conditionalPanel(condition = "input.viz_type == 'scatter' |
                                      input.viz_type == 'line' |
                                      input.viz_type == 'box'",
                             selectizeInput("viz_color", 'Colour', c(None="none", catvars)), 
                             uiOutput("ui_viz_check")
            ),
            conditionalPanel(condition = "input.viz_type == 'hist'",
                             sliderInput("viz_bins", label = "Number of bins:",
                                         min = 1, max = 50, value = 10,
                                         step = 1)
            ),
            uiOutput("ui_viz_axes"),
            sliderInput("viz_alpha", label = "Opacity:", min = 0, max = 1,
                        value = .5, step = .01),
            wellPanel(
              div(class="row",
                  div(class="col-xs-6",
                      numericInput("viz_plot_height", label = "Plot height:", min = 100,
                                   max = 2000, step = 50,
                                   value = 600)),
                  div(class="col-xs-6",
                      numericInput("viz_plot_width", label = "Plot width:", min = 100,
                                   max = 2000, step = 50,
                                   value = 600))
              ),
              br()
            )
          )#,
          #    help_and_report(modal_title = "Visualize",
          #                    fun_name = "visualize",
          #                    help_file = inclRmd(file.path(r_path,"base/tools/help/visualize.md")))
          
        )
      }
  )
})

## list of function arguments
viz_args <- as.list(formals(visualize))

## list of function inputs selected by user
viz_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  viz_args$data_filter <- ""
  # viz_args$dataset <- input$selectplotdata
  viz_args$dataset <- r_dataplot$plotdata
  viz_args$shiny <- TRUE
  for (i in r_drop(names(viz_args)))
    viz_args[[i]] <- input[[paste0("viz_",i)]]
  viz_args
})

viz_plot_width <- reactive({
  if (is.null(input$viz_plot_width)){
    600
  }else if (input$viz_plot_width > 100) {
    input$viz_plot_width
  }else 600
})

viz_plot_height <- reactive({
  if (is.null(input$viz_plot_height)) {
    600
  } else if(input$viz_plot_height > 100) {
    lx <- if (not_available(input$viz_xvar)) 1 else length(input$viz_xvar)
    ly <- ifelse (not_available(input$viz_yvar) || input$viz_type %in% c("hist","density"),
                  1, length(input$viz_yvar))
    (lx * ly) %>%
      { if (. > 1)
        (input$viz_plot_height/2) * ceiling(. / 2)
        else
          input$viz_plot_height
      }
  }else 600
})

.visualize <- reactive({
  ## need dependency on ..
  if (not_available(input$viz_xvar)) return()
  if (input$viz_type %in% c("scatter","line", "box", "bar") && not_available(input$viz_yvar)) return()
  r_dataplot$plt <- viz_inputs() %>% { .$shiny <- TRUE; . } %>% do.call(visualize, .)
  return(r_dataplot$plt)
})

output$visualize <- renderPlot({
  if (not_available(input$viz_xvar))
    return(
      plot(x = 1, type = 'n',
           main="\nPlease select variables from the dropdown menus to create a plot",
           axes = FALSE, xlab = "", ylab = "")
    )
  
  withProgress(message = 'Making plot', value = 0, {
    try(.visualize(), silent=TRUE) %>% 
      {if (!inherits(., "try-error")) . else return(invisible())} 
    #%>% 
    # { if (grid::is.grob(.)) . else return(invisible())}
  })
}, width = viz_plot_width, height = viz_plot_height)

output$ui_savePlot<- renderUI({
  if (inherits(r_dataplot$plt, "gg")) {
    actionButton("savePlot", "Save to report", class = "btn-success btn-sm")
  }else NULL
})



observeEvent(input$savePlot, { 
  if (inherits(r_dataplot$plt, "gg")) {
    if (is.null(input$viz_yvar)) plt_name_var <- input$viz_xvar else plt_name_var <- input$viz_yvar
    r_dataplot[[paste(input$selecteddata, input$viz_type, plt_name_var, r_dataplot$plot_num, sep="_")]] <- r_dataplot$plt
	r_dataplot$plot_num <- r_dataplot$plot_num + input$savePlot
  }else NULL	
})

