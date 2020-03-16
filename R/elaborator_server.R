#' elaborator_server - Server Part of the elaborator application
#'
#'@keywords internal

elaborator_server <- function(input, output, session){

  LBTESTCD <- TRTP <- AVISIT <- SUBJIDN <- tr <- data <- nonmissing <- tot <- percentage <- trt <- PARAMCD <- nonmiss <- nr_visits <- . <- LBORRES <- LBORNRLO <- LBORNRHI <- highref <- lowref <- InQuRa <- Range <- refRange <- LBORRES.y <- LBORRES.x <- vari <- js <- NULL

  colBoxplot4 <- "#004a8a"
  colBoxplot3 <- "#0075bc"
  colBoxplot2 <- "#00b4cb"
  colBoxplot1 <- "#2fb39f"

  colDecrease <- "#47d2bc"
  colIncrease <- "#9BD7D2"

  colLines <- "#f78300"

  colQualitative1 <- "#dff2fd"
  colQualitative2 <- "#c9e1f6"
  colQualitative3 <- "#b0d5f2"
  colQualitative4 <- "#95c7ed"
  colQualitative5 <- "#78b7e5"
  colQualitative6 <- "#57a7d9"
  colQualitative7 <- "#0092cd"
  colQualitative8 <- "#0082be"
  colQualitative9 <- "#0072a9"
  colQualitative10 <- "#00639b"
  colQualitative11 <- "#005c90"
  textcol <- "#f78300"
  arrowcol <- "#f78300"

  colRvbpPos <- "#2fb39f"
  colRvbpNeg <- "#f78300"

  ColorBG <- "#E2F3F2"
  ColorApp <- "#00b4cb"
  ColorPanel <- "#11c4d4"
  ColorHighlight <- "#f6ad82"
  ColorElements <- "#e3e3e3"
  ColorFont <- "#3c3c3b"

  colChoice <- list(
    'sequential orange' = list('col' = brewer.pal(9, 'Oranges'), 'gradient' = TRUE),
    'sequential blue'   = list('col' = brewer.pal(9, 'Blues'),   'gradient' = TRUE),
    'sequential green'  = list('col' = brewer.pal(9, 'Greens'),  'gradient' = TRUE),
    'sequential grey'   = list('col' = brewer.pal(9, 'Greys'),   'gradient' = TRUE),
    'sequential purple' = list('col' = brewer.pal(9, 'Purples'), 'gradient' = TRUE),
    'sequential red'    = list('col' = brewer.pal(9, 'Reds'),    'gradient' = TRUE),

    'sequential blue - green'  = list('col' = brewer.pal(9, 'BuGn'), 'gradient' = TRUE),
    'sequential blue - purple' = list('col' = brewer.pal(9, 'BuPu'), 'gradient' = TRUE),
    'sequential green - blue'  = list('col' = brewer.pal(9, 'GnBu'), 'gradient' = TRUE),
    'sequential orange - red'  = list('col' = brewer.pal(9, 'OrRd'), 'gradient' = TRUE),
    'sequential purple - blue' = list('col' = brewer.pal(9, 'PuBu'), 'gradient' = TRUE),

    'sequential purple - blue - green'= list('col' = brewer.pal(9, 'PuBuGn'),'gradient' = TRUE),
    'sequential purple - red'         = list('col' = brewer.pal(9, 'PuRd'),  'gradient' = TRUE),
    'sequential red - purple'         = list('col' = brewer.pal(9, 'RdPu'),  'gradient' = TRUE),

    'sequential yellow - green'         = list('col' = brewer.pal(9, 'YlGn'),  'gradient' = TRUE),
    'sequential yellow - green - blue'  = list('col' = brewer.pal(9, 'YlGnBu'),'gradient' = TRUE),
    'sequential yellow - orange - brown'= list('col' = brewer.pal(9, 'YlOrBr'),'gradient' = TRUE),
    'sequential yellow - orange - red'  = list('col' = brewer.pal(9, 'YlOrRd'),'gradient' = TRUE)
  )

  boxBlotColor <- function(input, output, session, dat, name) {
    output$controls <- shiny::renderUI({
      ns <- session$ns
      tags$div(
        shinyWidgets::pickerInput(ns("col"), paste0(name), dat, multiple = FALSE,
                                  choicesOpt = list(
                                    style = c("background-color:#2fb39f !important;color: #ffffff;
                                              font-weight: bold;", "background-color:#00b4cb !important;
                                              color: #ffffff; font-weight: bold;",
                                              "background-color:#0075bc !important;color: #ffffff;
                                              font-weight: bold;", "background-color:#004a8a !important;
                                              color: #ffffff; font-weight: bold;")
                                    )), width = "100%")
    })

    return(shiny::reactive({
      validate(need(input$col, FALSE))
      dat[,input$col]
    }))
  }
  #### UI ELEMENTS ####
  #### Start Screen ####
  output$myImage <- shiny::renderUI({
    list(shiny::HTML("<img src = 'www/BAY_eLaborator_Logo.svg' alt = 'Graphic cannot be displayed' width = '682' height = '286'>"))
  })

  #### Data Upload ####
  output$select.visit <- shiny::renderUI({
    choices <- levels(df()$AVISIT)
    selected <- choices
    uiElement <- shiny::selectizeInput(inputId = 'select.visit',
                                       label = 'Visits (exclude and rearrange)',
                                       choices = choices,
                                       selected = selected,
                                       multiple = TRUE,
                                       options = list('plugins' = list('remove_button', 'drag_drop')))
  })

  output$impdata <- shiny::renderUI({
    if(input$impswitch == '*.RData file'){
      shiny::fileInput(inputId = 'file',
                       label = 'Choose RData file',
                       multiple = FALSE,
                       accept = '.RData')
    }else if(input$impswitch == '*.CSV file'){
      shiny::tagList(
        shiny::fixedRow(

          shiny::fileInput(inputId = 'csvA', label = 'Choose CSV file',
                           multiple = TRUE,
                           accept = c('text/csv',
                                      'text/comma-separated-values,text/plain',
                                      '.csv')
          ),

          shinyWidgets::prettyRadioButtons(inputId = 'sep',
                                           label = 'Select separator',
                                           inline = TRUE,
                                           choices = c('Comma' = ',',
                                                       'Semicolon' = ';',
                                                       'Tab' = '\t'),
                                           selected = ','),
          shinyWidgets::prettyRadioButtons(inputId = 'quote',
                                           label = 'Select quote',
                                           inline = TRUE,
                                           choices = c(None = '',
                                                       'Double Quote (")' = '"',
                                                       "Single Quote (')" = "'"),
                                           selected = '"'),
          shinyWidgets::prettyRadioButtons(inputId = 'dec',
                                           label = 'Select decimal character',
                                           inline = TRUE,
                                           choices= c(None = '',
                                                      'Point (.)' = '.',
                                                      'Comma (,)' = ','),
                                           selected = '.'))
      )
    }
  })

  output$select.treatments <- shiny::renderUI({
    shiny::req(df())
    choices <- levels(df()$TRTP)

    uiElement <- shiny::selectizeInput(inputId = 'select.treatments',
                                       label = 'Treatment groups (exclude and rearrange)',
                                       choices = choices,
                                       selected = choices,
                                       multiple = TRUE,
                                       options = list(
                                         'plugins' = list('remove_button', 'drag_drop')))
  })

  output$select.lab <- shiny::renderUI({
    choices <- levels(df()$LBTESTCD)
    uiElement <- shinyWidgets::pickerInput(inputId = 'select.lab',
                                           label = 'Lab parameters',
                                           choices = choices,
                                           selected = choices, multiple = TRUE,
                                           options = list(`actions-box` = TRUE,
                                                          `selected-text-format` = 'count > 0',
                                                          `count-selected-text` =  '{0} selected (of {1})',
                                                          `live-search` = TRUE,
                                                          `header` = 'Select multiple items',
                                                          `none-selected-text` = 'All dropped!'
                                           ))
  })

  output$select.toleratedPercentage <- shiny::renderUI({
    shiny::req(tolPer$val)
    shiny::sliderInput(inputId = 'select.toleratedPercentage',
                       label = 'Select percentage of tolerated missing values',
                       min = 0.25,
                       max = 0.75,
                       value = tolPer$val,
                       step = 0.05 )
  })

  output$sameaxes <- shiny::renderUI({
    shiny::req(ds2())
    shiny::checkboxInput(inputId = "sameaxes",
                         label = tags$div(tags$h5("Same scales among all treatments")),
                         value = sameax$val)
  })

  output$add_points <- shiny::renderUI({
    shiny::req(ds2())
    shiny::checkboxInput(inputId = "add_points",
                         label = tags$div(tags$h5("Show patient-specific values")),
                         value = add_points$val)
  })

  output$sortpoint <- shiny::renderUI({
    shiny::req(ds2())
    shiny::checkboxInput(inputId = "sortpoint",
                         label = "Sort patient-specific values",
                         value = TRUE)
  })

  output$con_lin <- shiny::renderUI({
    shiny::checkboxInput(inputId = "con_lin",
                         label = tags$div(tags$h5("Draw connection lines")),
                         value = con_lin$val)
  })

  output$stattest <- shiny::renderUI({
    shiny::req(ds2())
    bsplus::use_bs_popover()
    bsplus::use_bs_tooltip()
    shinyWidgets::prettyRadioButtons(inputId = "stattest",
                                     label = "",
                                     choices = c("None" = "none",
                                                 "Sign test" = "signtest",
                                                 "T-test" = "ttest"),
                                     selected = stattest$val,
                                     status = "warning")
  })

  output$ueb.stattest <- shiny::renderUI({
    renderText <- bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Test for explorative trend detection", icon("question")))),
                                           title = "Explore wheter these trends ovvur over time (can only be approved for nearly balanced treatment groups). Choose the approproate statistical test. The statistical test aims to assess whether patient-specific changes in laboratory values occur.", placement = "bottom",expanded =TRUE)
  })

  output$minimumtext <- shiny::renderUI({
    shiny::req(ds2())
    shiny::helpText(HTML('<p style="color:red"> Please select at least 2 visits! </p>'))
  })

  output$trtcompar <- shiny::renderUI({
    shiny::req((input$select.visit))
    choices  <- (input$select.visit)
    selected <- c(choices[1], choices[length(choices)])
    checkboxGroupInput(inputId = "trtcompar",
                       label = "",
                       choices = choices,
                       selected = selected)
  })

  output$ueb.trtcompar <- shiny::renderUI({
    renderText <- bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Visits to compare", icon("question")))),
                                           title = "Select which visits you want to test for the existence of a trend. If more than two visits are selected, the first selection is tested against any of the others (pairwise testing).",
                                           placement = "top",
                                           expanded = TRUE)
  })

  output$ueb.pcutoff <- shiny::renderUI({
    renderText <- bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("p-value cutoff", icon("question")))),
                                           title = "Statistical tests are performed for each lab parameter and treatment group. Backgrounds are colored if the respective p-value lies below this p-value threshold.",
                                           placement = "top",
                                           expanded = TRUE)
  })

  output$pcutoff <- shiny::renderUI({
    shiny::req(ds2())
    shiny::sliderInput(inputId = "pcutoff",
                       label = tags$div(tags$h5(" ")),
                       min = 0,
                       max = 0.2,
                       value = pval$val)
  })

  output$helptextbox <- shiny::renderUI({
    shiny::req(ds2())
    shiny::helpText(HTML('<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>'))
  })

  output$dendro1 <- shiny::renderUI({
    shiny::req(data_param())
    shiny::plotOutput(outputId = 'dendro_1', height = "250px")
  })

  output$dendro_1 <- shiny::renderPlot({
    shiny::req(pre_clust(), shiny::isolate(clustermethod$val))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- pre_clust()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(clustermethod$val))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)
      graphics::rect(xleft = graphics::grconvertX(0, 'ndc', 'user'), xright = graphics::grconvertX(1,'ndc', 'user'),
                     ybottom = graphics::grconvertY(0, 'ndc', 'user'), ytop = graphics::grconvertY(1,'ndc', 'user'),
                     border = NA, col = ColorBG, xpd = TRUE)
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro, ylab = "Distance", horiz = FALSE)
    }
  })

  shiny::observeEvent(c(input$go,input$go_visual,input$go_select,input$go_select2,input$minus_zoom1,input$plus_zoom1,
                        input$minus_zoom2,input$plus_zoom2,input$minus_zoom3,input$plus_zoom3), {

                          shiny::req(ds2())
                          output$compl <- shiny::renderPlot({

                            dat <- isolate(ds2())
                            val <- isolate(values$default)
                            if(!is.list(val)){
                              info <- NA
                            }else{
                              info <- isolate(values$default)
                            }
                            cho <- isolate(input$trtcompar)
                            signtest2 <- isolate(stattest$val)
                            if(signtest2 == "signtest"){
                              signtest <- TRUE
                            }else{
                              signtest <- FALSE
                            }
                            sortpoint <- input$sortpoint
                            labelvis <- NULL
                            sameax <- sameax$val

                            pval <- isolate(pv())
                            if(input$go != 0){
                              b.col <- isolate(box_col())
                            }else{

                              b.col <- c(colBoxplot1, colBoxplot2, colBoxplot3, colBoxplot4)

                            }
                            if (signtest2 != "none") {
                              bordcol <- isolate(border.col())
                            }else{
                              bordcol <- NULL
                            }
                            sortin <- clust()

                            sortin <- sortin[sortin %in% isolate(input$select.lab)]
                            con_lin <- con_lin$val
                            add_points <- add_points$val

                            elaborator_plot_quant_trends(dat1 = dat, signtest = signtest, Visit1 = cho[1], Visit2 = cho[-1], labcolumn = "PARAMCD",
                                        cols = b.col, pcutoff = pval, sameaxes = sameax, sortpoints = sortpoint,
                                        labelvis = labelvis, cexoutliers = 0.5, infotest = info,
                                        sortinput = sortin, bordercol = bordcol, add_points = add_points,
                                        connect_lines = con_lin)

                          }, res = isolate(zoompx$val) / 3)
                        })

  output$zoompanel1 <- shiny::renderUI({
    shiny::req(ds2())
    shiny::absolutePanel(
      id = "controls",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      top = 640,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 90,
      height = "auto",
      "Graphic size:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_zoom1", icon = icon("minus"), size = "xs", status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_zoom1", icon = icon("plus"), size = "xs", status = "warning")))
      ),
      "Panel Height:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_panel1", icon = icon("minus"), size = "xs", status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_panel1", icon = icon("plus"), size = "xs", status = "warning")))
      )
    )
  })

  output$tab1 <- shiny::renderUI({
    shiny::req(data_param())
    hpx <- data_param()$ntreat
    wpx <- data_param()$nlab
    zoompx <- zoompx$val
    panelheight <- panelheight$val
    shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight,"px"),
                     shiny::plotOutput('compl', height = paste0(hpx * zoompx,'px'), width = paste0(wpx * zoompx, 'px')
                                       , hover = clickOpts("dist_hover", clip = FALSE)
                     )
    )
  })

  output$hover <- shiny::renderPlot({
     input$go_select2
     shiny::req(isolate(ds2()), input$dist_hover)

    if (input$dist_hover$coords_css$y > 0 & input$dist_hover$coords_css$x > 0) {
      y <- input$dist_hover$coords_css$y
      x <- input$dist_hover$coords_css$x
      if(!is.null(y) && !is.null(x)){
        dat <- isolate(ds2())
        val <- isolate(values$default)
        signtest2 <- isolate(stattest$val)
        if (signtest2 == "signtest") {
          signtest <- TRUE
        } else {
          signtest <- FALSE
        }
        sortin <- clust()
        sortin <- sortin[sortin %in% isolate(input$select.lab)]
        T1 <- isolate(input$trtcompar[1])
        T2 <- isolate(input$trtcompar[-1])

        dat_filt <- dat %>%
          dplyr::filter(TRTP == dat %>%
                          dplyr::pull(TRTP) %>%
                          levels() %>%
                          .[ceiling(y / isolate(zoompx$val))], PARAMCD == sortin[ceiling(x / isolate(zoompx$val))])

        dat_filt$TRTP <- factor(dat_filt$TRTP)
        cho <- isolate(input$trtcompar)
        #cho <- tcomp()
        sortpoint <- input$sortpoint
        labelvis <- NULL
        sameax <- sameax$val
        pval <- isolate(pv())
        if (input$go != 0) {
          b.col <- box_col()
        } else {
          b.col <- c(colBoxplot1, colBoxplot2, colBoxplot3, colBoxplot4)
        }
        if (signtest2 != "none") {
          bordcol <- isolate(border.col())
        } else {
          bordcol <- NULL
        }
        add_points <- add_points$val

        con_lin <- con_lin$val

        if (!is.list(val) | length(T1) < 1 | length(T2) < 1) {
          info <- NA
        } else {
          info <- elaborator_derive_test_values(data = dat_filt, signtest = signtest, Visit1 = T1, Visit2 = T2, lab_column = "PARAMCD")
        }

        elaborator_plot_quant_trends(dat1 = dat_filt, signtest = signtest, Visit1 = cho[1], Visit2 = cho[-1], labcolumn = "PARAMCD",
                    cols = b.col, pcutoff = pval, sameaxes = sameax, sortpoints = sortpoint,
                    labelvis = labelvis, cexoutliers = 0.5, infotest = info,
                    sortinput = sortin[ceiling(x / zoompx$val)],
                    bordercol = bordcol, add_points = add_points,
                    connect_lines = con_lin)
      }
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
      rect(xleft = grconvertX(0,'ndc','user'), xright = grconvertX(1, 'ndc', 'user'),
           ybottom = grconvertY(0,'ndc','user'), ytop = grconvertY(1, 'ndc', 'user'),
           border = NA, col = ColorBG, xpd = TRUE)
      text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
  }, width = 400)

  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(id = "hoverpanel",
                         class = "modal-content",
                         fixed = TRUE,
                         draggable = TRUE,
                         HTML(paste0("<div style='background-color:", ColorBG, "'>")),
                         HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo" style="color:white;"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>
                              '),
                         top = 70, left = "auto", right = 100, bottom = "auto",
                         width = 400,height = "auto",
                         tags$div(id = 'demo',  class = "collapse",
                                  shiny::fluidRow(
                                    shiny::column(2,
                                                  shiny::plotOutput(
                                                    'hover'
                                                  ))
                                  )
                         ))
  })

  output$cex.trend <- shiny::renderUI({
    shiny::req(ds2())
    shiny::sliderInput(inputId = 'cex.trend',
                       label = '',
                       min = 0,
                       max = 5,
                       value = cex.trend$val,
                       step = 0.5)
  })

  output$method <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(inputId = 'method',
                                     label = ' ',
                                     choices = c('Interquartil Range' = 'InQuRa',
                                                 'Range' = 'Range',
                                                 'Reference Range' = 'Reference Range'),
                                     selected = method$val,
                                     status = "warning")
  })

  output$percent <- shiny::renderUI({
    shiny::sliderInput(inputId = 'percent', label = "", min = 0, max = 20, value = percent$val, step = 0.5)
  })

  output$helptextbox2 <- shiny::renderUI({
    shiny::req(ds2())
    shiny::helpText(HTML('<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>'))
  })


  output$dendro2 <- shiny::renderUI({
    shiny::req(data_param())
    shiny::plotOutput('dendro_2', height = "250px")
  })
  output$dendro_2 <- shiny::renderPlot({
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      shiny::req(pre_clust(), shiny::isolate(clustermethod$val))
      tmp <- pre_clust()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(clustermethod$val))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro2 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(xleft = graphics::grconvertX(0,'ndc','user'), xright = graphics::grconvertX(1, 'ndc', 'user'),
                     ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1, 'ndc', 'user'),
                     border = NA, col = ColorBG, xpd = TRUE)
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro2, ylab = "Distance", horiz = FALSE)
    }
  })

  shiny::observeEvent(c(input$go, input$go_visual,input$go_select,  input$go_select2, input$minus_zoom1, input$plus_zoom1,
                        input$minus_zoom2, input$plus_zoom2, input$minus_zoom3, input$plus_zoom3),{
                          output$trendPlot <- shiny::renderPlot({
                            shiny::req(Summa())
                            dat <- ds2()
                            cex <- isolate(cex.trend$val)
                            Variab <- clust()
                            Variab <- Variab[Variab %in% isolate(input$select.lab)]
                            meth <- method$val
                            perc <- percent$val/100
                            Summa  <- Summa()
                            elaborator_plot_qual_trends(dat1 = dat,
                                                 Variab,
                                                 fontsize = cex,
                                                 method = meth,
                                                 percent = perc,
                                                 color_palette = c('white',colChoice[[shiny::req(input$select.pal1)]]$col,'black'),
                                                 Summa = Summa)
                          }, res = zoompx$val / 3)
                        })

  output$tab2 <- shiny::renderUI({
    shiny::req(data_param())
    hpx <- data_param()$ntreat
    wpx <- data_param()$nlab

    zoompx <- zoompx$val
    panelheight <- panelheight$val

    shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
                     shiny::plotOutput('trendPlot', height = paste0(hpx * zoompx, 'px'), width = paste0(wpx * zoompx, 'px') , hover = clickOpts("dist_hover2", clip = FALSE))
    )
  })

  output$legend <- shiny::renderPlot({

    on_ex <- graphics::par("mfrow","oma","mar")
    on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
    graphics::plot(NULL, NULL, ylim = c(0,10), xlim = c(0,1), axes = FALSE, ylab = "", xlab = "")
    leg.x <- 0.5
    leg.y <- seq(graphics::grconvertY(0, 'npc', 'user'), graphics::grconvertY(1, 'npc', 'user'), length.out = 12)
    leg.width <- 1
    graphics::rect(xleft = leg.x - 2, xright = leg.x + 2, ybottom = leg.y[-1], ytop = leg.y[-length(leg.y)], xpd = NA,
                   col = c(c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')), border = TRUE)
    graphics::text(leg.x, leg.y[-1] - 0.5, c("0-5%", "5-10%", "10-15%", "15-20%",
                                             "20-25%", "25-30%", "30-35%", "35-40%",
                                             "40-45%", "45-50%", ">50%"),
                   col = c('black','black','black','black','black','black',
                           'white','white','white','white','white'))

  }, width = 80)
  output$legendpanel <- shiny::renderUI({
    shiny::absolutePanel(id = "legendpanel",
                         class = "modal-content",
                         fixed = TRUE,
                         draggable = TRUE,

                         top = 240,
                         left = "auto",
                         right = 50,
                         bottom = "auto",
                         width = 80,
                         height = "auto",
                         shiny::fluidRow(
                           shiny::column(2,
                                         shiny::plotOutput(
                                           'legend'
                                         ))
                         ))
  })

  output$hoverpanel2 <- shiny::renderUI({

    nvi <- data_param()$nvisit

    shiny::absolutePanel(id = "hoverpanel2",
                         class = "modal-content",
                         fixed = TRUE,
                         draggable = TRUE,
                         HTML(paste0("<div style='background-color:", ColorBG, "'>")),
                         HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo2"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>'),
                         top = 70, left = "auto", right = 100,bottom = "auto",
                         width = nvi  * 100, height = "auto",
                         tags$div(id = 'demo2',  class = "collapse",
                                  shiny::fluidRow(
                                    shiny::column(2,
                                                  shiny::plotOutput(
                                                    'hover2'
                                                  ))
                                  )
                         ))
  })

  output$zoompanel2 <- shiny::renderUI({
    shiny::req(ds2())
    shiny::absolutePanel(
      id = "controls",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      top = 650, left = "auto", right = 50, bottom = "auto",
      width = 90, height = "auto",
      "Graphic size:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_zoom2",
                                                                                     icon = icon("minus"),
                                                                                     size = "xs",
                                                                                     status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_zoom2",
                                                                                     icon = icon("plus"),
                                                                                     size = "xs",
                                                                                     status = "warning")))
      ),
      "Panel Height:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_panel2",
                                                                                     icon = icon("minus"),
                                                                                     size = "xs",
                                                                                     status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_panel2",
                                                                                     icon = icon("plus"),
                                                                                     size = "xs",
                                                                                     status = "warning")))
      )
    )
  })

  shiny::observe({
    output$hover2 <- shiny::renderPlot({
      shiny::req(ds2(), input$dist_hover2, Summa())
      if (input$dist_hover2$coords_css$y > 0 & input$dist_hover2$coords_css$x > 0) {
        dat <- ds2()
        Variab <- clust()

        Variab <- Variab[Variab %in% isolate(input$select.lab)]
        dat_filt <- dat %>%
          dplyr::filter(TRTP == dat %>%
                          dplyr::pull(TRTP) %>%
                          levels() %>%
                          .[ceiling(input$dist_hover2$coords_css$y / zoompx$val)],
                        PARAMCD == Variab[ceiling(input$dist_hover2$coords_css$x / zoompx$val)])

        dat_filt$TRTP <- factor(dat_filt$TRTP)

        Summa  <- Summa()

        meth <- method$val
        perc <- percent$val/100
        suppressWarnings(elaborator_plot_qual_trends(dat1 = dat_filt,
                                   Variab[ceiling(input$dist_hover2$coords_css$x / zoompx$val)],
                                   fontsize = 2,
                                   method = meth,
                                   percent = perc,
                                   color_palette = c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black'),
                                   Summa = Summa))
      } else {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
        rect(xleft = grconvertX(0,'ndc','user'), xright = grconvertX(1, 'ndc', 'user'),
             ybottom = grconvertY(0,'ndc','user'), ytop = grconvertY(1, 'ndc', 'user'),
             border = NA, col = ColorBG, xpd = TRUE)
        text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
        text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
      }

    }, width = data_param()$nvisit * 100)
  })

  output$cex.rvbp <- shiny::renderUI({
    shiny::sliderInput(inputId = 'cex.rvbp',
                       label = '',
                       min = 0,
                       max =5,
                       value = cex.rvbp$val,
                       step = 0.5)
  })

  output$criterion <- shiny::renderUI({
    shiny::req(ds2())
    shinyWidgets::prettyRadioButtons(inputId = "criterion",
                                     label = tags$div(tags$h4("")),
                                     choices = c("above ULN OR below LLN" = "within",
                                                 "above ULN" = "greater",
                                                 "below LLN" = "less"),
                                     selected = criterion$val, status = "warning")
  })

  output$helptextbox3 <- shiny::renderUI({
    shiny::req(ds2())
    shiny::helpText(HTML('<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>'))
  })

  output$dendro3 <- shiny::renderUI({
    shiny::req(data_param())
    shiny::plotOutput(outputId = 'dendro_3', height = "250px")
  })

  output$dendro_3 <- shiny::renderPlot({
    shiny::req(pre_clust(), shiny::isolate(clustermethod$val))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- pre_clust()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(clustermethod$val))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro3 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(xleft = graphics::grconvertX(0,'ndc','user'), xright = graphics::grconvertX(1,'ndc','user'),
                     ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1,'ndc','user'),
                     border = NA, col = ColorBG, xpd = TRUE)
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro3, ylab = "Distance", horiz = FALSE)
    }
  })

  shiny::observeEvent(c(input$go,input$go_visual,input$go_select,input$go_select2,input$minus_zoom1,input$plus_zoom1,
                        input$minus_zoom2, input$plus_zoom2, input$minus_zoom3,input$plus_zoom3),{
                          output$inoutPlot <- shiny::renderPlot({
                            dat <- ds2()

                            dat <-  subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

                            dat$PARAMCD <- dat$PARAMCD %>%
                              factor()

                            cex <- cex.rvbp$val
                            crit <- criterion$val
                            sorti <- clust()
                            sorti <- sorti[sorti %in% levels(dat$PARAMCD)]

                            elaborator_plot_ref_pattern(data = dat, fontsize = cex, criterion = crit, sorting_vector = sorti)
                          }, res = zoompx$val / 3)
  })

  output$hoverpanel3 <- shiny::renderUI({
    shiny::absolutePanel(id = "hoverpanel3",
                         class = "modal-content",
                         fixed = TRUE,
                         draggable = TRUE,
                         HTML(paste0("<div style='background-color:", ColorBG, "'>")),
                         HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo3"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>'),
                         top = 70, left = "auto", right = 100, bottom = "auto",
                         width = 400,height = "auto",
                         tags$div(id = 'demo3',  class = "collapse",
                                  shiny::fluidRow(
                                    shiny::column(2,
                                                  shiny::plotOutput(
                                                    'hover3'
                                                  ))
                                  )
                         ))
  })

  output$hover3 <- shiny::renderPlot({
    shiny::req(ds2(), input$dist_hover3)
    if (input$dist_hover3$coords_css$y > 0 & input$dist_hover3$coords_css$x > 0) {
      dat <- ds2()

      dat <-  subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

      dat$PARAMCD <- dat$PARAMCD %>%
        factor()

      sorti <- clust()
      sorti <- sorti[sorti %in% levels(dat$PARAMCD)]

      dat_filt <- dat %>%
        dplyr::filter(TRTP == dat %>%
                        dplyr::pull(TRTP) %>%
                        levels() %>%
                        .[ceiling(input$dist_hover3$coords_css$y / zoompx$val)], PARAMCD == sorti[ceiling(input$dist_hover3$coords_css$x / zoompx$val)])
      dat_filt$TRTP <- dat_filt$TRTP %>%
        factor()

      cex <- cex.rvbp$val
      crit <- criterion$val

      elaborator_plot_ref_pattern(data = dat_filt, fontsize = 2, criterion = crit, sorting_vector = sorti[ceiling(input$dist_hover3$coords_css$x / zoompx$val)])

    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
      rect(xleft = grconvertX(0, 'ndc', 'user'), xright = grconvertX(1, 'ndc', 'user'),
           ybottom = grconvertY(0, 'ndc', 'user'), ytop = grconvertY(1, 'ndc', 'user'),
           border = NA, col = ColorBG, xpd = TRUE)
      text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
  }, width = 400)

  output$zoompanel3 <- shiny::renderUI({
    shiny::req(ds2())
    shiny::absolutePanel(
      id = "controls",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      top = 640,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 90,
      height = "auto",
      "Graphic size:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_zoom3",
                                                                                     icon = icon("minus"),
                                                                                     size = "xs",
                                                                                     status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_zoom3",
                                                                                     icon = icon("plus"),
                                                                                     size = "xs",
                                                                                     status = "warning")))
      ),
      "Panel Height:",
      shiny::fluidRow(
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_panel3",
                                                                                     icon = icon("minus"),
                                                                                     size ="xs",
                                                                                     status = "warning"))),
        shiny::column(2,
                      div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_panel3",
                                                                                     icon = icon("plus"),
                                                                                     size = "xs",
                                                                                     status = "warning")))
      )
    )
  })

  output$tab3 <- shiny::renderUI({
    shiny::req(data_param())

    hpx <- data_param()$ntreat

    wpx <- data_param()$nlab2
    zoompx <- zoompx$val
    panelheight <- panelheight$val

    shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
                     shiny::plotOutput(outputId = 'inoutPlot',
                                       height = paste0(hpx * zoompx, 'px'),
                                       width = paste0(wpx * zoompx, 'px'),
                                       hover = clickOpts("dist_hover3", clip = FALSE) )
    )
  })

  output$zoompx <- shiny::renderUI({
    shiny::sliderInput(inputId = 'zoompx',
                       label = 'Zoom / Pixel ratio (px)',
                       min = 10,
                       max = 820,
                       value = zoompx$val,
                       step = 10)
  })

  output$panelheight <- shiny::renderUI({
    shiny::sliderInput(inputId = 'panelheight',
                       label = 'Change panel height',
                       min = 400,
                       max = 2400,
                       value = panelheight$val,
                       step = 100)
  })

  output$orderinglab <- shiny::renderUI({
    shiny::req(ds2())
    shinyWidgets::prettyRadioButtons(inputId = "orderinglab",
                                     label = "",
                                     choices = c("As in input" = "asinp",
                                                 "AI sorted" = "auto",
                                                 "Alphabetically" = "alphabetically"),
                                     selected = orderinglab$val, status = "warning")
  })


  output$clustermethod <- shiny::renderUI({
    shiny::req(ds2())
    choices <- c('BBURCG', 'BBWRCG', 'TSP', 'R2E', 'MDS_metric',
                 'GW_single', 'GW_complete', 'GW_average', 'GW_ward',
                 'OLO_single', 'OLO_complete', 'OLO_average', 'OLO_ward',
                 'VAT','SA', 'Spectral',
                 'SPIN_NH', 'SPIN_STS')

    shinyWidgets::pickerInput(inputId ='clusterMethod',
                              label = 'Seriation algorithm',
                              choices = sort(choices),
                              selected = clustermethod$val,
                              multiple = FALSE,
                              options = list(`live-search` = TRUE,
                                             `header` = 'Select item'
                              ))
  })

  output$select.ai.first <- shiny::renderUI({
    shiny::req((input$select.visit))
    choices  <- (input$select.visit)
    uiElement <- shinyWidgets::pickerInput(inputId = 'select.ai.first',
                                           label = 'Select first visit for change assessment',
                                           choices = choices,
                                           selected = choices[1])
  })

  output$select.ai.last <- shiny::renderUI({
    shiny::req((input$select.visit))
    choices  <- (input$select.visit)
    uiElement <- shinyWidgets::pickerInput(inputId = 'select.ai.last',
                                           label = 'Select second visit for change assessment',
                                           choices = choices,
                                           selected = choices[length(choices)])
  })

  output$select.pal1 <- shiny::renderUI({
    choices <- names(colChoice)
    uiElement <- shinyWidgets::pickerInput(inputId = 'select.pal1',
                                           label = "",
                                           choices = choices,
                                           selected = choices[1],
                                           multiple = FALSE,
                                           options = list(`live-search` = TRUE,
                                                          `style`='background: btn-warning',
                                                          `header`='Select item'
                                           ))
  })

  output$prev.pal1 <- shiny::renderPlot({
    col <- c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')
    elaborator_draw_boxplot_color(x = col)
  })

  output$manual <- shiny::renderUI({
    list(HTML("
              <h2>File Format and Structure </h2>

              <h4>File Format</h4>
              Currently, the following two file formats are supported:
              <ul>
              <li> A <b>c</b>omma <b>s</b>eparated <b>v</b>alues (CSV) file </li>
              <li> An RData file <br>
              The RData file has to include a data frame with the following variables and formats:<br>
              <ul>
              <samp>
              'data.frame':	x obs. of  y variables: <br>
              $ SUBJIDN : int   <br>
              $ AVISIT  : Factor  <br>
              $ TRTP    : Factor  <br>
              $ LBTESTCD: Factor <br>
              $ LBORRES : num   <br>
              $ LBORNRLO: chr   <br>
              $ LBORNRHI: chr  <br> </samp></ul> </li>
              </ul><br>


              <h4>File Structure</h4>

              In order to use the e<b>lab</b>orator, your laboratory data file has to include the following columns:<br>
              <ul>
              <li>  a subject identifier (called <kbd>SUBJIDN</kbd>) </li>
              <li>  the visit (called <kbd>AVISIT</kbd>) </li>
              <li>  the treatment group (called <kbd>TRTP</kbd>) </li>
              <li>  an (abbreviated) name of the laboratory parameter (called <kbd>LBTESTCD</kbd>) </li>
              <li>  the laboratory value measurement (called <kbd>LBORRES</kbd>) </li>
              <li>  the lower limit of normal (LLN) (called <kbd>LBORNRLO</kbd>) </li>
              <li>  the upper limit of normal (ULN) (called <kbd>LBORNRHI</kbd>) </li>
              </ul>

              <h5>Example</h5>
              The first 6 lines of an <i> examplary dataset </i> are shown in the following.<br>
              <ul>

              <samp>

              SUBJIDN &ensp;          AVISIT &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;           TRTP &ensp;&ensp; LBTESTCD LBORRES LBORNRLO LBORNRHI<br>
              100080021    Randomization &ensp;&ensp;&ensp;Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
              100080021    Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.3 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
              100080021 End of Treatment Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
              100080021        Follow-up &ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    16.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
              100080053    Randomization &ensp;&ensp; 1 mg &ensp;&ensp;        HGB &ensp;&ensp;&ensp;&ensp;&ensp;    14.7 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
              100080053          Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; 1 mg &ensp;&ensp;         HGB &ensp;&ensp;&ensp;&ensp;&ensp;    13.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>

              </samp>
              </ul>
              <br>





              <h4>Important points to consider</h4>
              <ul>
              <li> Missing laboratory values must be coded as NA . We recommend carefully reading the section
              on <i>Handling Missing Data</i> on the &nbsp; <i class='fa fa-info'></i> ") ,
         shiny::actionLink("link_to_tab_info", "Information"),
         HTML("-tab for correct interpretation. The section describes in detail how the e<b>lab</b>orator deals with missing data. </li>
              <li> If a laboratory parameter has no lower  or upper limit of normal, please do not insert any character in the respective cell but leave the cell empty or use the NA coding. Please do not use blank/space. </li>
              <li> Variable names must be spelled correctly as shown above (please use upper case letters). </li>
              <li> Do not use special characters for variable names or laboratory parameter names. </li>
              <li> All laboratory measurements have to be numeric. That means, do not use '+', '-', '>', '<', 'negative' etc. For example, '<1' is not a valid laboratory measurement. </li>
              <li> Please click the 'Reload App'-button before uploading another data set. </li>
              <li> <b> Please always check your data carefully before uploading it to the e<b>lab</b>orator.  </b></li>
              </ul>

              ")
         )
  })

  output$helptext1 <- shiny::renderUI({
    list(HTML("
              <h2>The Concept of the e<b>lab</b>orator for Clinical Trial Laboratory Data</h2>

              The e<b>lab</b>orator provides a <i>complete overview</i> of laboratory results for each laboratory parameter and treatment group in a matrix-like structure. All the results related to a specific laboratory parameter are shown in a specific column.  Results for a treatment group are presented within a row.
              By providing this overview, you will be able to
              identify <i>differences between treatment groups, similarities in laboratory parameters</i> and <i>frequent patterns</i>.<br> <br>

              By using various types of analyses you will be able to view  your laboratory data from different perspectives. The following three different types of analyses are available:
              <ul> <li> Quantitative trends analysis </li>
              <li> Qualitative trends analysis </li>
              <li> Reference-value based pattern analysis </li></ul>



              You can find a concept description of each type of analysis in the following. Available graphic options as well as missing data handling are described below. <br>

              <br>
              <h4><i class='fa fa-chart-line'></i><b><i> &nbsp;Quantitative Trends</i></b></h4>
              Aim: Examine changes in laboratory values across study visits and explore whether changes differ between treatment groups. <br><br>

              This type of analysis depicts the distribution of laboratory parameters in each study visit. An example is shown in Figure 1. Figure 1 shows the distribution of platelets (in giga/l) in the 2 mg dose group at all four visits during a study ('Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3'). Distributions are shown using boxplots. The middle 50% of patient-specific values fall inside the box. The median value is represented by the horizontal line crossing through the box and might be used as an indicator for the central tendency. Changes over time can be easily detected by a shift in the boxplots along the y-axis. In this example, a  decrease in platelets is observed until the End of Treatment-Visit followed by a subsequent increase between the End of Treatment-Visit and the Follow-Up 3-Visit. <br><br>

              <img src='www/Fig1.png' alt='Graphic cannot be displayed' width='300' height='300'>
              <p> <i><b>Figure 1</b>: Example plot for quantitative trends analysis. The distribution of platelets (in giga/l) is shown for the 2 mg dose group at four study visits 'Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3'. Normal range, i.e. upper limit of normal and lower limit of normal, are indicated by dotted horizontal lines. </i></p>
              Click the 'Open/Close Zoom Panel'-button and use the mouse to hover over a specific plot. The respective plot is be shown in a larger window. Further options are described below.


              <ul>
              <li> <h6><b>Same scales within lab parameter</b></h6>
              You can select wheter the y-axis range is the same as a specific laboratory parameter (default) or it has to be on the data in the respective treatment group.
              Using the same range, simplyfies the comparison between the treatment groups. Using this option, extreme outliers will not appear (due to the cut-off scale) but they are indicated by arrows.
              The values next to the arrow indicate the values of the outliers. When 'same scale among treatments' is not ticked, outliers are still shown when present.</li>

              <li> <h6><b>Patient-specific values</b></h6>
              You can permit or block patient-specific values. When permitted (default), patient-specific values will be added as circles to the boxplots.
              Note that outliers are indicated through dots and 'belong' to the boxplot (i.e. you can not block outliers). Moreover, you can choose wheter patient-specific values are sorted
              from smallest to largest (default). When 'draw connection lines' is ticked, the values for each patient measurement in the study visits are connected. </li>

              <li> <h6><b>Test for explorative trend detection</b></h6>
              You can determine for changes between study visits by applying hypothesis testing. Note that a comparison of test results between the treatment groups is only recommended for balanced treatment groups, i.e. if treatment groups are of the same size.
              When treatment groups have different sizes, comparisons between treatment groups should not be made because of a difference in the statistical power. The figure background is colored if the p-value of the respective test falls below a specified local significance level (called 'p-value cutoff').
              The background is green for decreases, and yellow for increases (see e.g. Figure 1).  <br>
              The user can choose between two types of tests: the sign test and the t-test. The sign test is recommended for general use as it does not rely on distributional assumptions. It performs a check for a specific laboratory parameter and treatment group, whether there are more patients with an increase than patients with a decrease
              between the two visits, or vice versa. Patients with consistency values (i.e. without any change in the values) are eliminated when applying the sign test. The t-test is recommended for expert users only because test assumptions should apply. <br>
              The user is required to select two visits for defining any changes. If more than two visits are selected the first visit selected is tested against each of the remaining visits (pairwise tests). <br>
              No adjustments for multiple testing are performed(tests for several treatment groups, several laboratory parameters and eventually several visits). Be aware that the multiple testing problem might lead to many mistakenly detected changes.
              <b>The use of this feature is specifically for exploration, where significant test results must be interpreted with caution.</b></li>
              </ul><br>

              <h4><i class='fab fa-buromobelexperte'></i><b><i>&nbsp; Qualitative Trends</i></b></h4>
              Aim: Study frequent time courses and check if they differ between treatment groups. <br><br>

              This type of analysis assesses frequent time courses that are described through increases/decreases between two subsequent study visits.
              A patient might, for example, have the following measurements for a specific laboratory parameter:
              Value 3.2 at Randomization Visit; 1.6 at Treatment 1-Visit; 2.9 at the End of Treatment-Visit; 2.9 at the Follow-Up 3-visit.
              The time course for this patient will be characterized as decrease (from 3.2 to 1.6) - increase (from 1.6 to 2.9) - stable (from 2.9 to 2.9). This pattern is represented as '- + ='.<br>
              In this way, the patterns / time courses for each patient can be derived and the frequency of each pattern / time course can be counted. The time courses and frequencies are transferred to a diagram-like structure. Each cell of this diagram
              represents one specific pattern / time course. The time courses are arranged in a symmetric way within the diagram. For example, the time course '+ + +' is represented in the cell in the top, while
              the 'opposite' time course '- - -' is in the cell at the bottom of the diagram. There are three entries within each of the cells: the first and second entries show the absolute and relative number of subjects in the treatment group which have the specific
              time course, and the third entry shows the respective time course. You can use the font size slider to display the entries and increase the size of the numbers. By default, the font size is set at 0, that is, all entries are blocked.
              When a time course does not occur at all (i.e. the frequency and percentage are 0), the entries of the cell are blocked by default.
              <br>
              The frequency of a time course is shown by the color of the cell.
              Darker colors reflect more frequent and lighter colors less frequent time courses. The color key is provided on the right side of Figure 2.  <br><br>
              No more than approx. 5 visits are recommended because diagrams will get too complex with increasing number of cells.<br>

              <img src='www/Fig2.png' alt='Graphic cannot be displayed' width='500' height='350'>
              <p><i><b>Figure 2</b>: Example plot for qualitative trends analysis (left) and color key (right). Frequent patterns of increases/decreases in platelets between four subsequent study visits within the 2mg dose group are shown.
              The background of the cell is colored depending on the frequency of the respective pattern (cf. color key).</i></p>

              Use the 'Open/Close Zoom Panel'-button to inspect a specific plot and see details. Further options are described below.
              <ul>
              <li><h6><b>Font size</b></h6>
              Use the slider to increase (larger value) or decrease (smaller value). If the font size slider is set at 0 (default), no numbers or patterns are printed inside the cells. The background colors are more visible when numbers are blocked.
              </li>
              <li><h6><b>Method for defining stability</b></h6>
              Often laboratory parameters are measured on a continuous scale and measurements have several decimals. Then it might make sense not to consider very slight changes in laboratory values
              from one visit to another as increases or decreases. Instead laboratory values might be considered equal/stable even though they differ slightly.
              This 'tolerated difference' can be controlled by the user. By default, stability is defined only when two values are exactly equal, that is, the tolerated difference is set at 0.
              There are three options available for determining the tolerated difference:
              <ul>
              <li> Select the option 'IQR' (for interquartile range derived based on patient data at first visit) to determine the tolerated difference
              for each laboratory parameter as a (user-specified) percentage in the IQR. </li>
              <li> Select the option 'range' (i.e., maximum value minus minimum value observed based on patients data at first visit) to determine the tolerated
              difference for each laboratory parameter as a (user-specified) percentage of the range. Note that the range is sensitive to extreme values observed
              in the data (outliers). </li>
              <li> Select the option 'reference range' (i.e., upper limit of normal minus lower limit of normal) to determine the tolerated difference for each laboratory parameter as a (user-specified) percentage of the reference range.
              Note that the tolerated difference cannot be calculated for laboratory parameters which do not have a reference range defined by both the upper and the lower limit of normal. </li>
              </ul>
              The tolerated differences derived based on the method you have chosen and the percentage will be printed next to the diagram for each laboratory parameter.
              </li>
              <li><h6><b>Percentage</b></h6>
              Use the slider to specify the percentage of IQR, range or reference range to determine the tolerated difference (see also 'Method for defining stability'). If set at 0 (default) the tolerated difference is 0, that is, stability is defined only if two values are completely equal. <br>
              The exact value ('tolerated difference') is printed for each laboratory parameter next to the diagram.
              </li>
              <li><h6><b>Color scale</b></h6>
              Use the drop down menue to select your favorite color scale. This color scale is used to color the cell backgrounds. The darker the background color, the more frequent the pattern.
              </li>
              </ul>

              <br>

              <h4><i class='fab fa-cloudsmith'></i><b><i>&nbsp; Reference-value Based Patterns</i></b></h4>
              Aim: Assess how many patients have laboratory values outside the normal range during the study and whether there is a difference between treatment groups. <br><br>

              The tree diagram consists of a starting point (i.e. the root of the tree) and several layers. The first layer represents the first visit, the second layer
              the second visit, and so on. An example for a specific laboratory parameter in the placebo group is shown in Figure 3. You are able to track patients during the trial, and identify at which visits abnormal laboratory values occur. From the starting
              point the sample is split up into two groups: one group with patients who have laboratory values outside the normal range at the first visit (lower path / orange circle)
              and the other group of patients with laboratory values inside the normal range at the first visit (upper path / green circle). Each of the groups is then split
              up based on the laboratory values at the second visit, and so on. <br>
              The size of the circles is proportional to the number of patients. This enables users to identify frequent patterns (e.g. normal - abnormal - abnormal - normal) among visits.
              The total number of patients is depicted inside the circle at the starting point. <br><br>

              No more than approx. 5 visits are recommended because tree structures will get too complex with an increasing number of layers.<br>

              <img src='www/Fig3.png' alt='Graphic cannot be displayed' width='350' height='400'>
              <p> <i><b>Figure 3</b>: Example plot for reference-value based pattern analysis. The number of patients with hematocrit (HCT) values within the reference range(green) or outside the reference range(orange) at four visits, 'Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3', for the placebo group are shown. </i></p>
              Use the 'Open/Close Zoom Panel'-button to check a specific plot and see details. Further options are described below.

              <ul>
              <li><h6><b>Font size</b></h6>
              Use the slider to increase (larger value) or decrease (smaller value) the fontsize of the numbers inside the circles. When the font size is set at 0 (default), the numbers inside the circles are not shown.
              </li>
              <li><h6><b>Definition of abnormal values</b></h6>
              Choose the definition of abnormal values. The following three options are available:
              <ul>
              <li> Select the option 'above ULN or below LLN' if laboratory values are considered abnormal if they either exceed the upper limit of normal (ULN) or if they fall below the lower limit of normal (LLN). </li>
              <li> Select the option 'above ULN' if laboratory values are considered abnormal only if they exceed the upper limit of normal (ULN). </li>
              <li> Select the option 'below LLN' if laboratory values are considered abnormal only if they fall below the lower limit of normal (LLN). </li>
              </ul>
              </li>
              </ul><br>

              <h4><i class='fa fa-file-upload'></i><b>&nbsp; Data Upload</b></h4>
              The data structure and format required for upload is outlined in the &nbsp; <i class='fa fa-file'></i>") ,
         shiny::actionLink("link_to_structure_info", "Data Manual"),
         HTML("-tab. Options for omitting laboratory parameters, treatment groups or visits are described below. <br>

              <ul>
              <li>
              <h6><b>Visits (exclude and rearrange)</b></h6>
              Click the 'x' next to the visit to remove visits you are not needed. Please note that the visits will be removed for every laboratory parameter. There is no possibility to include the visit for some laboratory parameters but to exclude it for others (expect for manually setting the values to NA in your data file for the respective laboratory parameter).
              Drag and drop visits to change the order of the visits in the three types of analyses.</li>

              <li>
              <h6><b>Treatment groups (exclude and rearrange)</b></h6>
              Click the 'x' next to the treatment group to remove treatment groups you are not needed.
              Drag and drop visits to change the order of the visits in the three types of analyses.</li>

              <li>
              <h6><b>Lab parameters</b></h6>
              Click the laboratory parameters in the drop-down menue to deselect laboratory parameters that you are not needed in or re-select previously omitted laboratory parameters. You can also use the text field to search for specific laboratory parameters.<br>
              If you want to change the order of the laboratory parameters, please see the section below on 'Graphic Options'.</li>
              </ul><br>



              <h4><i class='fa fa-cogs'></i> <b>Graphic Options</b></h4>
              The following graphic options are available:
              <ul><li> <h6> <i class='fa fa-arrows-alt'></i><b>&nbsp; Panel/Plot Size</b></h6>
              Adjust the plot size and height by using the sliders and click 'Update!'. </li>

              <li><h6><i class='fa fa-sort-alpha-down'> </i><b>&nbsp; Seriate Lab Parameters</b></h6>
              Use one of three options to change the arrangement of laboratory parameters in the three types of analyses. The following options are available to arrange laboratory parameters:
              <ul>
              <li> Select the option 'as in input' (default) to arrange laboratory parameters according to your preference. You can implement your individual arrangement of laboratory parameters by modifying the
              arrangement in your input data file, such that your input data file reflects your preferred arrangement. </li>
              <li> Select the option 'AI' (for artificial intelligence) to use an intelligent data-driven ordering. This option searches for an arrangement which locates laboratory parameters with (either positively or negatively)
              correlated changes over time close to each other. Use the drop-down menue to select the visits which will be used for deriving the change. Several sorting algorithms
              can be selected by the user. The default method is hierarchical clustering combined with optimal leaf ordering. More information on the methodology can be found in a ")
         , shiny::actionLink("link_to_pdf_view", "short manual"),".")
  })

  output$helptext2 <- shiny::renderUI({
    list(HTML("when laboratory parameters are not included at a specific visit
              and if you have chosen this visit for defining change, the laboratory parameters cannot be used in the sorting algorithm. Therefore the respective laboratory parameters will simply be relocated
              to the arrangement/list obtained by the algorithm, and thus will be relocated at the last position. </li>
              <li> Select the option 'alphabetically' to arrange laboratory parameters alphabetically. </li>
              </ul>
              After your selection click 'Update Order!'. <br></li>

              <li><h6><i class='fa fa-palette'></i> <b>&nbsp; Colors</b></h6>
              Use the drop-down menu to select colors for each visit in the quantitative analysis. You may e.g. choose one color for visits at which patients were under treatment and another
              color for visits (e.g. randomization, follow-up) at which patients were off treatment.</li><br>
              </ul>
              <h4><b>Missing Value Handling</b></h4>
              The  analyses of a specific laboratory parameter require that the patients data must be complete (non-missing for all visits). The following mechanisms are implemented:
              <ul>
              <li> The e<b>lab</b>orator-app automatically omits study visits for a laboratory parameter if more than 50% of patients have missing values for that laboratory parameter. You can also change this percentage using the &nbsp; <i class='fa fa-file-upload'></i> <b> Data Upload</b>-tab. </li>
              <li> Patients who have a missing laboratory value at any of the 'considered' visits (i.e., excluding visits with more than 50% missing values, see first item, and visits that are manually removed by the user) will be excluded from all analyses of the respective laboratory parameter.</li></li>
              </ul>
              There are many different 'patterns' of missing values that might lead to a substantially reduced sample size. The user can, however, decide to automatically exclude some visits in order to avoid a possible substantial reduction in the sample size.
              For example, if a specific laboratory parameter is missing at a specific visit for 40% of the subjects, then the analyses can only use
              the remaining 60% of subjects with non-missing values for that visit (assuming no missing values for the remaining subjects at any of the other visits). A single visit with many missing values can therefore reduce the number of evaluable patients drastically.
              If you want to avoid the exclusion of too many subjects due to a large percentage of missing values at a specific visit (and accept the omission of visits instead), you can set the
              percentage of 'tolerated' missing values (which is by default set to 50%) to a small value.<br> <br>

              You can check the number of patients per treatment group used for all analyses of a specific laboratory parameters in the 'Reference-value Based Pattern' analysis: the number in the 'starting point', i.e. the root of the tree-like structures, shows the total patient number used for the analysis of a specific treatment group and laboratory parameter.
              Note that the number of subjects analyzed might differ between the laboratory parameters because the laboratory parameters are analyzed independently of each other.<br><br>

              The following example illustrates which subjects and visits will be used in the analysis in the occurence of missing data. <br>

              <h5> <b> Example </b></h5>
              The data of a study consists of three subjects and two laboratory parameters hematocrit (HCT) and hemoglobin (HGB). The user has not changed the percentage of 'tolerated' missing values, and therefore the default of 50% is used.
              The original data is summarized below. <br> <br>


              <style>
              table {
              font-family: arial, sans-serif;
              border-collapse: collapse;
              width: 100%;
              }

              td, th {
              border: 1px solid #dddddd;
              text-align: left;
              padding: 8px;
              }

              tr:nth-child(2) {
              background-color: #dff2fd;
              }

              tr:nth-child{
              background-color: #c9e1f6;
              }

              tr:first-child{
              background-color: #11c4d4;
              }
              </style>

              <table>

              <tr>
              <th colspan='1'> </th>
              <th colspan='3'>HCT</th>
              <th colspan='3'>HGB</th>
              </tr>
              <tr>
              <th>Subject</th>
              <th>Visit 1</th>
              <th>Visit 2</th>
              <th>Visit 3</th>
              <th>Visit 1</th>
              <th>Visit 2</th>
              <th>Visit 3</th>
              </tr>
              <tr>
              <th> 1 </th>
              <th> 42.8 </th>
              <th> <font color='#f78300'> NA </font>  </th>
              <th> <font color='#f78300'> NA </font>  </th>
              <th>  13.8</th>
              <th>  13.8</th>
              <th> 14.1 </th>
              </tr>
              <tr>
              <th>2</th>
              <th> 41.2 </th>
              <th>  <font color='#f78300'> NA </font> </th>
              <th>  42.2</th>
              <th>  16.2</th>
              <th>  15.8</th>
              <th>  16.4</th>
              </tr>
              <tr>
              <th>3</th>
              <th> <font color='#f78300'> NA </font> </th>
              <th> 40.9 </th>
              <th>  40.7</th>
              <th>  <font color='#f78300'> NA </font> </th>
              <th>  14.3</th>
              <th>  13.3</th>
              </tr>
              </table>
              <br>
              <i> Which visits will be omitted for each of the two laboratory parameters? </i>
              <ul>
              <li>Visit 2 will be automatically omitted for HCT since more than 50% of the values are missing. The visits 1 and 3 remain for HCT and will be used in the analyses. </li>
              <li> No visit will be omitted for HGB. At maximum 1/3 of the values are missing, thus all three visits will be saved for HGB.
              Visits which are not automatically deleted will be referred to as 'considered ' visits in the following.
              </li></ul>
              <i> Which subjects will be omitted from the analyses for each of the two laboratory parameters? </i>
              <ul>
              <li> Subject 1 will not be used for the analysis of HCT because this subject has a missing value at the considered visit 3. In contrast,  subject 1 has no missing value for any of the considered visits 1, 2 and 3, and is therefore included in the analysis of HGB.
              </li>
              <li> Subject 2 is included in both the analyses of HCT and HGB because it has no missing values for any of the considered visits (note that visit 2 has been omitted for HCT).
              </li>
              <li> Subject 3 is excluded for both HCT and HGB because it has a missing value at any of the considered visit.
              </li></ul>

              "))
  })


  #### REACTIVE OBJECTS ####

  #### reactiveValues ####
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  tolPer <- shiny::reactiveValues(val = 0.5)

  sameax <- shiny::reactiveValues(val = TRUE)

  criterion <- shiny::reactiveValues(val = "within")

  percent <- shiny::reactiveValues(val = 0)

  con_lin <- shiny::reactiveValues(val = FALSE)

  pval <- shiny::reactiveValues(val = 0.01)

  stattest <- shiny::reactiveValues(val = "none")

  add_points <- shiny::reactiveValues(val = TRUE)

  sameax <- shiny::reactiveValues(val = TRUE)

  clustermethod <- shiny::reactiveValues(val = 'OLO_average')

  orderinglab <- shiny::reactiveValues(val = "alphabetically")

  panelheight <- shiny::reactiveValues(val = 500)

  zoompx <- shiny::reactiveValues(val = 100)

  start <- shiny::reactiveValues(dat = FALSE)

  start.ai <- shiny::reactiveValues(dat = FALSE)

  method <- shiny::reactiveValues(val = "InQuRa")

  cex.trend <- shiny::reactiveValues(val = 0)

  cex.rvbp <- shiny::reactiveValues(val = 0)

  values <- shiny::reactiveValues(default = 0)

  #### reactive ####
  output$flag <- shiny::reactive(start$dat)
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  output$ai <- shiny::reactive(start.ai$dat)
  shiny::outputOptions(output, "ai", suspendWhenHidden = FALSE)

  output$check <- shiny::reactive({length(input$trtcompar)})
  shiny::outputOptions(output, 'check', suspendWhenHidden = FALSE)

  df <- shiny::reactive({
    if(input$impswitch == '*.RData file'){
      shiny::req(input$file)
      elaborator_data <- get(load(input$file$datapath))

      elaborator_data$LBTESTCD <- as.factor(elaborator_data$LBTESTCD)
      if("SUBJIDN" %in% names(elaborator_data) && !("SUBJID" %in% names(elaborator_data))){
        elaborator_data$SUBJID <- as.character(elaborator_data$SUBJIDN)
      }
      if("SUBJID" %in% names(elaborator_data) && !("SUBJIDN" %in% names(elaborator_data))){
        elaborator_data$SUBJIDN <- as.numeric(elaborator_data$SUBJID)
      }
      elaborator_data
    }else if(input$impswitch == '*.CSV file'){
      shiny::req(input$csvA)
      elaborator_data <- utils::read.csv(input$csvA$datapath, header=TRUE, na.strings = c('NA','.',''),
                                         sep = input$sep, quote = input$quote)
      elaborator_data$LBORNRHI <- as.character(elaborator_data$LBORNRHI)
      elaborator_data$LBORNRLO <- as.character(elaborator_data$LBORNRLO)
      elaborator_data$LBTESTCD <- as.factor(elaborator_data$LBTESTCD)
      if("SUBJIDN" %in% names(elaborator_data) && !("SUBJID" %in% names(elaborator_data))){
        elaborator_data$SUBJID <- as.character(elaborator_data$SUBJIDN)
      }
      if("SUBJID" %in% names(elaborator_data) && !("SUBJIDN" %in% names(elaborator_data))){
        elaborator_data$SUBJIDN <- as.numeric(elaborator_data$SUBJID)
      }
      elaborator_data
    }
  })


  ds <- shiny::eventReactive(c(input$go_select,input$go_select2), {
    shiny::req(input$select.treatments,isolate(input$select.lab), (input$select.visit))
    df <- df()
    choices.trt <- input$select.treatments
    choices.lab <- isolate(input$select.lab)
    choices.visit <- input$select.visit
    ds <- df %>%
      dplyr::filter(LBTESTCD %in% c(choices.lab) & TRTP %in% c(choices.trt) & AVISIT %in% c(choices.visit))
    ds$AVISIT <- factor(ds$AVISIT)

    if(orderinglab$val == "asinp"){
      ds$LBTESTCD <- factor(ds$LBTESTCD,levels = unique((ds$LBTESTCD)))
      ds$PARAMCD <- factor(ds$LBTESTCD,levels = unique((ds$LBTESTCD)))
    }else if(orderinglab$val=="alphabetically"){
      ds$LBTESTCD <- factor(ds$LBTESTCD,levels = sort(levels(ds$LBTESTCD)))
      ds$PARAMCD <- factor(ds$LBTESTCD,levels = sort(levels(ds$LBTESTCD)))
    }else if(orderinglab$val=="auto"){
      ds$LBTESTCD <- factor(ds$LBTESTCD,levels = levels(ds$LBTESTCD))
      ds$PARAMCD <- factor(ds$LBTESTCD,levels = levels(ds$LBTESTCD))
    }
    ds$TRTP <- factor((ds$TRTP))
    ds$TRTP <- ds$TRTP %>%
      forcats::fct_relevel(input$select.treatments)
    ds$AVISIT <- ds$AVISIT %>%
      forcats::fct_relevel(input$select.visit)
    ds
  })


  ds2 <- shiny::reactive({
    shiny::req(ds(), tolPer$val)
    dat1 <- ds()
    tolPerc <- tolPer$val
    tmp <- dat1 %>%
      dplyr::select(SUBJIDN,TRTP) %>%
      dplyr::distinct() %>%
      dplyr::group_by(TRTP) %>%
      dplyr::summarise(tot = n()) %>%
      dplyr::full_join(dat1, by ="TRTP") %>%
      dplyr::group_by(AVISIT, TRTP, LBTESTCD) %>%
      tidyr::nest() %>%
      dplyr::mutate(nonmissing = purrr::map_int(data, ~ sum(!is.na(.$LBORRES)) )) %>%
      tidyr::unnest_legacy() %>%
      dplyr::mutate(percentage = nonmissing/tot)

    tmp2 <- tmp %>%
      dplyr::group_by(AVISIT, LBTESTCD) %>%
      dplyr::summarise(tr = min(percentage))

    tmp3 <- tmp2 %>%
      dplyr::full_join(tmp, by = c("AVISIT","LBTESTCD")) %>%
      dplyr::filter(tr >= tolPerc) %>%
      dplyr::select(colnames(dat1)) %>%
      dplyr::ungroup()


    countVisits <- tmp3 %>%
      dplyr::group_by(fct_explicit_na(PARAMCD,"NA")) %>%
      tidyr::nest() %>%
      dplyr::mutate(nr_visits = purrr::map_int(data, ~ .$AVISIT %>%
                                                 unique() %>%
                                                 length())) %>%
      tidyr::unnest_legacy()

    nonMissing <- tmp3 %>%
      dplyr::group_by(SUBJIDN,LBTESTCD) %>%
      tidyr::nest() %>%
      dplyr::mutate(nonmiss = purrr::map_int(data, ~ sum(!is.na(.$LBORRES)))) %>%
      tidyr::unnest_legacy() %>%
      dplyr::select(SUBJIDN, LBTESTCD, AVISIT, nonmiss)

    res <- nonMissing %>%
      dplyr::full_join(countVisits, by = c("SUBJIDN","LBTESTCD","AVISIT")) %>%
      dplyr::filter(nr_visits == nonmiss) %>%
      dplyr::select(colnames(tmp3)) %>%
      dplyr::ungroup()

    as.data.frame(res)
  })

  data_param <- shiny::reactive({
    shiny::req(ds2())
    ntreat <- length(unique(ds2()$TRTP))
    nvisit <- length(unique(ds2()$AVISIT))
    nlab <- length(unique(ds2()$PARAMCD))
    tmp <- ds2()
    tmp <- subset(tmp,!(tmp$LBORNRLO == "" & tmp$LBORNRHI == ""))
    nlab2 <- length(unique(tmp$PARAMCD))
    list(ntreat = ntreat, nvisit = nvisit, nlab = nlab, nlab2 = nlab2)
  })

  Summa <-  shiny::reactive({
    shiny::req(ds2(), percent$val)
    dat1 <- ds2()
    percent <- percent$val/100
    firstVisit <- dat1 %>%
      dplyr::pull(AVISIT)%>%
      levels() %>%
      .[1]
    Yall <- dat1 %>%
      tidyr::spread(AVISIT,LBORRES) %>%
      dplyr::select(c(PARAMCD, LBORNRLO, LBORNRHI,
                      SUBJIDN, TRTP, LBTESTCD, firstVisit))
    lowquant <- highquant <- NULL
    Summa <- Yall %>%
      dplyr::group_by(PARAMCD) %>%
      dplyr::summarise(lowquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.25),
                       highquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.75),
                       max = max(!!rlang::sym(firstVisit), na.rm = TRUE),
                       min = min(!!rlang::sym(firstVisit), na.rm = TRUE),
                       highref = mean(as.numeric(LBORNRHI),na.rm = TRUE),
                       lowref = mean(as.numeric(LBORNRLO), na.rm = TRUE)) %>%
      mutate(InQuRa = percent * (highquant - lowquant),
             Range = percent * (max - min),
             refRange = percent * (highref - lowref)) %>%
      dplyr::select(PARAMCD, InQuRa, Range, refRange) %>%
      dplyr::rename(variable = PARAMCD)
    Summa
  })

  trtcompar_val <- shiny::reactive({
    shiny::req(ds2())
    choices  <- as.character(unique(ds2()$AVISIT))
    choices
  })

  #### eventReactive ####
  tcomp <- shiny::eventReactive(c(input$go_select, input$go_select2), {
    input$trtcompar
  })

  pv <- shiny::eventReactive(c(input$go_select, input$go_select2), {
    pval$val
  })

  box_col <- shiny::eventReactive(input$go, {
    shiny::req(input$select.visit)
    visits <- input$select.visit
    selected <- input$trtcompar
    b.col <- c(input$'id1-col', input$'id2-col', input$'id3-col', input$'id4-col', input$'id5-col', input$'id6-col',
               input$'id7-col', input$'id8-col', input$'id9-col', input$'id10-col', input$'id11-col', input$'id12-col',
               input$'id13-col', input$'id14-col', input$'id15-col', input$'id16-col', input$'id17-col', input$'id18-col',
               input$'id19-col', input$'id20-col')
    if (!is.null(b.col)) {
      b.col[b.col == "Color1"] <- colBoxplot1
      b.col[b.col == "Color2"] <- colBoxplot2
      b.col[b.col == "Color3"] <- colBoxplot3
      b.col[b.col == "Color4"] <- colBoxplot4
    }
    if ({stattest$val != "none"})
      b.col[!(visits %in% selected)] <- elaborator_transform_transparent(b.col[!(visits %in% selected)], 70)
    b.col
  })

  pre_clust <- shiny::eventReactive(c(input$go3), {
    shiny::req(ds2())
    ds <- ds2()
    if (orderinglab$val == "auto") {
      first <- input$select.ai.first
      last <- input$select.ai.last

      shiny::validate(
        need(first != last, "Please select different Timepoints for Seriation! The first timepoint must differ from second timepoint.")
      )

      shiny::validate(
        need(sum(ds$AVISIT == first) == sum(ds$AVISIT == last), "The selected timepoints have a different number of observations! Please use other timepoints or try to adjust the percentage of tolerated missing values.")
      )

      if (length(unique(ds$PARAMCD)) > 1 && length(unique(ds$AVISIT)) > 1) {
        for (i in 1:length(ds$LBORRES)) {
          if (is.na(ds[i, ]$LBORRES)) {
            ds[i, ]$LBORRES <- ds %>%
              dplyr::filter(PARAMCD == as.character(ds[i, ]$PARAMCD) & TRTP == as.character(ds[i, ]$TRTP)) %>%
              dplyr::select(LBORRES) %>%
              as.matrix %>%
              stats::median(na.rm = TRUE)
          }}

        ds_new <- ds %>%
          dplyr::group_by(SUBJIDN, PARAMCD) %>%
          dplyr::mutate(n = n())

        df_first <- ds_new %>%
          dplyr::filter(AVISIT != first) %>%
          dplyr::select(PARAMCD, AVISIT, SUBJIDN, LBORRES)
        df_last <-  ds_new %>%
          dplyr::filter(AVISIT != last) %>%
          dplyr::select(PARAMCD, AVISIT, SUBJIDN, LBORRES)

        df_first$AVISIT <- df_last$AVISIT

        tmp <- merge(df_first, df_last, by = c("PARAMCD", "AVISIT", "SUBJIDN")) %>%
          dplyr::mutate(LBORRES = LBORRES.y - LBORRES.x) %>%
          dplyr::select(PARAMCD, AVISIT, SUBJIDN, LBORRES) %>%
          tidyr::spread(key = PARAMCD, value = LBORRES) %>%
          dplyr::mutate(vari = paste0(SUBJIDN, "_", AVISIT)) %>%
          dplyr::select(-dplyr::one_of(c("SUBJIDN","AVISIT")))
        tmp_nam <- tmp %>%
          dplyr::select(vari)
        tmp2 <- tmp %>%
          dplyr::select(-vari) %>%
          t()
        colnames(tmp2) <- t(tmp_nam)

        tmp2

      }}else{NULL}
  })

  clust <- shiny::eventReactive(c(input$go3), {
    shiny::req(ds2())

    tmp2 <- pre_clust()

    ds <- ds2()

    if(orderinglab$val == "asinp"){as.character(unique(ds$PARAMCD)) }
    else if(orderinglab$val == "alphabetically"){ sort(as.character(unique(ds$PARAMCD)))}
    else if(orderinglab$val == "auto"){
      shiny::req(pre_clust())

      tmp2 %>%
        elaborator_calculate_spearman_distance() %>%
        seriation::seriate(method = clustermethod$val) %>%
        seriation::get_order() %>%
        rownames(tmp2)[.]
    }else{
      as.character(unique(ds$PARAMCD))}

  })

  border.col <-shiny::eventReactive(c(input$go_select, input$go_select2),{
    choices <- input$select.visit
    selected <- input$trtcompar
    col <- rep(elaborator_transform_transparent("black", alpha = 70), length(choices))
    col[choices %in% selected] <- "black"
    col
  })

  #### OBSERVERS ####

  #### observe ####

  shiny::observe({
    purrr::map(paste0("id", 1:data_param()$nvisit), ~ shiny::callModule(boxBlotColor, id = .x, c("Color1", "Color2", "Color3", "Color4"),
                                                                        paste0("Select Color for '",
                                                                               input$select.visit[as.numeric(substr(.x,3,nchar(.x)))],
                                                                               "'")))
  })

  shiny::observe({
    if(zoompx$val < 60){
      shinyjs::disable("minus_zoom1")
      shinyjs::disable("minus_zoom2")
      shinyjs::disable("minus_zoom3")
    } else if(zoompx$val >= 60){
      shinyjs::enable("minus_zoom1")
      shinyjs::enable("minus_zoom2")
      shinyjs::enable("minus_zoom3")
    }
    if(zoompx$val > 780){
      shinyjs::disable("plus_zoom1")
      shinyjs::disable("plus_zoom2")
      shinyjs::disable("plus_zoom3")
    } else if(zoompx$val <= 780){
      shinyjs::enable("plus_zoom1")
      shinyjs::enable("plus_zoom2")
      shinyjs::enable("plus_zoom3")
    }
  })

  #### observeEvent ####
  shiny::observeEvent(input$link_to_pdf_view, {
    output$pdfview <- shiny::renderUI({
      tags$iframe(style = "height:500px; width:100%",
                  src = "www/Seriation_methods_20191115.pdf")
    })
  })

  shiny::observeEvent(df(), {
    start$dat <- TRUE
  })

  shiny::observeEvent(input$go3, {
    if (orderinglab$val == "auto") {
      if (!is.null(input$clusterMethod)) {
        if ((startsWith(input$clusterMethod, "OLO") | startsWith(input$clusterMethod, "GW"))) {
          start.ai$dat <- TRUE
        } else {
          start.ai$dat <- FALSE
        }
      }else{
        start.ai$dat <- FALSE
      }
    } else {
      start.ai$dat <- FALSE
    }
  })

  shiny::observeEvent(c(input$select.lab), {
    if (input$go_select == 0) {
      shinyjs::click("go_select")
      shinyjs::click("go_select2")
      click("go3")
    }
  })

  shiny::observeEvent(input$go_select,{
    tolPer$val <- input$select.toleratedPercentage
  })

  shiny::observeEvent(ds2(),{
    shinyjs::disable("impdata")
    shinyjs::disable("impswitch")
  })

  shiny::observeEvent(c(input$go_select, input$go_select2), {
    if (!is.null(input$stattest)) {
      stattest$val <- input$stattest
    }
  })

  shiny::observeEvent(c(input$go_select, input$go_select2), {
    shiny::req(ds2(), input$trtcompar, stattest$val, input$select.treatments,
               isolate(input$select.lab), (input$select.visit))

    dat <- ds2()
    T1 <- input$trtcompar[1]
    T2 <- input$trtcompar[-1]
    signtest <- stattest$val

    if (stattest$val == "signtest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
      values$default <- elaborator_derive_test_values(data = dat,
                                       signtest = TRUE,
                                       Visit1 = T1,
                                       Visit2 = T2,
                                       lab_column = "PARAMCD")
    } else if (stattest$val == "ttest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
      values$default <- elaborator_derive_test_values(data = dat,
                                       signtest = FALSE,
                                       Visit1 = T1,
                                       Visit2 = T2,
                                       lab_column = "PARAMCD")
    } else {
      values$default <- NA
    }
  })

  shiny::observeEvent(input$pcutoff, {
    pval$val <- input$pcutoff
  })

  shiny::observeEvent(input$sameaxes, {
    sameax$val <- input$sameaxes
  })

  shiny::observeEvent(input$con_lin, {
    con_lin$val <- input$con_lin
  })

  shiny::observeEvent(input$criterion, {
    criterion$val <- input$criterion
  })

  shiny::observeEvent(input$go_reload, {
    shinyjs::runjs(
      sprintf("location.reload();")
    )
  })

  shiny::observeEvent(input$go_visual, {
    zoompx$val <- input$zoompx
  })

  shiny::observeEvent(input$link_to_tab_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "helptext")
  })

  shiny::observeEvent(input$link_to_structure_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "datamanual")
  })

  shiny::observeEvent(input$minus_zoom1, {
    if(zoompx$val >= 60){
      zoompx$val <- zoompx$val - 40}
  })

  shiny::observeEvent(input$plus_zoom1, {
    if(zoompx$val < 800){
      zoompx$val <- zoompx$val + 40}
  })
  shiny::observeEvent(input$minus_zoom2, {
    if(zoompx$val >= 60){
      zoompx$val <- zoompx$val - 40}
  })

  shiny::observeEvent(input$plus_zoom2, {
    if(zoompx$val < 800){
      zoompx$val <- zoompx$val + 40}
  })
  shiny::observeEvent(input$minus_zoom3, {
    if(zoompx$val >= 60){
      zoompx$val <- zoompx$val - 40}
  })

  shiny::observeEvent(input$plus_zoom3, {
    if(zoompx$val < 800){
      zoompx$val <- zoompx$val + 40}
  })

  shiny::observeEvent(c(input$minus_panel1, input$minus_panel2, input$minus_panel3,
                        input$plus_panel1, input$plus_panel2, input$plus_panel3), {

                          shinyjs::disable("minus_panel1")
                          shinyjs::disable("minus_panel2")
                          shinyjs::disable("minus_panel3")
                          shinyjs::disable("plus_panel1")
                          shinyjs::disable("plus_panel2")
                          shinyjs::disable("plus_panel3")
                          shinyjs::delay(3000, shinyjs::enable("minus_panel1"))
                          shinyjs::delay(3000, shinyjs::enable("minus_panel2"))
                          shinyjs::delay(3000, shinyjs::enable("minus_panel3"))
                          shinyjs::delay(3000, shinyjs::enable("plus_panel1"))
                          shinyjs::delay(3000, shinyjs::enable("plus_panel2"))
                          shinyjs::delay(3000, shinyjs::enable("plus_panel3"))
                        })

  shiny::observeEvent(c(input$minus_zoom1, input$minus_zoom2, input$minus_zoom3,
                        input$plus_zoom1, input$plus_zoom2, input$plus_zoom3), {

                          shinyjs::disable("minus_zoom1")
                          shinyjs::disable("minus_zoom2")
                          shinyjs::disable("minus_zoom3")
                          shinyjs::disable("plus_zoom1")
                          shinyjs::disable("plus_zoom2")
                          shinyjs::disable("plus_zoom3")
                          shinyjs::delay(1000, shinyjs::enable("minus_zoom1"))
                          shinyjs::delay(1000, shinyjs::enable("minus_zoom2"))
                          shinyjs::delay(1000, shinyjs::enable("minus_zoom3"))
                          shinyjs::delay(1000, shinyjs::enable("plus_zoom1"))
                          shinyjs::delay(1000, shinyjs::enable("plus_zoom2"))
                          shinyjs::delay(1000, shinyjs::enable("plus_zoom3"))
                        })

  shiny::observeEvent(input$panelheight, {
    panelheight$val <- input$panelheight
  })

  shiny::observeEvent(input$minus_panel1, {
    if (panelheight$val > 400) {
      panelheight$val <- panelheight$val - 100
    }
  })

  shiny::observeEvent(input$plus_panel1, {
    if (panelheight$val < 2400) {
      panelheight$val <- panelheight$val + 100
    }
  })
  shiny::observeEvent(input$minus_panel2, {
    if (panelheight$val > 400) {
      panelheight$val <- panelheight$val - 100
    }
  })

  shiny::observeEvent(input$plus_panel2, {
    if (panelheight$val < 2400) {
      panelheight$val <- panelheight$val + 100
    }
  })
  shiny::observeEvent(input$minus_panel3, {
    if (panelheight$val > 400) {
      panelheight$val <- panelheight$val - 100
    }
  })

  shiny::observeEvent(input$plus_panel3, {
    if(panelheight$val < 2400) {
      panelheight$val <- panelheight$val + 100
    }
  })

  shiny::observeEvent(input$orderinglab, {
    orderinglab$val <- input$orderinglab
  })

  shiny::observeEvent(input$clusterMethod, {
    clustermethod$val <- input$clusterMethod
  })

  shiny::observeEvent(input$add_points,{
    add_points$val <- input$add_points
  })

  shiny::observeEvent(input$go_range,{
    percent$val <- input$percent
  })

  shiny::observeEvent(input$go_range,{
    method$val <- input$method
  })

  shiny::observeEvent(input$cex.trend,{
    cex.trend$val <- input$cex.trend
  })

  shiny::observeEvent(input$cex.rvbp,{
    cex.rvbp$val <- input$cex.rvbp
  })


}
