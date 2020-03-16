#' elaborator_ui - User Interface of the elaborator application
#'
#'@keywords internal

jscode <- "shinyjs.disableTab = function(name) {
              var tab = $('.nav li a[data-value='+name+']');
              tab.bind('click.tab', function(e) {
              e.preventDefault();
              return false;});
              tab.addClass('disabled');}

              shinyjs.enableTab = function(name) {
              var tab = $('.nav li a[data-value='+name+']');
              tab.unbind('click.tab');
              tab.removeClass('disabled');}"

css <- ".nav li a.disabled { background-color: #aaa !important;
              color: #333 !important;
              cursor: not-allowed !important;
              border-color: #aaa !important; }"

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

boxBlotColorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("controls"))
}

boxBlotColor <- function(input, output, session, dat, name) {
  output$controls <- shiny::renderUI({
    ns <- session$ns
    tags$div(
      shinyWidgets::pickerInput(inputId = ns("col"),
                                label = paste0(name),
                                choices = dat,
                                multiple = FALSE,
                                choicesOpt = list(
                                  style = c("background-color:#2fb39f !important;color: #ffffff;
                                            font-weight: bold;", "background-color:#00b4cb !important;
                                            color: #ffffff; font-weight: bold;",
                                            "background-color:#0075bc !important;color: #ffffff;
                                            font-weight: bold;", "background-color:#004a8a !important;
                                            color: #ffffff; font-weight: bold;"
                                    )
                                  )
      ), width = "100%"
    )
  })
  return(shiny::reactive({
    validate(need(input$col, FALSE))
    dat[,input$col]
  }))
}

elaborator_ui <- shinydashboard::dashboardPage(title = "elaborator",
  shinydashboard::dashboardHeader(title = shiny::img(src = 'www/BAY_eLaborator_Logo-lang_Negativ.svg',
                                                     height = 24, align = "left"
                                                     ),
                                                     titleWidth = 450
                                  ),
  #### Dashboardsidebar ####
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = 'sidebarmenu',
      shinydashboard::menuItem(text = 'Quantitative Trends',
                               icon = icon('chart-line'),
                               tabName = 'quant'
      ),
      shinydashboard::menuItem(text = 'Qualitative Trends',
                               icon = icon('buromobelexperte'),
                               tabName = 'qual'
      ),
      shinydashboard::menuItem(text = 'Reference-value Based Patterns',
                               icon = icon('cloudsmith'),
                               tabName = 'rvbp'
      ),
      shinydashboard::menuItem(text = 'Graphic Options',
                               tabName = 'options',
                               icon = icon('cogs'),
                               startExpanded = FALSE,
        shinydashboard::menuItem(text = 'Panel/Plot Size ',
                                 tabName = 'panelsizeoptions',
                                 icon = icon('arrows-alt'),
          shiny::uiOutput('zoompx'),
          shiny::uiOutput('panelheight'),
          shiny::actionButton(inputId = "go_visual",
                              label = "Update!",
                              icon = icon("redo"),
                              style = paste0("color: ",
                                             ColorBG, "; background-color: ",
                                             ColorHighlight, "; border-color: ",
                                             ColorBG
                                      )
          )
        ),
        shinydashboard::menuItem(text = 'Seriate Lab Parameters ',
                                 icon = icon('sort-alpha-down'),
                                 tabName = 'ordersequoptions',
          bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Order of lab parameters",
                                                                icon("question"))
                                            )
                                          ),
                                   title = "You can choose between three options to arrange laboratory parameters. Details on the AI-sortng option are given in the 'Information'-tab.",
                                   placement = "top",
                                   expanded = TRUE
          ),
          shiny::uiOutput("orderinglab"),
          shiny::conditionalPanel(condition = "input.orderinglab == 'auto'",
            shiny::uiOutput("select.ai.first"),
            shiny::uiOutput("select.ai.last"),
            shiny::uiOutput("clustermethod")
          ),
          shiny::actionButton(inputId = "go3",
                              label = "Update Order!",
                              icon = icon("redo"),
                              style = paste0("color: ",
                                             ColorBG,"; background-color: ",
                                             ColorHighlight,"; border-color: ",
                                             ColorBG
                                      )
          )
        ),
        shinydashboard::menuItem(text = 'Colors',
                                icon = icon('palette'),
                                selected = TRUE,
                                startExpanded = FALSE,
          purrr::map(paste0("id", 1:20), ~ boxBlotColorUI(id = .x)),
          shiny::actionButton(inputId = "go",
                              label = "Update Colors!",
                              icon = icon("redo"),
                              style = paste0("color: ",
                                             ColorBG, "; background-color: ",
                                             ColorHighlight, "; border-color: ",
                                             ColorBG
                                      )
          )
        )
      ),
      shinydashboard::menuItem(text = 'Data Upload',
                               tabName = 'datimport',
                               icon = icon('file-upload'),
                               selected = TRUE,
                               startExpanded = TRUE,
        shinyWidgets::prettyRadioButtons(inputId = 'impswitch',
                                        label = 'Select file format',
                                        status ="warning",
                                        shape = 'round',
                                        animation = 'smooth',
                                        choices = c('*.RData file', '*.CSV file')
        ),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = jscode),
        shinyjs::inlineCSS(css),
        shiny::actionButton(inputId = 'go_reload',
                           label = "Reload App",
                           icon = icon('refresh'),
                           style = paste0("color: ",
                                          ColorBG,
                                          "; background-color: ",
                                          ColorPanel,
                                          "; border-color: ",
                                          ColorBG
                                          )
        ),
        shiny::uiOutput('impdata'),
        shiny::uiOutput('select.visit'),
        shiny::uiOutput('select.treatments'),
        shiny::uiOutput('select.lab'),
        shiny::conditionalPanel(condition = "output.flag == true",
          shiny::uiOutput('select.toleratedPercentage'),
          shiny::actionButton(inputId = "go_select",
                             label = "Update!",
                             icon = icon("redo"),
                             style = paste0("color: ",
                                            ColorBG,
                                            "; background-color: ",
                                            ColorHighlight,
                                            "; border-color: ",
                                            ColorBG
                                            )
          )
        )
      ),
      shinydashboard::menuItem(text = "Data Manual",icon = icon("file"), tabName = "datamanual"),
      shinydashboard::menuItem(text = "Information",icon = icon("info"), tabName = "helptext")
    )
  ),
  #### Dashboardbody ####
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".shiny-progress {top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px !important; color: blue;font-size: 20px;font-style: italic;}")),
    tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                          Shiny.onInputChange("innerWidth", window.innerWidth);
                          });
                          $(window).resize(function(e) {
                          Shiny.onInputChange("innerWidth", window.innerWidth);
                          });'
              )
    ),
    tags$head(tags$style(shiny::HTML(paste0(".content-wrapper, .right-side { background-color: ", ColorBG, ";}
                                                           .checkbox-inline, .radio-inline {text-align: center; margin-left: 0px;
                                                           margin-right: 0px;padding: 0px;width: 20%;}
                                                           .main-sidebar .sidebar .sidebar-menu .treeview-menu  {background-color: ", ColorPanel, " !important;}
                                                           .main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {background-color: ", ColorApp, " !important;}
                                                           .skin-blue .main-header .logo { background-color: ", ColorApp, ";}
                                                           .skin-blue .main-header .logo:hover {background-color: ", ColorApp, ";}
                                                           .progress-bar{background-color:", ColorHighlight, ";}
                                                           .radio-item-warning {color: ", ColorHighlight, "}
                                                           .btn-warning{ background-color:", ColorHighlight, ";}
                                                           .btn-warning:hover{ background-color:", ColorHighlight, ";}
                                                           .skin-blue .main-header .navbar {background-color: ", ColorApp, ";}
                                                           /* main sidebar */
                                                           .skin-blue .main-sidebar {background-color: ", ColorApp, ";}
                                                           /* active selected tab in the sidebarmenu */
                                                           .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: ", ColorPanel, ";}
                                                           /* other links in the sidebarmenu */
                                                           .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: ", ColorApp, ";color: #ffffff;}
                                                           .skin-blue .sidebar-menu > li.active > a,
                                                           .skin-blue .sidebar-menu > li:hover > a {border-left-color: ", ColorHighlight, ";}
                                                           /* other links in the sidebarmenu when hovered */
                                                           .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: ", ColorPanel, ";}
                                                           /* toggle button when hovered  */
                                                           .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ", ColorBG, ";}
                                                           .skin-blue .main-sidebar .navbar { background-color: ", ColorApp, ";}
                                                           .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ", ColorBG, ";}"
                                          )
                                    )
                        )
    ),
    shinyWidgets::chooseSliderSkin(skin = "Modern", color = "#f6ad82"),
    tags$style(type = 'text/css',
               paste0(".bg-black {background-color: ", ColorApp, "!important; }")
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "quant",
        shiny::fluidPage(
          shiny::tags$head(shiny::tags$style(".fa-question {color:#e3e3e3}",
                                                                   "fa-plus {color:#ffffff}",
                                                                   "fa-minus {color:#ffffff}",
                                                                   ".fa-square {color:#47d2bc}",
                                                                   ".fa-stop {color: #ffeeaa}",
                                                                   ".fa-flask {color: ", ColorBG, "}"
                                            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
            shinydashboard::box(width = NULL,
                                title = span(shiny::tagList('', icon("cogs"))),
                                solidHeader = TRUE,
                                background = 'black',
                                collapsible = TRUE,
                                collapsed = FALSE,
              shiny::fluidRow(
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                shiny::column(2,
                  shinydashboard::box(width = 15,
                    bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Use same scales within lab parameter:",
                                                                          icon("question")))),
                                                                          title = "Define wheter the scales are the same among all treatment groups. Using the same scales among all treatment groups enables a much better comparison between treatment groups. Otherwise, each plot will have its own scale.",
                                                                          placement = "top",
                                                                          expanded = TRUE
                    ),
                    shiny::uiOutput('sameaxes'), background = 'black'
                  )
                ),
                shiny::column(2,
                  shinydashboard::box(width = 12,
                    bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Patient-specific values", icon("question")))),
                                             title = "Tick first box for plotting patient-specific lab values as single points. Tick second box for sorting patient-specific values from smallest to largest; if not ticked they are shown as they occur in the dataset. Tick third box for plotting connection lines between patient measurements.",
                                             placement = "bottom",
                                             expanded = TRUE
                    ),
                    shiny::uiOutput('add_points'),
                    shiny::uiOutput('sortpoint'),
                    shiny::uiOutput('con_lin'),
                    background = 'black'
                  )
                ),
                shinydashboard::box(background = 'black',
                  shiny::column(3,
                    shiny::uiOutput("ueb.stattest"),
                    shiny::uiOutput("stattest"),
                    shiny::actionButton(inputId = "go_select2",
                                        label = "Update!",
                                        icon = icon("redo"),
                                        style = paste0("color: ",
                                                       ColorBG,
                                                       "; background-color: ",
                                                       ColorHighlight,
                                                       "; border-color: ",
                                                       ColorBG
                                                       )
                    )
                  ),
                  shiny::conditionalPanel(condition = "input.stattest != 'none'",
                    shiny::column(4,
                      shiny::uiOutput('ueb.trtcompar'),
                      shiny::uiOutput('trtcompar'),
                      shiny::conditionalPanel(condition = "output.check <2",
                        shiny::uiOutput('minimumtext')
                      )
                    ),
                    shiny::column(3,
                      shiny::uiOutput('ueb.pcutoff'),
                      shiny::uiOutput('pcutoff')
                    )
                  )
                ),
                shiny::column(2,
                  shiny::uiOutput("select.color")
                ),
                shiny::column(2,
                  shiny::uiOutput("helptextbox"),
                  shiny::conditionalPanel(condition = "input.stattest != 'none'",
                    bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList(tags$i(class = "fa fa-square", style = "color:#47d2bc"),"Decrease"))),
                                             title = "Statistical test indicates a decrease in values.", placement = "top",expanded = TRUE
                    ),
                    bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList(tags$i(class = "fa fa-square", style = "color:#ffeeaa"),"Increase"))),
                                             title = "Statistical test indicates an increase in values.", placement = "top",expanded = TRUE
                    ),
                    bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList(tags$i(class = "fa fa-square", style = "color:#A9A9A9"),"Missing"))),
                                             title = "Statistical test indicates missing values.", placement = "top",expanded = TRUE
                    )
                  )
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.ai == true",
            shinydashboard::box(width = NULL,
                                title = span(shiny::tagList('', icon("sort-alpha-down"),'Dendrogram - (Click on the + symbol to open)')),
                                solidHeader = TRUE,
                                background = 'black',
                                collapsible = TRUE,
                                collapsed = TRUE,
              shiny::fluidRow(
                shiny::column(12,
                  shiny::uiOutput("dendro1", height = 'auto')
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12,
              shiny::conditionalPanel(condition = "output.flag == false",
                shiny::uiOutput("myImage"),
                h2("is a novel concept for generating knowledge and gaining insights into laboratory data. You will be able to efficiently and easily explore your laboratory data
                   from different perspectives."
                ),
                br(),
                tags$div(HTML(paste("<i class='fa fa-file-upload'></i>&emsp;",
                                    tags$span(style = "font-size:150%",
                                              "Upload your",
                                              tags$span(style = "color:#f78300", "laboratory data"),
                                              " by using the 'Data Upload'-tab in the task bar on the left.
                                              Select the file format and click
                                              the 'Browse...'-button.",
                                              sep = ""
                                    )
                              )
                          )
                ),
                tags$div(HTML(paste("<i class= 'fa fa-file'></i>&emsp;",
                                    tags$span(style = "font-size:150%",
                                              "Click the 'Data Manual'-tab for the required format and structure for laboratory data file."
                                    )
                              )
                        )
                ),
                tags$div(HTML(paste("<i class='fa fa-info'></i>&emsp;",
                                    tags$span(style = "font-size:150%",
                                              " If you want to access information on the elaborator, click the 'Information'-tab.",
                                              sep = ""
                                    )
                              )
                          )
                )
              ),
              shiny::uiOutput('tab1', width = 'auto'),
              shiny::conditionalPanel(condition = "output.flag == true",
                shiny::uiOutput('hoverpanel')
              ),
              shiny::uiOutput('zoompanel1')
            )
          )
        )
      ),
      shinydashboard::tabItem(tabName = "datamanual",
        shiny::uiOutput('manual')
      ),
      shinydashboard::tabItem(tabName = "helptext",
        shiny::uiOutput('helptext1'),
        shiny::uiOutput('pdfview'),
        shiny::uiOutput('helptext2')
      ),
      shinydashboard::tabItem(tabName = "qual",
        shiny::fluidPage(
          shinydashboard::box(width = NULL,
                              title = span(shiny::tagList(' ',icon("cogs"))),
                              background = 'black',
                              solidHeader = TRUE,
                              collapsible = TRUE,
            shiny::fluidRow(
              shiny::column(2,
                bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Font size", icon("question")))),
                                         title = "Adapt font size. Set font size to 0 to exclude any text.",
                                         placement = "top",
                                         expanded = TRUE
                ),
                shiny::uiOutput('cex.trend')
              ),
              shiny::column(2,
                bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Choose method for defining stability", icon("question")))),
                                         title = "You can specify a tolerated difference in which a change in two adjacent lab values are considered stable ('='). This tolerated difference can be derived as a (small) percentage of the interquartile range (IQR), the range or the reference range. The IQR and the range is evaluated at the first visit across all treatment groups.",
                                         placement = "bottom",
                                         expanded = TRUE
                ),
                shiny::uiOutput('method')
              ),
              shiny::column(2,
                bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Select percentage", icon("question")))),
                                         title = "Select a percent value in the method chosen in order to derive the critical boundary. If set to 0, then adjacent lab values must be exactly equal in order to be considered stable.",
                                         placement = "top",
                                         expanded = TRUE
                ),
                shiny::uiOutput('percent'),
                shiny::actionButton("go_range","Update Selection!",
                                    icon = icon("redo"),
                                    style = paste0("color: ", ColorBG, "; background-color: ", ColorHighlight,"; border-color: ", ColorBG)
                )
              ),
              shiny::column(2,
                bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Select a color scale", icon("question")))),
                                         title = "Select your favorite color scale used for highlighting frequent patterns.",
                                         placement = "top",
                                         expanded = TRUE
                ),
                shiny::uiOutput('select.ev1'),
                shiny::uiOutput('select.pal1'),
                shiny::plotOutput('prev.pal1', height = '20px')
              ),
              shiny::column(width = 2, offset = 4,
                shiny::uiOutput("helptextbox2")
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.ai == true",
            shinydashboard::box(width = NULL,
                                title = span(shiny::tagList('', icon("sort-alpha-down"),'Dendrogram - (Click on the + symbol to open)')),
                                solidHeader = TRUE,
                                background = 'black',
                                collapsible = TRUE,
                                collapsed = TRUE,
              shiny::fluidRow(
                shiny::column(12,
                  shiny::uiOutput("dendro2", height = 'auto')
                )
              )
            )
          ),
          shiny::uiOutput('tab2', width = 'auto'),
          shiny::uiOutput('zoompanel2'),
          shiny::uiOutput('legendpanel'),
          shiny::uiOutput('hoverpanel2')
        )
      ),

      shinydashboard::tabItem(tabName = "rvbp",
      shiny::fluidPage(
        shinydashboard::box(width = NULL,
                            title = span(shiny::tagList('', icon("cogs"))),
                            background = 'black',
                            solidHeader = TRUE,
                            collapsible = TRUE,
          shiny::column(2,
            bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Font size", icon("question")))),
                                     title = "Adapt font size. Set font size to 0 to suppress any text.",
                                     placement = "top",
                                     expanded = TRUE
            ),
            shiny::uiOutput('cex.rvbp')
          ),
          shiny::column(2,
            bsplus::bs_embed_tooltip(tag = h4(span(shiny::tagList("Definition of abnormal values", icon("question")))),
                                     title = "Select how to define abnormal values based on the upper limit of normal (ULN) and lower limit of normal (LLN).",
                                     placement = "top",
                                     expanded = TRUE
            ),
            shiny::uiOutput('criterion')
          ),
          shiny::column(width = 2, offset = 6,
            shiny::uiOutput("helptextbox3")
          )
        ),
        shiny::conditionalPanel(condition = "output.ai == true",
          shinydashboard::box(width = NULL,
                              title = span(shiny::tagList('',
                                                          icon("sort-alpha-down"),
                                                          'Dendrogram - (Click on the + symbol to open)'
                                           )
                                      ),
                              solidHeader = TRUE,
                              background = 'black',
                              collapsible = TRUE,
                              collapsed = TRUE,
            shiny::fluidRow(
              shiny::column(12,
                shiny::uiOutput("dendro3", height = 'auto')
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::uiOutput('tab3', width = 'auto'),
            shiny::uiOutput('zoompanel3'),
            shiny::uiOutput('hoverpanel3')
          )
        )
      )
    )
  ),
  tags$script(HTML("$('body').addClass('sidebar-mini');")),
  tags$head(tags$style(HTML(" h1 {font-family: 'Arial';line-height: 1.1;color: #fffff;}")
            )
  ),
  tags$script(HTML('$(document).ready(function() {
                    $("header").find("nav").append(\' <h4 style="color:white"> A New Perspective on Laboratory Data </h4>\');
                    })
                    '
              )
  ),

  tags$style(type = 'text/css',
  ".selectize-dropdown-content {max-height: 50px;}"
  ),
  tags$style(type = 'text/css', ".selectize-input { background-color: #F8F8F8;}")
  )
)
