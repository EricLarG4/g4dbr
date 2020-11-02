#' G4 biophysics database management and visualization
#'
#' @return Launches the shiny app.
#' @examples
#' g4db()

g4db <- function() {

    #libraries----

    library(shiny)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(shinyWidgets)
    library(shinyBS)
    library(shinysky)

    library(tidyverse)
    library(readxl)
    library(writexl)
    library(DT)
    library(QWUtils)
    library(zoo)
    library(data.table)
    library(DescTools)
    library(hablar)
    library(assertive.types)
    library(magrittr)

    library(ggsci)
    library(ggpmisc)
    library(ggpubr)
    library(ggthemes)
    library(ggrepel)
    library(plotly)

    library(rmarkdown)

    #sources----
    # source('EpsilonCalc.R')
    # source('nb_row_extract.R')

    #UI-----------
    ui <- dashboardPagePlus(
        #header--------------
        dashboardHeader(title = "g4db"),
        #sidebar-------------
        sidebar_fullCollapse = TRUE,
        dashboardSidebar(
            conditionalPanel(
                condition = "input.tabs == 'importR'",
                column(12,
                       h3('Import')
                ),
                fileInput(
                    'raw.data.input',
                    'Select .xlsx file'
                ),
                column(12,
                       hr(),
                       h3('Filters')
                ),
                uiOutput("select.oligo"),
                uiOutput("select.buffer.id"),
                uiOutput("select.buffer"),
                uiOutput("select.cation"),
                column(12,
                       hr(),
                       h3('Colours')
                ),
                uiOutput('select.import.palette.fam'),
                uiOutput('select.import.palette'),
                column(12,
                       hr(),
                       h3('Export'),
                       h5('1/ Open a database')
                ),
                uiOutput('db.file.select.2'),
                column(12,
                       h5('2/ Select methods to export')
                ),
                switchInput(inputId = 'exp.CD',
                            label = 'CD',
                            offLabel = 'Off',
                            onLabel = 'On',
                            onStatus = 'info',
                            offStatus = 'danger'
                ),
                switchInput(inputId = 'exp.NMR',
                            label = 'NMR',
                            offLabel = 'Off',
                            onLabel = 'On',
                            onStatus = 'info',
                            offStatus = 'danger'
                ),
                switchInput(inputId = 'exp.MS',
                            label = 'MS',
                            offLabel = 'Off',
                            onLabel = 'On',
                            onStatus = 'info',
                            offStatus = 'danger'
                ),
                switchInput(inputId = 'exp.melt',
                            label = 'UV',
                            offLabel = 'Off',
                            onLabel = 'On',
                            onStatus = 'info',
                            offStatus = 'danger'
                ),
                column(12,
                       h5('3/ Write on the database'),
                       downloadButton("downloadData", "Write to db file"),
                       hr()
                )
            ),
            conditionalPanel(
                condition = "input.tabs == 'meltR'",
                boxPlus(
                    width = "100%",
                    title = "Raw data",
                    status = 'info',
                    solidHeader = F,
                    collapsible = T,
                    switchInput(inputId = "melt.blank", #toggles baseline on/off
                                label = "Blank",
                                onLabel = 'subtract',
                                offLabel = 'ignore',
                                value = TRUE,
                                size = 'normal',
                                width = 'auto'),
                    switchInput(inputId = "fit.or.raw",
                                label = "select data",
                                onLabel = "fit",
                                offLabel = 'raw',
                                value = T,
                                size = 'normal',
                                width = 'auto'),
                    actionBttn(inputId = "bttn.uv.export", #initiates fit
                               label = "Send to importR",
                               icon = icon('sign-in-alt', class = 'regular'),
                               style = "simple",
                               color = "primary",
                               size = "sm",
                               block = F,
                               no_outline = TRUE)
                ),
                boxPlus(
                    width = "100%",
                    title = "Derivative",
                    status = 'primary',
                    solidHeader = F,
                    collapsible = T,
                    sliderInput(inputId = "melt.deriv.smooth.width",
                                label = "Smooth window",
                                min = 1,
                                max = 20,
                                value = 5,
                                step = 1),
                    actionBttn(inputId = "bttn.deriv.melt", #initiates fit
                               label = "Plot derivatives",
                               icon = icon('calculator', class = 'regular'),
                               style = "simple",
                               color = "primary",
                               size = "sm",
                               block = F,
                               no_outline = TRUE)
                ),
                boxPlus(
                    width = "100%",
                    title = "Fitting",
                    status = 'danger',
                    solidHeader = F,
                    collapsible = T,
                    sliderInput("nb.it.melt.fit",
                                "Max iterations",
                                min = 500,
                                max = 100000,
                                value = 5000,
                                step = 500),
                    actionBttn(inputId = "bttn.init.melt", #initiates fit
                               label = "Initialize fitting",
                               icon = icon('sign-out-alt', class = 'regular'),
                               style = "simple",
                               color = "warning",
                               size = "sm",
                               block = F,
                               no_outline = TRUE),
                    actionBttn(inputId = "bttn.fit.melt", #initiates fit
                               label = "Launch fitting",
                               icon = icon('sign-in-alt', class = 'regular'),
                               style = "simple",
                               color = "danger",
                               size = "sm",
                               block = F,
                               no_outline = TRUE),
                    switchInput(inputId = "toggle.baseline", #toggles baseline on/off
                                label = "toggle baselines",
                                value = TRUE),
                    sliderInput("temp.therm",
                                "Temperature (K) for DeltaG", #Temperature for deltaG calculation
                                min = 273,
                                max = 373,
                                value = 273.15 + 22)
                ),
                boxPlus(
                    title = "Download figures",
                    id = "melt.dl",
                    collapsible = T,
                    solidHeader = F,
                    width = '100%',
                    downloadBttn(
                        outputId = "dwn.melt.fit",
                        label = "Fit data",
                        style = "material-flat",
                        size = 'xs'
                    ),
                    downloadBttn(
                        outputId = "dwn.melt.model",
                        label = "Model data",
                        style = "material-flat",
                        size = 'xs'
                    ),
                    downloadBttn(
                        outputId = "dwn.melt.folded",
                        label = "Folded fraction",
                        style = "material-flat",
                        size = 'xs'
                    ),
                    downloadBttn(
                        outputId = "dwn.melt.Tm",
                        label = "Tm summary",
                        style = "material-flat",
                        size = 'xs'
                    )
                )
            ),
            conditionalPanel(
                condition = "input.tabs == 'database'",
                column(12,
                       h3('Load'),
                       uiOutput('db.file.select'),
                       hr()
                ),
                column(12,
                       h3('Filters')
                ),
                uiOutput("select.oligo.db"),
                uiOutput("select.buffer.id.db"),
                uiOutput("select.buffer.db"),
                uiOutput("select.cation.db"),
                column(12,
                       hr(),
                       h3('Colours')
                ),
                uiOutput('select.import.palette.fam.db'),
                uiOutput('select.import.palette.db'),
                column(12,
                       hr(),
                       h3('Report')
                ),
                switchInput(inputId = 'report.choice',
                            label = 'Report type',
                            offLabel = 'SI',
                            onLabel = 'Full',
                            onStatus = 'info',
                            offStatus = 'success'
                ),
                radioButtons('format', 'Document format', c('Word', 'HTML', 'PDF'),
                             inline = TRUE),
                column(12,
                       downloadButton('downloadReport')
                ),
                column(12,
                       hr(),
                       h3('Erase db'),
                       h5('1/ Select oligonucleotides to erase from the sidebar filter'),
                       h5('The following oligonucleotides will be erased:'),
                       textOutput('oligos.to.remove'),
                       h5('2/ Select the methods to erase:'),
                       switchInput(inputId = 'erase.CD',
                                   label = 'CD',
                                   offLabel = 'Off',
                                   onLabel = 'On',
                                   onStatus = 'info',
                                   offStatus = 'danger'
                       ),
                       switchInput(inputId = 'erase.NMR',
                                   label = 'NMR',
                                   offLabel = 'Off',
                                   onLabel = 'On',
                                   onStatus = 'info',
                                   offStatus = 'danger'
                       ),
                       switchInput(inputId = 'erase.MS',
                                   label = 'MS',
                                   offLabel = 'Off',
                                   onLabel = 'On',
                                   onStatus = 'info',
                                   offStatus = 'danger'
                       ),
                       switchInput(inputId = 'erase.UV',
                                   label = 'UV',
                                   offLabel = 'Off',
                                   onLabel = 'On',
                                   onStatus = 'info',
                                   offStatus = 'danger'
                       ),
                       h5('3/ Download the curated database:'),
                       downloadButton("erase.db", "Erase to a db file"),
                )
            )
        ),
        #body----
        dashboardBody(
            #color of selected datatable rows
            tags$style(HTML('table.dataTable tr.selected td,
                        table.dataTable td.selected {background-color: pink !important;}')),
            navbarPage(
                'Navigation',
                id = 'tabs',
                #panel database----
                tabPanel(
                    title = 'database',
                    icon = icon("database"),
                    fluidRow(
                        column(12,
                               gradientBox(id = 'input.info.db',
                                           title = 'General information and oligonucleotide selection',
                                           icon = "fa fa-info-circle",
                                           gradientColor = 'teal',
                                           collapsible = T,
                                           collapsed = T,
                                           width = 12,
                                           footer = DTOutput("input.info.db")
                               )
                        ),
                        column(12,
                               boxPlus(id = 'output.p.CD.db',
                                       title = 'Circular Dichroism',
                                       collapsible = T,
                                       collapsed = F,
                                       width = 6,
                                       uiOutput("p.CD.db.ui"),
                                       enable_sidebar = T,
                                       sidebar_width = 20,
                                       sidebar_content = tagList(
                                           switchInput(inputId = 'cd.data.select.db',
                                                       label = 'normalized',
                                                       value = T,
                                                       onLabel = 'Yes',
                                                       offLabel = 'No',
                                                       onStatus = 'danger',
                                                       offStatus = 'info',
                                                       size = 'small',
                                                       width = '400px'),
                                           selectInput(
                                               inputId = "cd.superimpose.db",
                                               label = "superimposition",
                                               choices = c('none', 'oligos', 'buffer', 'all'),
                                               selected = 'all'
                                           ),
                                           selectInput(
                                               inputId = 'cd.free.db',
                                               label = 'scale',
                                               choices = c('free', 'not free'),
                                               selected = 'free'
                                           ),
                                           sliderInput(
                                               inputId = "slide.cd.db",
                                               label = "Wavelength (nm)",
                                               min = 200,
                                               max = 350,
                                               value = c(220, 320),
                                               step = 5
                                           ),
                                           sliderInput(
                                               inputId = "cd.size.pt.db",
                                               label = "point size",
                                               min = 0,
                                               max = 10,
                                               value = 2,
                                               step = 0.5
                                           ),
                                           sliderInput(
                                               inputId = "cd.size.line.db",
                                               label = "line size",
                                               min = 0,
                                               max = 5,
                                               value = 0,
                                               step = 0.25
                                           ),
                                           sliderInput(
                                               inputId = "cd.alpha.pt.db",
                                               label = "transparency",
                                               min = 0,
                                               max = 1,
                                               value = 0.85,
                                               step = 0.05
                                           )
                                       )
                               ),
                               boxPlus(id = 'output.p.NMR.db',
                                       title = '1H NMR',
                                       collapsible = T,
                                       collapsed = F,
                                       width = 6,
                                       uiOutput("p.NMR.ui.db"),
                                       enable_sidebar = T,
                                       sidebar_width = 20,
                                       sidebar_content = tagList(
                                           selectInput(
                                               inputId = "nmr.superimpose.db",
                                               label = "superimposition",
                                               choices = c('none', 'oligos', 'buffer', 'all'),
                                               selected = 'none'
                                           ),
                                           selectInput(
                                               inputId = 'nmr.free.db',
                                               label = 'scale',
                                               choices = c('free', 'not free'),
                                               selected = 'not free'
                                           ),
                                           sliderInput(
                                               inputId = "slide.nmr.db",
                                               label = "chemical shift (ppm)",
                                               min = 8,
                                               max = 13,
                                               value = c(9.5, 12.5),
                                               step = 0.25
                                           ),
                                           sliderInput(
                                               inputId = "nmr.size.line.db",
                                               label = "line size",
                                               min = 0.25,
                                               max = 5,
                                               value = 1,
                                               step = 0.05
                                           )
                                       )
                               )
                        ),
                        column(12,
                               boxPlus(
                                   id = 'output.CD.db',
                                   title = 'CD data',
                                   collapsible = T,
                                   collapsed = T,
                                   width = 6,
                                   DTOutput("input.CD.db")
                               ),
                               boxPlus(
                                   id = 'output.NMR.db',
                                   title = 'NMR data',
                                   icon = icon('magnet'),
                                   collapsible = T,
                                   collapsed = T,
                                   width = 6,
                                   DTOutput("input.NMR.db")
                               )
                        ),
                        column(12,
                               boxPlus(
                                   id = 'output.UV.fit.db',
                                   title = 'UV-melting - raw data',
                                   collapsible = T,
                                   collapsed = F,
                                   width = 6,
                                   uiOutput('p.UV.fit.ui.db'),
                                   enable_sidebar = T,
                                   sidebar_width = 20,
                                   sidebar_content = tagList(
                                       sliderInput(
                                           inputId = "slide.uv.fit.db",
                                           label = "Temperature (K)",
                                           min = 0+273,
                                           max = 100+273,
                                           value = c(5+273, 95+273),
                                           step = 2.5
                                       ),
                                       sliderInput(
                                           inputId = "uv.fit.size.line.db",
                                           label = "line size",
                                           min = 0.25,
                                           max = 5,
                                           value = 1,
                                           step = 0.05
                                       ),
                                       sliderInput(
                                           inputId = "uv.fit.alpha.line.db",
                                           label = "line transparency",
                                           min = 0,
                                           max = 1,
                                           value = 0.85,
                                           step = 0.05
                                       ),
                                       sliderInput(
                                           inputId = "uv.fit.size.pt.db",
                                           label = "point size",
                                           min = 0.5,
                                           max = 10,
                                           value = 2,
                                           step = 0.5
                                       ),
                                       sliderInput(
                                           inputId = "uv.fit.alpha.pt.db",
                                           label = "point transparency",
                                           min = 0,
                                           max = 1,
                                           value = 0.85,
                                           step = 0.05
                                       )
                                   )
                               ),
                               boxPlus(
                                   id = 'output.UV.melting.db',
                                   title = 'UV-melting - folded fraction',
                                   collapsible = T,
                                   collapsed = F,
                                   width = 6,
                                   uiOutput('p.UV.melting.ui.db'),
                                   enable_sidebar = T,
                                   sidebar_width = 20,
                                   sidebar_content = tagList(
                                       sliderInput(
                                           inputId = "slide.uv.db",
                                           label = "Temperature (K)",
                                           min = 0+273,
                                           max = 100+273,
                                           value = c(5+273, 95+273),
                                           step = 2.5
                                       ),
                                       sliderInput(
                                           inputId = "uv.size.pt.db",
                                           label = "point size",
                                           min = 0.5,
                                           max = 10,
                                           value = 2,
                                           step = 0.5
                                       ),
                                       sliderInput(
                                           inputId = "uv.alpha.pt.db",
                                           label = "point transparency",
                                           min = 0,
                                           max = 1,
                                           value = 0.85,
                                           step = 0.05
                                       )
                                   )
                               )
                        ),
                        column(12,
                               boxPlus(
                                   id = 'input.UV.db',
                                   title = 'UV-melting data',
                                   collapsible = T,
                                   collapsed = T,
                                   width = 6,
                                   DTOutput('input.UV.db')
                               ),
                               boxPlus(
                                   id = 'input.MS.db',
                                   title = 'native ESI-MS data',
                                   collapsible = T,
                                   collapsed = T,
                                   width = 6,
                                   DTOutput('input.ms.db')
                               )
                        ),
                        column(12,
                               column(12,
                                      actionBttn(inputId = "plotMS.db",
                                                 label = "plot MS",
                                                 icon = icon('check-circle', class = 'regular'),
                                                 style = "simple",
                                                 color = "danger",
                                                 size = "sm",
                                                 block = F,
                                                 no_outline = TRUE)
                               ),
                               boxPlus(id = 'output.MS.db',
                                       title = 'Native ESI-MS',
                                       collapsible = T,
                                       collapsed = F,
                                       width = 12,
                                       uiOutput("p.MS.ui.db"),
                                       enable_sidebar = T,
                                       sidebar_width = 20,
                                       sidebar_content = tagList(
                                           sliderInput(
                                               inputId = "slide.ms.db",
                                               label = "m/z",
                                               min = 300,
                                               max = 3000,
                                               value = c(800, 2500),
                                               step = 25
                                           ),
                                           uiOutput('select.tune.db'),
                                           uiOutput("select.rep.db"),
                                           selectInput(
                                               inputId = "ms.superimpose.db",
                                               label = "Layout",
                                               choices = c("oligo x buffer","oligo x tune","oligo x replicate",
                                                           "buffer x tune","buffer x replicate","tune x replicate"),
                                           ),
                                           switchInput(
                                               inputId = 'switch.grid.ms.db',
                                               label = 'transpose grid',
                                               onLabel = 'yes',
                                               offLabel = 'no',
                                               value = T
                                           ),
                                           switchInput(
                                               inputId = 'switch.label.ms.db',
                                               label = 'labels',
                                               onLabel = 'yes',
                                               offLabel = 'no',
                                               value = T
                                           ),
                                           sliderInput(
                                               inputId = "ms.size.line.db",
                                               label = "line size",
                                               min = 0.25,
                                               max = 5,
                                               value = 1,
                                               step = 0.05
                                           )
                                       )
                               )
                        ),
                        column(12,
                               boxPlus(id = 'output.MS.db.2',
                                       title = 'Zoomed native ESI-MS',
                                       collapsible = T,
                                       collapsed = F,
                                       width = 12,
                                       uiOutput("p.MS.ui.db.2"),
                                       enable_sidebar = TRUE,
                                       sidebar_width = 20,
                                       sidebar_content = tagList(
                                           selectInput(
                                               inputId = 'charge.select',
                                               label = 'charge',
                                               multiple = F,
                                               choices = 1:10,
                                               selected = 5
                                           )
                                       )
                               )
                        )
                    )
                ),
                #panel importR---------
                tabPanel(
                    title = 'importR',
                    icon = icon('wrench'),
                    column(12,
                           gradientBox(id = 'input.info',
                                       title = 'General information',
                                       gradientColor = 'teal',
                                       collapsible = T,
                                       collapsed = T,
                                       width = 12,
                                       footer = DTOutput("input.info")
                           ),
                           # gradientBox(id = 'input.info.debug',
                           #             title = 'info written to db for debug',
                           #             gradientColor = 'teal',
                           #             collapsible = T,
                           #             collapsed = T,
                           #             width = 12,
                           #             footer = DTOutput("info.debug")
                           # )
                    ),
                    column(12,
                           textOutput("selected.oligos")),
                    column(12,
                           boxPlus(id = 'output.p.CD',
                                   title = 'Circular dichroism',
                                   collapsible = T,
                                   collapsed = F,
                                   width = 6,
                                   uiOutput("p.CD.ui"),
                                   enable_sidebar = T,
                                   sidebar_width = 20,
                                   sidebar_content = tagList(
                                       switchInput(inputId = 'cd.data.select',
                                                   label = 'normalized',
                                                   value = T,
                                                   onLabel = 'Yes',
                                                   offLabel = 'No',
                                                   onStatus = 'danger',
                                                   offStatus = 'info',
                                                   size = 'small',
                                                   width = '400px'),
                                       selectInput(
                                           inputId = "cd.superimpose",
                                           label = "superimposition",
                                           choices = c('none', 'oligos', 'buffer', 'all'),
                                           selected = 'all'
                                       ),
                                       selectInput(
                                           inputId = 'cd.free',
                                           label = 'scale',
                                           choices = c('free', 'not free'),
                                           selected = 'free'
                                       ),
                                       sliderInput(
                                           inputId = "slide.cd",
                                           label = "Wavelength (nm)",
                                           min = 200,
                                           max = 350,
                                           value = c(220, 330),
                                           step = 5
                                       ),
                                       sliderInput(
                                           inputId = "cd.size.pt",
                                           label = "point size",
                                           min = 0.5,
                                           max = 10,
                                           value = 2,
                                           step = 0.5
                                       ),
                                       sliderInput(
                                           inputId = "cd.alpha.pt",
                                           label = "transparency",
                                           min = 0,
                                           max = 1,
                                           value = 0.85,
                                           step = 0.05
                                       )
                                   )
                           ),
                           boxPlus(id = 'output.p.NMR',
                                   title = '1H NMR',
                                   collapsible = T,
                                   collapsed = F,
                                   width = 6,
                                   uiOutput("p.NMR.ui"),
                                   enable_sidebar = T,
                                   sidebar_width = 20,
                                   sidebar_content = tagList(
                                       selectInput(
                                           inputId = "nmr.superimpose",
                                           label = "superimposition",
                                           choices = c('none', 'oligos', 'buffer', 'all'),
                                           selected = 'none'
                                       ),
                                       selectInput(
                                           inputId = 'nmr.free',
                                           label = 'scale',
                                           choices = c('free', 'not free'),
                                           selected = 'not free'
                                       ),
                                       sliderInput(
                                           inputId = "slide.nmr",
                                           label = "chemical shift (ppm)",
                                           min = 8,
                                           max = 13,
                                           value = c(9.5, 12.5),
                                           step = 0.25
                                       ),
                                       sliderInput(
                                           inputId = "nmr.size.line",
                                           label = "line size",
                                           min = 0.25,
                                           max = 5,
                                           value = 1,
                                           step = 0.05
                                       )
                                   )
                           )
                    ),
                    column(12,
                           boxPlus(
                               id = 'output.CD',
                               title = 'CD data',
                               collapsible = T,
                               collapsed = T,
                               width = 6,
                               DTOutput("input.CD")
                           ),
                           boxPlus(
                               id = 'output.NMR',
                               title = 'NMR data',
                               collapsible = T,
                               collapsed = T,
                               width = 6,
                               DTOutput("input.NMR")
                           )
                    ),
                    column(12,
                           dropdownButton(
                               tags$h3("Data reduction"),
                               sliderInput(
                                   inputId = "mz.filter.range",
                                   label = "m/z range",
                                   min = 300,
                                   max = 3000,
                                   value = c(300, 3000),
                                   step = 25
                               ),
                               sliderInput(
                                   inputId = "mz.baseline.range",
                                   label = "baseline range",
                                   min = 300,
                                   max = 3000,
                                   value = c(1750, 2000),
                                   step = 25
                               ),
                               sliderInput(
                                   inputId = "baseline.int",
                                   label = "filtering multiplicator",
                                   min = 0,
                                   max = 5,
                                   value = 0,
                                   step = 0.1
                               ),
                               circle = TRUE,
                               status = "danger",
                               icon = icon("gear"),
                               width = "100px",
                               tooltip = tooltipOptions(title = "Click to access data reduction tools!")
                           )
                    ),
                    column(12,
                           boxPlus(id = 'output.MS',
                                   title = 'Native ESI-MS',
                                   collapsible = T,
                                   collapsed = F,
                                   width = 12,
                                   actionBttn(inputId = "plotMS",
                                              label = "plot MS",
                                              icon = icon('check-circle', class = 'regular'),
                                              style = "simple",
                                              color = "danger",
                                              size = "sm",
                                              block = F,
                                              no_outline = TRUE
                                   ),
                                   uiOutput("p.MS.ui"),
                                   enable_sidebar = T,
                                   sidebar_width = 20,
                                   sidebar_content = tagList(
                                       sliderInput(
                                           inputId = "slide.ms",
                                           label = "m/z",
                                           min = 300,
                                           max = 3000,
                                           value = c(800, 2500),
                                           step = 25
                                       ),
                                       uiOutput('select.tune'),
                                       uiOutput("select.rep"),
                                       selectInput(
                                           inputId = "ms.superimpose",
                                           label = "Layout",
                                           choices = c("oligo x buffer","oligo x tune","oligo x replicate",
                                                       "buffer x tune","buffer x replicate","tune x replicate"),
                                       ),
                                       switchInput(
                                           inputId = 'switch.grid.ms',
                                           label = 'transpose grid',
                                           onLabel = 'yes',
                                           offLabel = 'no',
                                           value = T
                                       ),
                                       switchInput(
                                           inputId = 'switch.label.ms',
                                           label = 'labels',
                                           onLabel = 'yes',
                                           offLabel = 'no',
                                           value = T
                                       ),
                                       sliderInput(
                                           inputId = "ms.size.line",
                                           label = "line size",
                                           min = 0.25,
                                           max = 5,
                                           value = 1,
                                           step = 0.05
                                       )
                                   )
                           )
                    ),
                    column(12,
                           boxPlus(
                               id = 'input.MS',
                               title = 'native ESI-MS data',
                               collapsible = T,
                               collapsed = T,
                               width = 6,
                               DTOutput('input.MS')
                           ),
                           boxPlus(
                               id = 'output.UV.melting',
                               title = 'UV-melting data',
                               collapsible = T,
                               collapsed = T,
                               width = 6,
                               DTOutput("input.UV.melting")
                           )
                    )
                ),
                #panel meltR---------
                tabPanel(
                    title = 'meltR',
                    icon = icon("thermometer-half"),
                    fluidRow(
                        column(12,
                               collapsible_tabBox(
                                   title = 'Input data',
                                   id = 'tabbox.1',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                       title = 'Input data',
                                       plotOutput("p.melt.filtered"),
                                       icon = icon('server', class = 'regular'),
                                   ),
                                   tabPanel(
                                       title = 'Filtered data table',
                                       DTOutput("melt.filtered")
                                   ),
                                   tabPanel(
                                       title = 'Derivative plot',
                                       plotOutput("p.melt.derivative"),
                                       icon = icon('calculator', class = 'regular'),
                                   )
                               ),
                               collapsible_tabBox(
                                   title = 'Fit',
                                   id = 'tabbox.2',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                       title = 'Approximate Tm',
                                       DTOutput("melt.derivative"),
                                       width = 6,
                                       icon = icon('thermometer-half')
                                   ),
                                   tabPanel(
                                       title = 'Fit initialization',
                                       hotable("hotable1"),
                                       width = 6,
                                       icon = icon('sign-out-alt')
                                   ),
                                   tabPanel(
                                       title = 'Fit result',
                                       width = 6,
                                       plotOutput("p.raw.melt.fit"),
                                       icon = icon('sign-out-alt')
                                   )
                               )
                        ),
                        column(12,
                               collapsible_tabBox(
                                   title = 'Fit results',
                                   id = 'tabbox.3',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                       title = 'Folded fraction',
                                       width = 6,
                                       plotOutput("p.folded.melt.fit")
                                   ),
                                   tabPanel(
                                       title = 'Modeled folded fraction',
                                       width = 6,
                                       plotlyOutput("p.folded.modeled")
                                   )
                               ),
                               collapsible_tabBox(
                                   title = 'Melting temperatures',
                                   id = 'tabbox.4',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                       title = 'Table',
                                       width = 6,
                                       DTOutput("fit.melt.result.summary")
                                   ),
                                   tabPanel(
                                       title = 'Plot',
                                       width = 6,
                                       plotOutput("fit.melt.result.plot")
                                   )
                               )
                        ),
                        column(12,
                               boxPlus(id = 'fit.output',
                                       title = 'Fit output',
                                       collapsible = T,
                                       collapsed = T,
                                       width = 12,
                                       DTOutput("nlfit.melt.results")
                               )
                        )

                    ),
                    absolutePanel(
                        id = "filter.melt",
                        # class = "panel panel-default",
                        top = 250, right = 600,
                        width = 200, height = 'auto',
                        draggable = TRUE, fixed = TRUE,
                        bsCollapse(id = 'bsCollapseMelt',
                                   open = 'Filter',
                                   bsCollapsePanel(
                                       'Filter',
                                       sliderInput("slider.therm",
                                                   "Filter temperatures", #Temperature for deltaG calculation
                                                   min = 275,
                                                   max = 375,
                                                   step = 0.5,
                                                   value = c(276, 363)),
                                       uiOutput("select.melting.oligo"),
                                       uiOutput("select.melting.ramp"),
                                       uiOutput("select.melting.comment"),
                                       uiOutput("select.melting.rep"),
                                       uiOutput("select.melting.id"),
                                       switchInput(inputId = "melt.merge.replicates", #initiates fit
                                                   label = "merge replicates",
                                                   value = F),
                                       sliderInput('slider.melt.rounder',
                                                   "group temperature",
                                                   min = 1,
                                                   max = 1.5,
                                                   value = 1,
                                                   step = 0.01),
                                       style = 'success'
                                   )
                        ),
                        style = "opacity: 0.9"
                    ),
                    absolutePanel(
                        id = "custom.melt",
                        # class = "panel panel-default",
                        top = 125, right = 100,
                        width = 200, height = 'auto',
                        draggable = TRUE, fixed = TRUE,
                        bsCollapse(id = 'bsCollapseTest',
                                   open = 'Customisation',
                                   bsCollapsePanel(
                                       'Customisation',
                                       uiOutput('select.melting.palette.fam'),
                                       uiOutput('select.melting.palette'),
                                       sliderInput('size.dot.melt', 'Dot size',
                                                   min=0, max=10, value=4,
                                                   step=0.25, round=0),
                                       sliderInput('alpha.dot.melt', 'Dot transparency',
                                                   min = 0, max = 1, value = 0.7,
                                                   step=0.05, round=0),
                                       sliderInput('size.line.melt', 'line size',
                                                   min=0, max=5, value=1,
                                                   step=0.1, round=0),
                                       sliderInput('alpha.line.melt', 'line transparency',
                                                   min = 0, max = 1, value = 1,
                                                   step=0.05, round=0),
                                       sliderInput('size.baseline.melt', 'baseline size',
                                                   min=0, max=5, value=1,
                                                   step=0.1, round=0),
                                       sliderInput('alpha.baseline.melt', 'baseline transparency',
                                                   min = 0, max = 1, value = 0.75,
                                                   step=0.05, round=0),
                                       style = 'primary'
                                   )
                        ),
                        style = "opacity: 0.9"
                    )
                )
            )
        )
    )


    #SERVER----
    server <- function(input, output, session) {

        options(shiny.maxRequestSize=5000*1024^2)

        #1-input----

        #Oligo selection
        selected.oligos <- reactive({

            if (is.null(input$input.info_rows_selected)) {
                return(NULL)
            } else {
                selected.oligos <- info.epsilon() %>%
                    slice(input$input.info_rows_selected) %>%
                    dplyr::select(oligo)

                selected.oligos <- as.vector(selected.oligos[[1]])

                return(selected.oligos)
            }

        })

        input.file <- reactive({
            input$raw.data.input
        })

        input.info <- reactive({
            read_excel(input.file()$datapath,
                       sheet = 'info') %>%
                mutate( #converts DOI input to hyperlink if it's not already been done
                    DOI = ifelse(substr(DOI, 1, 2)=='10',
                                 paste0("<a href=http://dx.doi.org/",DOI,">",DOI,"</a>"),
                                 DOI)
                ) %>%
                mutate(depo.date = as.Date(Sys.Date(),format='%Y/%m/%d')) #deposition date added and formatted to ISO
        })

        input.CD <- reactive({

            wide.input <- read_excel(input.file()$datapath,
                                     sheet = "CD")

            #extract descriptors
            descriptors <- wide.input %>%
                slice(1:6)

            #extract data
            raw.data <- wide.input %>%
                slice(-1:-6)

            data.collector <- data.frame()

            for (i in 1:ncol(raw.data)-1) {
                if (i %% 2 != 0) { #runs on uneven columns only
                    buffer <- raw.data %>%
                        select(i, i+1) %>% #select every couple columns
                        mutate(descriptors[[1, i+1]], #adds columns for descriptors
                               descriptors[[2, i+1]],
                               descriptors[[3, i+1]],
                               descriptors[[4, i+1]],
                               descriptors[[5, i+1]]) %>%
                        magrittr::set_colnames(c('wl', 'CD', 'oligo', 'buffer', 'cation', 'l', 'con')) %>%
                        mutate(buffer.id = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                        convert(num('wl', 'CD', 'con', 'l')) #converts some columns to numeric type
                    #binds data
                    data.collector <- rbind(data.collector, buffer,
                                            make.row.names = F)
                }
            }

            data.collector <- data.collector %>%
                mutate(cation = replace_na(cation, 'none'))

            return(data.collector)

        })

        input.NMR <- reactive({

            nmr.labels.input <- read_xlsx(input.file()$datapath,
                                          sheet = "NMR labels")

            #extract descriptors
            nmr.labels.descriptors <- nmr.labels.input %>%
                slice(1:4)

            #extract data
            nmr.labels.input <- nmr.labels.input %>%
                slice(-1:-4)

            label.collector <- data.frame()

            for (k in 1:ncol(nmr.labels.input)-1) {
                if (k %% 2 != 0) { #runs on uneven columns only
                    label.buffer <- nmr.labels.input %>%
                        select(k, k+1) %>% #select every couple columns
                        mutate(nmr.labels.descriptors[[1, k+1]], #adds columns for descriptors
                               nmr.labels.descriptors[[2, k+1]],
                               nmr.labels.descriptors[[3, k+1]]) %>%
                        magrittr::set_colnames(c('peak.number', 'shift', 'oligo', 'buffer', 'cation')) %>%
                        mutate(buffer.id = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                        mutate(rshift = shift) %>%
                        convert(num('shift')) #converts some columns to numeric type
                    #binds data
                    label.collector <- rbind(label.collector, label.buffer,
                                             make.row.names = F)
                }
            }


            label.collector <-  label.collector %>%
                drop_na(shift) %>%
                drop_na(peak.number) %>%
                mutate(rshift = shift) %>%
                select(c(1:3, 6:7))

            wide.input <- read_excel(input.file()$datapath,
                                     sheet = "NMR")

            #extract descriptors
            descriptors <- wide.input %>%
                slice(1:4)

            #extract data
            wide.input <- wide.input %>%
                slice(-1:-4)

            data.collector <- data.frame()

            for (i in 1:ncol(wide.input)-1) {
                if (i %% 2 != 0) { #runs on uneven columns only
                    buffer <- wide.input %>%
                        select(i, i+1) %>% #select every couple columns
                        mutate(descriptors[[1, i+1]], #adds columns for descriptors
                               descriptors[[2, i+1]],
                               descriptors[[3, i+1]]) %>%
                        magrittr::set_colnames(c('shift', 'int', 'oligo', 'buffer', 'cation')) %>%
                        mutate(buffer.id = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                        convert(num('shift', 'int')) #converts some columns to numeric type
                    #binds data
                    data.collector <- rbind(data.collector, buffer,
                                            make.row.names = F)
                }
            }
            buffer <- data.frame() #empty for memory
            wide.input <- data.frame() #same


            data.collector <- data.collector %>%
                mutate(cation = replace_na(cation, 'none')) %>%
                mutate(rshift = round(shift, 3)) %>%
                group_by(rshift) %>%
                left_join(label.collector,
                          by = c('oligo', 'buffer.id', unique('rshift'))) %>%
                set_colnames(c('shift', 'int', 'oligo', 'buffer', 'cation', 'buffer.id', 'rshift', 'peak.number', 'drop')) %>%
                select(c(1:8)) %>%
                group_by(oligo, buffer.id) %>%
                mutate(peak.number = replace(peak.number, duplicated(peak.number), NA)) %>%
                filter(buffer.id %in% input$select.buffer.id) %>%
                filter(buffer %in% input$select.buffer) %>%
                filter(cation %in% input$select.cation)

            return(data.collector)
        })

        input.MS <- eventReactive(input$plotMS,{

            wide.input <- read_excel(input.file()$datapath,
                                     sheet = "MS")

            #extract descriptors
            descriptors <- wide.input %>%
                slice(1:6)

            #extract data
            wide.input <- wide.input %>%
                slice(-1:-6)


            data.collector <- data.frame()

            for (i in 1:ncol(wide.input)-1) {
                if (i %% 2 != 0) { #runs on uneven columns only
                    buffer <- wide.input %>%
                        select(i, i+1) %>% #select every couple columns
                        mutate(descriptors[[1, i+1]], #adds columns for descriptors
                               descriptors[[2, i+1]],
                               descriptors[[3, i+1]],
                               descriptors[[4, i+1]],
                               descriptors[[5, i+1]]) %>%
                        magrittr::set_colnames(c('mz', 'int', 'oligo', 'buffer', 'cation', 'tune', 'rep')) %>%
                        mutate(buffer.id = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                        convert(num('mz', 'int')) #converts some columns to numeric type
                    #binds data
                    data.collector <- rbind(data.collector, buffer,
                                            make.row.names = F)
                }
            }

            wide.input <- data.frame() #empty for memory
            buffer <- data.frame() #same

            #imports user labels
            ms.labels.input <- read_xlsx(input.file()$datapath,
                                         sheet = "MS labels")

            #extract descriptors
            ms.labels.descriptors <- ms.labels.input %>%
                slice(1:6)

            #extract data
            ms.labels.input <- ms.labels.input %>%
                slice(-1:-6)

            label.collector <- data.frame()

            for (k in 1:ncol(ms.labels.input)-1) {
                if (k %% 2 != 0) { #runs on uneven columns only
                    label.buffer <- ms.labels.input %>%
                        select(k, k+1) %>% #select every couple columns
                        mutate(ms.labels.descriptors[[1, k+1]], #adds columns for descriptors
                               ms.labels.descriptors[[2, k+1]],
                               ms.labels.descriptors[[3, k+1]],
                               ms.labels.descriptors[[4, k+1]],
                               ms.labels.descriptors[[5, k+1]]) %>%
                        magrittr::set_colnames(c('charge', 'label', 'oligo', 'buffer', 'cation', 'tune', 'rep')) %>%
                        mutate(buffer.id = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                        convert(num('charge')) #converts some columns to numeric type
                    #binds data
                    label.collector <- rbind(label.collector, label.buffer,
                                             make.row.names = F)
                }
            }

            #selects calculated average MW
            info.labels <- info.mass() %>%
                select(oligo, averagemw)

            #data filtering
            data.collector <- data.collector %>%
                mutate(cation = replace_na(cation, 'none')) %>%
                filter(oligo %in% selected.oligos()) %>%
                filter(buffer.id %in% input$select.buffer.id) %>%
                filter(buffer %in% input$select.buffer) %>%
                filter(cation %in% input$select.cation)

            #data reduction with mass.diet()
            data.collector <- mass.diet(fat.mass = data.collector, baseline.int = input$baseline.int,
                                        base.start = min(input$mz.baseline.range), base.end = max(input$mz.baseline.range),
                                        range.start = min(input$mz.filter.range), range.end = max(input$mz.filter.range))

            #More filtering and normalize imported MS data
            data.collector <- data.collector %>%
                filter(mz > min(input$slide.ms) & mz < max(input$slide.ms)) %>%
                group_by(oligo, buffer.id) %>%
                mutate(int.min = min(int), int.max = max(int)) %>%
                group_by(mz, oligo, buffer.id, tune, rep) %>%
                mutate(norm.int = (int - int.min)/(int.max - int.min))


            ave.mass <- massdb %>%
                filter(atom %in% c('K', 'H')) %>%
                group_by(atom) %>%
                mutate(av.mass = ab1*mass1 + ab2*mass2 + ab3*mass3) %>%
                select(av.mass)
            ave.K <- ave.mass[[2,2]]
            ave.H <- ave.mass[[1,2]]

            #calculates m/z of user labels (based on label 'name')
            ms.labels <-  label.collector %>%
                drop_na(charge) %>%
                drop_na(label) %>%
                set_colnames(c('charge', 'species', 'oligo', 'buffer', 'cation', 'tune', 'rep', 'buffer.id')) %>%
                select(-c('buffer', 'cation')) %>%
                left_join(info.labels) %>%
                mutate(label.mass = averagemw + case_when(
                    species == 'M' ~ 0,
                    species == 'MK' ~ ave.K,
                    species == 'MK2' ~ 2*ave.K,
                    species == 'MK3' ~ 3*ave.K,
                    species == 'MK4' ~ 4*ave.K,
                    species == 'MK5' ~ 5*ave.K,
                    species == 'MK6' ~ 6*ave.K,
                    species == 'MK7' ~ 7*ave.K,
                    species == 'MK8' ~ 8*ave.K,
                    species == 'MK9' ~ 9*ave.K,
                    species == 'MK10' ~ 10*ave.K
                )
                ) %>%
                mutate(rmz = round((label.mass - charge*ave.H)/charge, 1)) %>%
                select(oligo, charge, species, rmz, buffer.id, tune, rep)

            #joins label names to corresponding oligo/buffer at the adequate m/z (based on round value)
            data.collector <- data.collector %>%
                mutate(rmz = round(mz, 1)) %>%
                group_by(rmz) %>%
                left_join(ms.labels,
                          by = c('oligo', 'buffer.id', 'tune', 'rep', unique('rmz'))) %>%
                group_by(oligo, buffer.id, charge) %>%
                #remove potentially duplicated labels for a given charge state
                mutate(species = replace(species, duplicated(paste(charge, species, oligo, tune, rep, buffer.id)), NA))

            return(data.collector)
        })

        #2-Calculations----

        #a/mass----

        info.mass <- reactive({

            #mass database
            massdb <- massdb %>%
                group_by(atom) %>%
                mutate(av.mass = ab1*mass1 + ab2*mass2 + ab3*mass3)

            #mass calculation
            input.mass <- input.info() %>%
                mutate(
                    nbA = str_count(sequence, "A"),
                    nbC = str_count(sequence, "C"),
                    nbG = str_count(sequence, "G"),
                    nbT = str_count(sequence, "T"),
                    nbN = nbA + nbC + nbG + nbT,
                    nP = nbA + nbT + nbG + nbC - 1,
                    nC = nbA*10 + nbG*10 + nbC*9 + nbT*10,
                    nH = nbA*12 + nbG*12 + nbC*12 + nbT*13 + 1,
                    nO = nbA*5 + nbG*6 + nbC*6 + nbT*7 - 2,
                    nN = nbA*5 + nbG*5 + nbC*3 + nbT*2,
                    monomw = nH * massdb$mass1[massdb$atom == "H"] +
                        nC * massdb$mass1[massdb$atom == "C"] +
                        nN * massdb$mass1[massdb$atom == "N"] +
                        nO * massdb$mass1[massdb$atom == "O"] +
                        nP * massdb$mass1[massdb$atom == "P"],
                    averagemw = nH * massdb$av.mass[massdb$atom == "H"] +
                        nC * massdb$av.mass[massdb$atom == "C"] +
                        nN * massdb$av.mass[massdb$atom == "N"] +
                        nO * massdb$av.mass[massdb$atom == "O"] +
                        nP * massdb$av.mass[massdb$atom == "P"]
                )
        })


        #b/UV----

        #calculates epsilon for each oligo
        info.epsilon <- reactive({

            info.epsilon <- info.mass() %>%
                add_column(ext.coeff.260 = NULL)

            info.epsilon$ext.coeff.260 <- lapply(info.mass()$sequence, epsilon.calculator)

            return(info.epsilon)
        })

        #c/CD----
        calc.cd <- reactive({
            input.CD() %>%
                filter(buffer.id %in% input$select.buffer.id) %>%
                filter(buffer %in% input$select.buffer) %>%
                filter(cation %in% input$select.cation) %>%
                filter(wl > min(input$slide.cd) & wl < max(input$slide.cd)) %>%
                group_by(oligo, buffer.id, wl, l, con) %>%
                mutate(delta.epsilon = CD/(32980*con/1000000*l)) %>%
                mutate(plot.y = if_else(isTRUE(input$cd.data.select), delta.epsilon, CD))
        })


        #d/NMR----
        #in input

        #e/UV-melting----

        #export UV-melting data to importR
        calc.UV <- eventReactive(input$bttn.uv.export, {

            if (isTRUE(input$fit.or.raw)) {
                calc.UV <- fit.melt.result.df() #export of fitted data as is
            } else {
                calc.UV <- melt.filtered() %>% #export of raw data
                    group_by(id) %>%
                    mutate(nb.data.pt = NA, #generation of missing columns (kept empty)
                           init.Tm = NA,
                           RSS = NA,
                           SE.residual = NA,
                           P1 = NA, P1SD = NA,
                           P2 = NA, P2SD = NA,
                           P3 = NA, P3SD = NA,
                           P4 = NA, P4SD = NA,
                           P5 = NA, P5SD = NA,
                           P6 = NA, P6SD = NA,
                           fit.Tm.K = NA, fit.Tm.C = NA,
                           DeltaH = NA, DeltaS = NA,
                           folded.fraction = NA) %>%
                    #calculation of a normalized absobance in lieu of the folded fraction
                    mutate(min.abs = min(abs.melt),
                           max.abs = max(abs.melt)) %>%
                    group_by(id, T.K) %>%
                    mutate(folded.fraction.base = (abs.melt - min.abs)/(max.abs - min.abs)) %>%
                    select(-c(min.abs, max.abs)) %>%
                    mutate(raw.fit.y = NA,
                           low.T.baseline = NA, high.T.baseline = NA)
            }

            #data filtering
            calc.UV <- calc.UV %>%
                filter(comment %in% input$select.buffer.id) %>%
                filter(buffer %in% input$select.buffer) %>%
                filter(cation %in% input$select.cation) %>%
                filter(oligo %in% selected.oligos())
        })

        #3-outputs----

        #a/inputs----

        output$select.tune <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.tune",
                            label = "Tunes",
                            choices = "upload data first",
                            multiple = F
                )
            } else {
                pickerInput("select.tune",
                            label = "Tunes",
                            choices = unique(input.MS()$tune),
                            selected = unique(input.MS()$tune)[[1]], #first tune name selected by default
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        output$select.rep <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.rep",
                            label = "Replicates",
                            choices = "upload data first",
                            multiple = F
                )
            } else {
                pickerInput("select.rep",
                            label = "Replicates",
                            choices = unique(input.MS()$rep),
                            selected = unique(input.MS()$rep)[[1]], #first rep name selected by default
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        output$select.oligo <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.oligo",
                            label = "Oligonucleotide",
                            choices = "upload data first",
                            multiple = T
                )
            } else {
                pickerInput("select.oligo",
                            label = "Oligonucleotides",
                            choices = unique(info.mass()$oligo),
                            selected = unique(info.mass()$oligo),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        buffer.list <- reactive({



            buffer.collect <- data.frame(buffers = unique(input.CD()$buffer)) %>%
                # rbind(buffers = unique(input.UV.melting()$buffer)) %>%
                unique()

            buffer.list <- as.vector(buffer.collect$buffer)

            return(buffer.list)
        })

        output$buffer.list <- DT::renderDT({
            buffer.list()
        })

        output$select.buffer <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.buffer",
                            label = "Electrolyte",
                            choices = "upload data first",
                            multiple = T
                )
            } else {
                pickerInput("select.buffer",
                            label = "Electrolyte",
                            choices = buffer.list(),
                            selected = buffer.list(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        buffer.id.list <- reactive({

            buffer.id.collect <- data.frame(buffer.ids = unique(input.CD()$buffer.id)) %>%
                # rbind(buffers = unique(input.NMR()$buffer)) %>%
                unique()

            buffer.id.list <- as.vector(buffer.id.collect$buffer.ids)

            return(buffer.id.list)
        })

        output$buffer.id.list <- DT::renderDT({
            buffer.id.list()
        })

        output$select.buffer.id <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.buffer.id",
                            label = "Buffer",
                            choices = "upload data first",
                            multiple = T
                )
            } else {
                pickerInput("select.buffer.id",
                            label = "Buffer",
                            choices = buffer.id.list(),
                            selected = buffer.id.list(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        cation.list <- reactive({

            cation.collect <- data.frame(cations = unique(input.CD()$cation)) %>%
                # rbind(buffers = unique(input.NMR()$buffer)) %>%
                unique()

            cation.list <- as.vector(cation.collect$cations)

            return(cation.list)
        })

        output$cation.list <- DT::renderDT({
            cation.list()
        })

        output$select.cation <- renderUI({
            if(is.null(input$raw.data.input)) {
                pickerInput("select.cation",
                            label = "Cation",
                            choices = "upload data first",
                            multiple = T
                )
            } else {
                pickerInput("select.cation",
                            label = "Cation",
                            choices = cation.list(),
                            selected = cation.list(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        #b/tables----

        output$input.info <- DT::renderDT({
            if(is.null(input$raw.data.input)) {return(NULL)}else{

                info.epsilon() %>%
                    setcolorder(c('oligo', 'DOI', 'submitted_by', 'depo.date',
                                  'sequence', 'nbN', 'averagemw', 'monomw',
                                  'ext.coeff.260')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = F, #need to be false to get hyperlink of DOI parsed
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c('Monoisotopic mass' = 'monomw',
                                     'Average mass' = 'averagemw',
                                     'Length' = 'nbN',
                                     'Adenine' = 'nbA',
                                     'Guanine' = 'nbG',
                                     'Cytosine' = 'nbC',
                                     'Thymine' = 'nbT',
                                     'Phosphorus' = 'nP',
                                     'Hydrogen' = 'nH',
                                     'Carbon' = 'nC',
                                     'Nitrogen' = 'nN',
                                     'Oxygen' = 'nO',
                                     'Extinction coefficient (260 nm)' = 'ext.coeff.260',
                                     'Deposition date' = 'depo.date',
                                     'Oligonucleotide' = 'oligo',
                                     'Sequence' = 'sequence',
                                     'Submitted by' = 'submitted_by'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(10:18)))
                        )
                    ) %>%
                    formatRound(c('Monoisotopic mass'),
                                digits = 5) %>%
                    formatRound(c('Average mass'),
                                digits = 2)
            }
        })

        output$input.UV.melting <- DT::renderDT(server=FALSE,{
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                calc.UV() %>%
                    filter(oligo %in% selected.oligos()) %>%
                    mutate(blk.sub = ifelse(blk.sub == 1, 'yes', 'no')) %>%
                    setcolorder(c('id', 'oligo', 'buffer', 'cation', 'comment', 'ramp', 'rep',
                                  'T.unk', 'T.K', 'abs.raw', 'abs.blk', 'blk.sub',
                                  'abs.melt', 'raw.fit.y', 'folded.fraction.base', 'folded.fraction')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'comment',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Ramp' = 'ramp',
                            'T (K)' = 'T.K',
                            'Epsilon' = 'abs.melt',
                            'Folded fraction' = 'folded.fraction.base',
                            'Absorbance' = 'abs.raw',
                            'Modeled folded fraction' = 'folded.fraction',
                            'Modeled absorbance' = 'raw.fit.y',
                            'Blank absorbance' = 'abs.blk',
                            'Input T' = 'T.unk',
                            'Blank subtraced?' = 'blk.sub',
                            'Replicate' = 'rep'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(0,2,3,7,9:13,15:37)))
                        )
                    ) %>%
                    formatRound(c('Modeled folded fraction', 'Folded fraction', 'Epsilon',
                                  'Absorbance', 'Modeled absorbance', 'Blank absorbance'),
                                digits = 3)
            }
        })



        output$input.CD <- DT::renderDT(server=FALSE,{
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                calc.cd() %>%
                    setcolorder(c('oligo', 'buffer.id')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        escape = T,
                        rownames = F,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Wavelength (nm)' = 'wl',
                            'CD (mdeg)' = 'CD',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Buffer' = 'buffer.id',
                            'Oligonucleotide' = 'oligo',
                            'Path length (cm)' = 'l',
                            'Concentration (microM)' = 'con',
                            'Delta Epsilon (M-1 cm-1)' = 'delta.epsilon',
                            'Plotted data' = 'plot.y'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(4, 5, 6, 7, 8, 9)))
                        )
                    ) %>%
                    formatRound(c('CD (mdeg)', 'Delta Epsilon (M-1 cm-1)', 'Plotted data'),
                                digits = 2)
            }
        })

        output$input.NMR <- DT::renderDT(server=FALSE,{
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                input.NMR() %>%
                    setcolorder(c('oligo', 'buffer.id', 'shift', 'int')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Intensity' ='int',
                            'Chemical shift (ppm)'='shift',
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'buffer.id',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(4, 5, 6, 7)))
                        )
                    )
            }
        })

        output$input.MS <- DT::renderDT({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                input.MS() %>%
                    setcolorder(c('oligo', 'buffer.id', 'tune', 'rep', 'mz', 'norm.int')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'm/z' = 'mz',
                            'Intensity' = 'int',
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'buffer.id',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Normalized intensity' = 'norm.int',
                            'Tune' = 'tune',
                            'Replicate' = 'rep'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(7:13)))
                        )
                    ) %>%
                    formatRound(c('Normalized intensity'), digits = 3)
            }
        })

        #c/figures----

        #extraction of number of rows in grid
        row.p.CD <- reactive({
            gg_facet_nrow_ng(p.CD())
        })

        output$p.CD <- renderPlot({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                if (is.null(selected.oligos())) {return(NULL)}else{
                    return(p.CD())
                }
            }
        })

        p.CD <- reactive({
            p.CD <- calc.cd() %>%
                filter(oligo %in% selected.oligos()) %>%
                ggplot(aes(x = wl, y = plot.y, color = buffer.id, shape = oligo)) +
                geom_point(size = input$cd.size.pt,
                           alpha = input$cd.alpha.pt) +
                theme_pander() +
                xlab("Wavelength (nm)") +
                labs(colour = "buffer",
                     shape = 'oligo')


            if (input$cd.superimpose == "none") {
                if (input$cd.free == 'free') {
                    p.CD <- p.CD +
                        facet_grid(buffer.id~oligo,
                                   scales = 'free_y')
                } else {
                    p.CD <- p.CD +
                        facet_grid(buffer.id~oligo)
                }
            }

            if (input$cd.superimpose == "oligos") {
                if (input$cd.free == 'free') {
                    p.CD <- p.CD +
                        facet_grid(~oligo,
                                   scales = 'free_y')
                } else {
                    p.CD <- p.CD +
                        facet_grid(~oligo)
                }
            }

            if (input$cd.superimpose == "buffer") {
                if (input$cd.free == 'free') {
                    p.CD <- p.CD +
                        facet_grid(~buffer.id,
                                   scales = 'free_y')
                } else {
                    p.CD <- p.CD +
                        facet_grid(~buffer.id)
                }
            }

            if (isTRUE(input$cd.data.select)) {
                p.CD <- p.CD + ylab(expression(paste(Delta*epsilon, ' (M'^{-1},'cm'^{-1}, ')')))
            } else {
                p.CD <- p.CD + ylab('mdeg')
            }

            p.CD <- palette.modifier(plot = p.CD)

            return(p.CD)

        })

        output$p.CD.ui <- renderUI({
            if (is.null(selected.oligos())) {return(NULL)}else{
                plotOutput("p.CD",
                           height = 300 * row.p.CD())}
        })

        row.p.NMR <- reactive({
            gg_facet_nrow_ng(p.NMR())
        })

        output$p.NMR <- renderPlot({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                if (is.null(selected.oligos())) {return(NULL)}else{
                    return(p.NMR())
                }
            }
        })

        p.NMR <- reactive({

            nmr.bounds <- input.NMR() %>%
                filter(oligo %in% selected.oligos()) %>%
                filter(shift > min(input$slide.nmr) & shift < max(input$slide.nmr)) %>%
                group_by(oligo) %>% #y-scale normalization (helps with labelling y-scale limits and spectra comparisons)
                mutate(int = (int - min(int))/(max(int) - min(int)))

            limits <- c(0.8*max(nmr.bounds$int), 1.3*max(nmr.bounds$int)) #y-scale labelling limits

            p.NMR <- nmr.bounds %>%
                mutate(peak.number = if_else(is.na(peak.number), "", peak.number)) %>% #assigns empty labels to avoid label over data points
                ggplot(aes(x = shift, y = int, color = oligo)) +
                geom_line(size = input$nmr.size.line.db) +
                geom_text_repel(aes(x = shift, y = int, label = peak.number,
                                    color = oligo, segment.color = oligo),
                                force = 2,
                                direction = 'y',
                                min.segment.length = 0.15,
                                segment.size = 0.5,
                                box.padding = 1,
                                alpha = 1,
                                size = 6,
                                fontface = 'bold',
                                show.legend = F,
                                ylim = limits
                ) +
                scale_x_reverse() + #inverted x scale for chemical shift
                theme_pander() +
                xlab("Chemical shift (ppm)") +
                theme(
                    axis.text.y = element_blank(),
                    axis.title.y = element_blank()
                ) + #allows for some extra space on the y-scale for labelling
                coord_cartesian(ylim = c(min(nmr.bounds$int), max(nmr.bounds$int)*1.2))

            if (input$nmr.superimpose == "none") {
                if (input$nmr.free == 'free') {
                    p.NMR <- p.NMR +
                        facet_grid(buffer.id~oligo,
                                   scales = 'free_y')
                } else {
                    p.NMR <- p.NMR +
                        facet_grid(oligo~buffer.id)
                }
            }

            if (input$nmr.superimpose == "oligos") {
                if (input$nmr.free == 'free') {
                    p.NMR <- p.NMR +
                        facet_grid(~oligo,
                                   scales = 'free_y')
                } else {
                    p.NMR <- p.NMR +
                        facet_grid(~oligo)
                }
            }

            if (input$nmr.superimpose == "buffer") {
                if (input$nmr.free == 'free') {
                    p.NMR <- p.NMR +
                        facet_grid(~buffer.id,
                                   scales = 'free_y')
                } else {
                    p.NMR <- p.NMR +
                        facet_grid(~buffer.id)
                }
            }

            p.NMR <- palette.modifier(plot = p.NMR)

            return(p.NMR)

        })

        output$p.NMR.ui <- renderUI({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                if (is.null(selected.oligos())) {return(NULL)}else{
                    plotOutput("p.NMR",
                               height = 300 * row.p.NMR())
                }
            }
        })

        #extraction of number of rows in grid
        row.p.MS <- reactive({
            gg_facet_nrow_ng(p.MS())
        })

        p.MS <- reactive({

            p.MS <- input.MS() %>%
                filter(tune %in% input$select.tune) %>% #filter by tune name
                filter(rep %in% input$select.rep) %>% #filter by replicate number
                ggplot(aes(x = mz, y = norm.int)) +
                theme_pander() +
                theme(
                    legend.position = "right",
                    axis.text.y = element_blank()
                ) +
                xlab("m/z") +
                ylab("") +
                labs(colour = "legend")


            if (input$ms.superimpose == "oligo x buffer") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(oligo~buffer.id,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(buffer.id~oligo,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("replicate:", rep,
                                                ", tune:", tune))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("replicate:", rep,
                                                     ", tune:", tune)),
                                   show.legend = F)
                }

            }

            if (input$ms.superimpose == "oligo x tune") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(oligo~tune,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(tune~oligo,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("replicate:", rep,
                                                ", buffer:", buffer.id))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("replicate:", rep,
                                                     ", buffer:", buffer.id)),
                                   show.legend = F)
                }
            }

            if (input$ms.superimpose == "oligo x replicate") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(oligo~rep,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(rep~oligo,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("buffer:", buffer.id,
                                                ", tune:", tune))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("buffer:", buffer.id,
                                                     ", tune:", tune)),
                                   show.legend = F)
                }
            }

            if (input$ms.superimpose == "buffer x tune") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(buffer.id~tune,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(tune~buffer.id,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("oligo:", oligo,
                                                ", replicate:", rep))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("oligo:", oligo,
                                                     ", replicate:", rep)),
                                   show.legend = F)
                }
            }

            if (input$ms.superimpose == "buffer x replicate") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(buffer.id~rep,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(rep~buffer.id,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("oligo:", oligo,
                                                ", tune:", tune))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("oligo:", oligo,
                                                     ", tune:", tune)),
                                   show.legend = F)
                }
            }

            if (input$ms.superimpose == "tune x replicate") {
                if(isFALSE(input$switch.grid.ms)){
                    p.MS <- p.MS + facet_grid(tune~rep,
                                              scales = "free_y")
                } else {
                    p.MS <- p.MS + facet_grid(rep~tune,
                                              scales = "free_y")
                }

                p.MS <- p.MS +
                    geom_line(size = input$ms.size.line,
                              aes(color = paste("oligo:", oligo,
                                                ", buffer:", buffer.id))
                    )

                if (isTRUE(input$switch.label.ms)) {
                    p.MS <- p.MS  +
                        geom_label(aes(label = species, y = 1,
                                       color = paste("oligo:", oligo,
                                                     ", buffer:", buffer.id)),
                                   show.legend = F)
                }
            }


            p.MS <- palette.modifier(plot = p.MS)

            return(p.MS)
        })



        output$p.MS <- renderPlot({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                return(p.MS())
            }
        })

        output$p.MS.ui <- renderUI({
            if(is.null(input$raw.data.input)) {return(NULL)}else{
                plotOutput("p.MS",
                           height = 300*row.p.MS())
            }
        })

        #4-UV-melting----

        #allows to toggle the blank subtraction on/off
        blk.subtract <- reactive({
            if (input$melt.blank == T) {
                blk.subtract = 1
            } else {
                blk.subtract = 0
            }
        })

        melt <- reactive({

            wide.input <- read_excel(input.file()$datapath,
                                     sheet = "UV-melting")

            #extract descriptors
            descriptors <- wide.input %>%
                slice(1:7)

            #extract data
            raw.data <- wide.input %>%
                slice(-1:-7)

            data.collector <- data.frame()

            for (i in 1:(ncol(raw.data)/3)) {

                n <- 1+3*(i-1) #converts i to the temperature column index (starts at 1 then increase by 3)

                buffer <- raw.data %>%
                    select(n, n+1, n+2) %>% #select every couple column group
                    mutate(descriptors[[1, n+1]], #adds columns for descriptors
                           descriptors[[2, n+1]],
                           descriptors[[3, n+1]],
                           descriptors[[4, n+1]],
                           descriptors[[5, n+1]],
                           descriptors[[6, n+1]]) %>%
                    magrittr::set_colnames(c('T.unk', 'abs.raw', 'abs.blk', 'oligo', 'buffer', 'cation', 'rep', 'melt.l', 'melt.c')) %>%
                    mutate(comment = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                    convert(num('T.unk', 'abs.raw', 'abs.blk', 'melt.l', 'melt.c')) %>% #converts some columns to numeric type
                    drop_na('T.unk')

                #binds data
                data.collector <- rbind(data.collector, buffer,
                                        make.row.names = F)

            }


            melt.buffer <- data.collector %>%
                filter(!is.na(oligo)) %>% #removes empty lines
                group_by(oligo, comment, rep) %>%
                mutate(ramp = if_else(lead(T.unk) > T.unk, 'heating', 'cooling')) %>% #ramp determination
                #no ramp found for last row of each spl (because there's no next T value)
                mutate(ramp = if_else(is.na(ramp), lag(ramp), ramp)) %>%
                ungroup() %>%  #necessary to use data at derivative step (not sure why)
                mutate(id = paste(oligo, comment, ramp, rep, sep = '-'))%>% #create an experiment id
                # Detects whether the raw data is supplied in Celsius or Kelvin and converts to Kelvin if necessary
                mutate(T.K = if_else(abs.raw < 100, T.unk + 273.15, T.unk)) %>%
                add_column(blk.sub = blk.subtract()) %>%
                group_by(id) %>%
                #subtract the blank column is values are provided and toggle activated + converts to molar absorbtion coeff
                mutate(abs.melt = if_else(is.na(abs.blk), abs.raw/(melt.c/1E6 * melt.l),
                                          if_else(blk.sub == 1, (abs.raw - abs.blk)/(melt.c/1E6 * melt.l), abs.raw/(melt.c/1E6 * melt.l)))) %>%
                ungroup()

            return(melt.buffer)
        })

        #melting data selection-----

        output$select.melting.oligo <- renderUI({
            # if(is.null(input$input.file)) {
            #     pickerInput("select.melting.oligo",
            #                 label = "Choose oligos",
            #                 choices = "upload data first",
            #                 multiple = T
            #     )
            # } else {
            pickerInput("select.melting.oligo",
                        label = "Choose oligos",
                        choices = unique(melt()$oligo),
                        selected = unique(melt()$oligo),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
            # }
        })

        output$select.melting.ramp <- renderUI({
            # if(is.null(input$input.file)) {
            #     pickerInput("select.melting.oligo",
            #                 label = "Choose ramps",
            #                 choices = "upload data first",
            #                 multiple = T
            #     )
            # } else {
            pickerInput("select.melting.ramp",
                        label = "Choose ramps",
                        choices = unique(melt()$ramp),
                        selected = unique(melt()$ramp),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
            # }
        })

        output$select.melting.comment <- renderUI({
            # if(is.null(input$input.file)) {
            #     pickerInput("select.melting.oligo",
            #                 label = "Choose comments",
            #                 choices = "upload data first",
            #                 multiple = T
            #     )
            # } else {
            pickerInput("select.melting.comment",
                        label = "Choose buffer",
                        choices = unique(melt()$comment),
                        selected = unique(melt()$comment),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
            # }
        })

        output$select.melting.rep <- renderUI({
            # if(is.null(input$input.file)) {
            #     pickerInput("select.melting.oligo",
            #                 label = "Choose replicates",
            #                 choices = "upload data first",
            #                 multiple = T
            #     )
            # } else {
            pickerInput("select.melting.rep",
                        label = "Choose replicates",
                        choices = unique(melt()$rep),
                        selected = unique(melt()$rep),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
            # }
        })

        output$select.melting.id <- renderUI({
            # if(is.null(input$input.file)) {
            #     pickerInput("select.melting.oligo",
            #                 label = "Choose id",
            #                 choices = "upload data first",
            #                 multiple = T
            #     )
            # } else {
            pickerInput("select.melting.id",
                        label = "Choose id",
                        choices = unique(melt()$id),
                        selected = unique(melt()$id),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
            # }
        })

        #melting data display----

        melt.filtered <- reactive({

            # if(is.null(input$input.file)) {
            #     return(NULL)
            # } else {

            melt.filtered.buffer <-  melt() %>% #input data filtering
                filter(oligo %in% input$select.melting.oligo) %>%
                filter(ramp %in% input$select.melting.ramp) %>%
                filter(comment %in% input$select.melting.comment) %>%
                filter(rep %in% input$select.melting.rep) %>%
                filter(id %in% input$select.melting.id) %>%
                filter(T.K > min(input$slider.therm) & T.K < max(input$slider.therm))

            if(input$melt.merge.replicates == T){
                melt.filtered.buffer <- melt.filtered.buffer %>%
                    mutate(rounded.T.K = RoundTo(T.K, multiple = input$slider.melt.rounder, FUN = round)) %>%
                    group_by(oligo, ramp, comment, rounded.T.K) %>%
                    mutate(abs.melt = mean(abs.melt), T.K = mean(T.K)) %>%
                    mutate(id = paste(oligo, comment, ramp, sep="-"))
            }
            return(melt.filtered.buffer)
            # }

        })

        output$melt.filtered <- DT::renderDT(server=FALSE,{
            melt.filtered()
            # melt()
        })

        output$p.melt.filtered <- renderPlot({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p45 <- ggplot(data = melt.filtered(),
                          aes(x = T.K, y = abs.melt, color = id, shape = ramp)) +
                geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                scale_color_d3() +
                theme_pander() +
                xlab("Temperature (K)") +
                ylab(bquote(epsilon*' ('*M^-1~cm^-1*')'))  #modifies axes titles

            # p45 <- melt.palette.modifier(plot = p45)

            return(p45)
            # }
        })

        #Derivatives--------

        melt.derivative <- eventReactive(input$bttn.deriv.melt,{

            melt.derivative.calc <- data.frame() #initialize data frame for loop result collection

            for (i in unique(melt.filtered()$id)) {

                #extract data per id
                buffer.melt <- melt.filtered() %>%
                    filter(i == melt.filtered()$id)

                #calculates differences
                diffy <- diff(buffer.melt$abs.melt)
                diffx <- diff(buffer.melt$T.K)

                melt.derivative.calc.buffer <- cbind(diffy, diffx) %>%
                    as.data.frame() %>%
                    mutate(diffyx = diffy/diffx) %>% #calculates derivative
                    add_column(id = i) %>% #adds id
                    #adds temperatures (and removes first row to match number of rows from differences)
                    add_column(T.K = buffer.melt$T.K[2:length(buffer.melt$T.K)],
                               ramp = buffer.melt$ramp[2:length(buffer.melt$ramp)])

                #result collection
                melt.derivative.calc <- base::rbind(melt.derivative.calc.buffer, melt.derivative.calc)

            }

            #switches UI tab automatically to derivative when calculating it
            observeEvent(input$bttn.deriv.melt, {
                updateTabsetPanel(session = session,
                                  inputId = "tabbox.1",
                                  selected = 'Derivative plot'
                )
            })

            #Smoothing and removal of extrema
            melt.derivative <- melt.derivative.calc %>%
                group_by(id) %>%
                mutate(rM = abs(rollmean(diffyx, input$melt.deriv.smooth.width, fill = NA, align="right"))) %>% #rolling average
                slice((input$melt.deriv.smooth.width+1):(length(rM)-(input$melt.deriv.smooth.width+1))) #removes extrema

            return(melt.derivative)

        })

        #plot derivatives
        output$p.melt.derivative <- renderPlot({

            p46 <- ggplot(melt.derivative(), aes(T.K, rM, color = id, shape = ramp)) +
                geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                theme_pander() +
                scale_color_d3(palette = "category20") +
                xlab("Temperature (K)") +
                ylab(bquote(Delta*epsilon*'/'*Delta*'T ('*M^-1~cm^-1*K^-1*')'))

            # p46 <- melt.palette.modifier(plot = p46)

            return(p46)

        })

        Tm.init.deriv <- reactive({
            melt.derivative() %>%
                group_by(id) %>%
                filter(rM == max(rM)) %>%
                select(id, T.K)
        })

        output$melt.derivative <- DT::renderDT({
            Tm.init.deriv()
        })

        tm.init0 <- eventReactive(input$bttn.init.melt, {
            tm.init0 <- Tm.init.deriv() %>%
                rename("Tm.init" = "T.K") %>%
                add_column(P1.init = 1.3e+05,
                           P3.init = 1,
                           P4.init = 0.3,
                           P5.init = 0,
                           P6.init = -0.2)

            tm.init0$legend = tm.init0$id

            return(tm.init0)

        })


        tm.init.change <- reactive({
            as.data.frame(hot.to.df(input$hotable1))
        })

        output$hotable1 <- renderHotable({tm.init0() }, readOnly = F)

        #switches UI tab automatically to hottable when initializing it
        observeEvent(input$bttn.init.melt, {
            updateTabsetPanel(session = session,
                              inputId = "tabbox.2",
                              selected = 'Fit initialization'
            )
        })

        #switches UI tab automatically to hottable when initializing it
        observeEvent(input$bttn.fit.melt, {
            updateTabsetPanel(session = session,
                              inputId = "tabbox.2",
                              selected = 'Fit result'
            )
        })





        #Fitting----------------------------

        nlfit.melt <- eventReactive(input$bttn.fit.melt, {

            #initialize the data.frame to collect results
            fit.melt.results <- data.frame()

            #loops across all unique selected ids
            for (i in unique(melt.filtered()$id)) {

                #buffers the data to fit
                fit.melt.input.buffer <- data.frame(melt.filtered()) %>%
                    filter(id == i)

                #initialize Parameters
                fit.melt.init.par <- subset(tm.init.change(), id == i)

                melt.c <- unique(fit.melt.input.buffer$melt.c) #oligo concentration
                melt.l <- unique(fit.melt.input.buffer$melt.l) #path length

                P1s <- as.vector(fit.melt.init.par$P1.init)
                P2s <- as.vector(fit.melt.init.par$Tm.init)
                P3s <- as.vector(fit.melt.init.par$P3.init)/(melt.c/1E6 * melt.l) #convert initial parameters to molar abs coeff.
                P4s <- as.vector(fit.melt.init.par$P4.init)/(melt.c/1E6 * melt.l)
                P5s <- as.vector(fit.melt.init.par$P5.init)/(melt.c/1E6 * melt.l)
                P6s <- as.vector(fit.melt.init.par$P6.init)/(melt.c/1E6 * melt.l)

                #fit
                ms <- nls(
                    data=fit.melt.input.buffer,
                    fit.melt.input.buffer$abs.melt~(P3+P4*fit.melt.input.buffer$T.K)*1/(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K)))+
                        (P5+P6*fit.melt.input.buffer$T.K)*exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))
                    /(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))),
                    start = list(P1 = P1s, P2 = P2s, P3=P3s, P4=P4s, P5=P5s, P6=P6s),
                    nls.control(maxiter = input$nb.it.melt.fit,
                                warnOnly = T)
                )

                #buffers the fit results
                fit.melt.output.buffer <- data.frame(id = i,
                                                     nb.data.pt = nobs(ms),
                                                     init.Tm =  P2s,
                                                     RSS = sum(residuals(ms)^2),
                                                     SE.residual = sigma(ms),
                                                     P1 = as.vector(coef(ms))[1],
                                                     P1SD = summary(ms)$coefficient[1,2],
                                                     P2 = as.vector(coef(ms))[2],
                                                     P2SD = summary(ms)$coefficient[2,2],
                                                     P3 = as.vector(coef(ms))[3],
                                                     P3SD = summary(ms)$coefficient[3,2],
                                                     P4 = as.vector(coef(ms))[4],
                                                     P4SD = summary(ms)$coefficient[4,2],
                                                     P5 = as.vector(coef(ms))[5],
                                                     P5SD = summary(ms)$coefficient[5,2],
                                                     P6 = as.vector(coef(ms))[6],
                                                     P6SD = summary(ms)$coefficient[6,2],
                                                     fit.Tm.K = round(as.vector(coef(ms))[2], 2),
                                                     fit.Tm.C = round(as.vector(coef(ms))[2] - 273.15, 2),
                                                     DeltaH = -as.vector(coef(ms))[1],
                                                     DeltaS = -as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                                     # DeltaG = as.vector(coef(ms))[1] - input$slider.therm * as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                                     # DeltaG = -8.314 * input$slider.therm * log(as.vector(coef(ms))[1] * (1 - input$slider.therm/as.vector(coef(ms))[2])/8.314 * input$slider.therm)
                )


                #row bind the results acroos the loop
                fit.melt.results <- rbind(fit.melt.results, fit.melt.output.buffer)
            }

            return(fit.melt.results)
        })

        #fit results table output
        output$nlfit.melt.results <- DT::renderDT(server=FALSE,{
            datatable(
                nlfit.melt(),
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                colnames = c("Data points" = "nb.data.pt",
                             'Initial Tm' = 'init.Tm',
                             "P1 sd" = 'P1SD',
                             "P2 sd" = 'P2SD',
                             "P3 sd" = 'P3SD',
                             "P4 sd" = 'P4SD',
                             "P5 sd" = 'P5SD',
                             "P6 sd" = 'P6SD',
                             "RMSE" = "SE.residual"),
                rownames = F,
                escape = T,
                filter = 'top',
                autoHideNavigation = T,
                options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = F,
                    pageLength = 25,
                    autoWidth = F,
                    dom = 'Bfrtip', #button position
                    buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                    columnDefs = list(list(visible=FALSE, targets=c(3,6, 8, 10, 12, 14, 16, 17, 18, 19, 20)))
                )
            ) %>%
                formatRound(c("P4", 'P4 sd', "P6", 'P6 sd', 'RMSE'), digits = 5) %>%
                formatRound(c('P1', 'P1 sd'), digits = 0) %>%
                formatRound(c('P2', 'P2 sd'), digits = 2) %>%
                formatRound(c('P3', 'P3 sd', 'P5', 'P5 sd'), digits = 3) %>%
                formatRound(c('RSS'), digits = 6)
        })


        fit.melt.result.df <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            left_join(melt.filtered(),nlfit.melt(),
                      by = c("id")) %>% #join fit result with raw data (only selected ids)
                mutate(folded.fraction = (1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K))))) %>%  #folded fraction
                mutate(folded.fraction.base = (P5+P6*T.K-abs.melt)/(P5+P6*T.K - P3-P4*T.K)) %>% #baseline corrected folded fraction
                #fitted line
                mutate(raw.fit.y = (P3+P4*T.K)*1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))+(P5+P6*T.K)*exp(-P1*(1-T.K/P2)/(8.31451*T.K))/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))) %>%
                mutate(low.T.baseline = P3+P4*T.K) %>%
                mutate(high.T.baseline = P5+P6*T.K) %>%
                filter(T.K > min(input$slider.therm) & T.K < max(input$slider.therm))
            # }
        })

        fit.melt.result.summary <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            fit.melt.result.df() %>%
                select(id, oligo, ramp, comment, rep, fit.Tm.K, fit.Tm.C, P2SD, DeltaH, DeltaS) %>%
                distinct() %>%
                group_by(id) %>%
                mutate(DeltaG = DeltaH - input$temp.therm * DeltaS) %>%
                group_by(oligo, ramp, comment) %>%
                mutate(mean.Tm.K = mean(fit.Tm.K), mean.Tm.C = mean(fit.Tm.C),
                       sd.Tm.K = SD(fit.Tm.K), sd.Tm.C = SD(fit.Tm.C))
            # }
        })

        #outputs the fitted raw data
        p.raw.melt.fit <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p0 <- ggplot(fit.melt.result.df()) +
                geom_point(aes(T.K, abs.melt, color = id), size = input$size.dot.melt, alpha = input$alpha.dot.melt, shape = 16) + #plots the experimental data
                geom_line(aes(x = T.K, y = raw.fit.y, color = id),
                          size = input$size.line.melt, alpha = input$alpha.line.melt) +
                ylab(bquote(epsilon*' ('*M^-1~cm^-1*')')) + #modifies axes titles
                xlab("Temperature (K)") +
                # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
                labs(color="id") +
                # scale_color_d3(palette = "category20") +
                theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
                theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                      axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
                theme(legend.position="right",
                      legend.box = "vertical",
                      legend.title = element_text(size=14,
                                                  face="bold"),
                      legend.key = element_rect(fill = "white"),
                      legend.text = element_text(size=12,
                                                 face="bold")) +
                theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
                theme(
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.75)) +
                theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
                theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
                theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
                coord_cartesian(clip = "off")  #no clipping

            #toggles baselines on and off
            if (input$toggle.baseline == T) {
                p0 <-  p0 + geom_line(aes(x = T.K, y = low.T.baseline, color = id),
                                      size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed") +
                    geom_line(aes(x = T.K, y = high.T.baseline, color = id),
                              size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed")
            } else { p0 }

            p0 <- melt.palette.modifier(plot = p0)

            return(p0)
            # }
        })

        output$p.raw.melt.fit <- renderPlot({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p.raw.melt.fit()
            # }
        })

        #outputs a plot of the modeled folded fraction
        p.folded.modeled <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p44 <- ggplot(fit.melt.result.df()) +
                geom_point(aes(T.K, folded.fraction, color = id),
                           size = input$size.dot.melt-2, alpha = input$alpha.dot.melt,
                           shape = 16) + #plots the experimental data
                ylab("folded fraction") + #modifies axes titles
                xlab("Temperature (K)") +
                # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
                labs(color="id") +
                # scale_color_d3(palette = "category20") +
                theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
                theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                      axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
                theme(legend.position="right",
                      legend.box = "vertical",
                      legend.title = element_text(size=14,
                                                  face="bold"),
                      legend.key = element_rect(fill = "white"),
                      legend.text = element_text(size=12,
                                                 face="bold")) +
                theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
                theme(
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.75)) +
                theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
                theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
                theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
                coord_cartesian(clip = "off")  #no clipping

            p44 <- melt.palette.modifier(plot = p44)

            return(p44)
            # }
        })

        output$p.folded.modeled <- renderPlotly({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p.folded.modeled()
            # }
        })

        #plots the baseline subtracted data
        p.folded.melt.fit <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p43 <- ggplot(fit.melt.result.df()) +
                geom_point(aes(T.K, folded.fraction.base, color = id),
                           size = input$size.dot.melt, alpha = input$alpha.dot.melt,
                           shape = 16) + #plots the experimental data
                ylab(bquote(bold("folded fraction"))) + #modifies axes titles
                xlab("Temperature (K)") +
                # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
                labs(color="id") +
                # scale_color_d3(palette = "category20") +
                theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
                theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                      axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
                theme(legend.position="right",
                      legend.box = "vertical",
                      legend.title = element_text(size=14,
                                                  face="bold"),
                      legend.key = element_rect(fill = "white"),
                      legend.text = element_text(size=12,
                                                 face="bold")) +
                theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
                theme(
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.75)) +
                theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
                theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
                theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
                coord_cartesian(clip = "off")  #no clipping

            p43 <- melt.palette.modifier(plot = p43)

            return(p43)
            # }
        })

        output$p.folded.melt.fit <- renderPlot({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p.folded.melt.fit()
            # }
        })

        #summary table output
        output$fit.melt.result.summary <- DT::renderDT(server=FALSE,{
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            datatable(
                fit.melt.result.summary(),
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                colnames = c("Tm (K)" = "fit.Tm.K",
                             "Tm (°C)" = "fit.Tm.C",
                             "SD Tm (fit)" = "P2SD",
                             "SD Tm (K)" = "sd.Tm.K",
                             "SD Tm (°C)" = "sd.Tm.C",
                             "Mean Tm (K)" = "mean.Tm.K",
                             "Mean Tm (°C)" = "mean.Tm.C"),
                rownames = F,
                escape = T,
                filter = 'top',
                autoHideNavigation = T,
                options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = F,
                    pageLength = 25,
                    autoWidth = F,
                    dom = 'Bfrtip', #button position
                    buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                    columnDefs = list(list(visible=FALSE, targets=c(1, 2, 3, 4)))
                )
            ) %>%
                formatRound(c("Tm (K)", "Tm (°C)", "DeltaH", "DeltaS", "DeltaG",
                              "Mean Tm (K)", "Mean Tm (°C)", "SD Tm (K)", "SD Tm (°C)",
                              "SD Tm (fit)"),
                            digits = 2)
            # }
        })

        #output boxplot of summary
        fit.melt.result.plot <- reactive({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            p47 <- ggplot(data = fit.melt.result.summary()) +
                geom_boxplot(aes(x = paste(oligo, comment, sep = "-"), y = fit.Tm.C),
                             color = "grey75") +
                geom_point(aes(x = paste(oligo, comment, sep = "-"), y = fit.Tm.C, color = ramp, shape = factor(rep)),
                           size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                theme_pander() +
                xlab("") +
                ylab("Melting temperature (°C)")

            p47 <- melt.palette.modifier(plot = p47)

            p47 <- p47 + scale_color_discrete(labels = c("cooling", "heating"))

            return(p47)
            # }
        })

        output$fit.melt.result.plot <- renderPlot({
            # if(is.null(input$input.file)) {return(NULL)}
            # else {
            fit.melt.result.plot()
            # }
        })

        #Download melt plots------

        output$dwn.melt.fit <- downloadHandler(
            filename = function() { paste("fit", '.png', sep='') },
            content = function(file) {
                ggsave(file, plot = p.raw.melt.fit(), device = "png")
            }
        )

        output$dwn.melt.model <- downloadHandler(
            filename = function() { paste("model", '.png', sep='') },
            content = function(file) {
                ggsave(file, plot = p.folded.modeled(), device = "png")
            }
        )

        output$dwn.melt.folded <- downloadHandler(
            filename = function() { paste("folded", '.png', sep='') },
            content = function(file) {
                ggsave(file, plot = p.folded.melt.fit(), device = "png")
            }
        )

        output$dwn.melt.Tm <- downloadHandler(
            filename = function() { paste("Tm", '.png', sep='') },
            content = function(file) {
                ggsave(file, plot = fit.melt.result.plot(), device = "png")
            }
        )


        #5-Display db----

        #a/input----

        #database file selection

        #UI in db tab
        output$db.file.select <- renderUI({
            fileInput(
                'db.load',
                'Select .Rda file'
            )
        })

        #UI in importR tab
        output$db.file.select.2 <- renderUI({
            fileInput(
                'db.load.2',
                'Select .Rda file'
            )
        })

        #file choice variable initialization
        db.load.choice <- reactiveValues(reactInd = 0)

        #file choice variable assignment based on last uploaded file
        observe({ #file uploaded in db tab
            input$db.load
            db.load.choice$reactInd <- 1
        })
        observe({ #file uploaded in importR tab
            input$db.load.2
            db.load.choice$reactInd <- 2
        })

        #db file selection based on choice variable assignment
        db.file <- reactive({
            #select the last provided file (from db or import tabs)
            if (db.load.choice$reactInd == 1) { #imports from db tab
                input$db.load
            } else { #inports from importR tab
                input$db.load.2
            }
        })

        #info loading
        db.info <- reactive({

            withProgress(message = 'Loading info',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             load(db.file()$datapath)

                             incProgress(amount=2/2)

                             db.info %>% #date formatting
                                 mutate(depo.date = as.Date(depo.date, format='%Y/%m/%d'))

                         })
        })

        #CD loading
        db.CD <- reactive({

            withProgress(message = 'Loading CD',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             load(db.file()$datapath)

                             incProgress(amount=2/2)

                             db.CD

                         })

        })


        #CD filtering
        db.cd.select <- reactive({

            withProgress(message = 'Filtering CD',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             db.CD() %>%
                                 filter(cation %in% input$select.cation.db) %>%
                                 filter(buffer.id %in% input$select.buffer.id.db) %>%
                                 filter(buffer %in% input$select.buffer.db)
                         })
        })

        #NMR loading
        db.NMR <- reactive({

            withProgress(message = 'Loading NMR',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             load(db.file()$datapath)

                             incProgress(amount=2/2)

                             db.NMR

                         })
        })

        #NMR filtering
        db.nmr.select <- reactive({

            withProgress(message = 'Filtering NMR',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             db.NMR() %>%
                                 filter(buffer.id %in% input$select.buffer.id.db) %>%
                                 filter(buffer %in% input$select.buffer.db) %>%
                                 filter(cation %in% input$select.cation.db)

                         })
        })

        #MS loading
        db.MS <- reactive({

            withProgress(message = 'Loading MS',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             load(db.file()$datapath)

                             incProgress(amount=2/2)

                             db.MS
                         })
        })

        #UV loading
        db.UV <- reactive({

            withProgress(message = 'Loading UV',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             load(db.file()$datapath)

                             incProgress(amount=2/2)

                             db.UV
                         })
        })

        #UV filtering
        db.UV.select <- reactive({

            withProgress(message = 'Filtering UV',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             db.UV() %>%
                                 filter(cation %in% input$select.cation.db) %>%
                                 filter(comment %in% input$select.buffer.id.db) %>%
                                 filter(buffer %in% input$select.buffer.db) %>%
                                 filter(T.K > min(input$slide.uv.fit.db) & T.K < max(input$slide.uv.fit.db))

                         })
        })

        #Oligo selection db
        output$select.oligo.db <- renderUI({
            if (is.null(db.file())) {
                return(NULL)
            } else {
                pickerInput("select.oligo.db",
                            label = "Oligonucleotides",
                            choices = unique(db.info()$oligo),
                            selected = unique(db.info()$oligo),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        selected.oligos.db <- reactive({

            if (is.null(input$input.info.db_rows_selected)) {
                return(NULL)
            } else {

                withProgress(message = 'Collecting oligonucleotides',
                             detail = 'Please wait', value = 0, {

                                 incProgress(amount=1/3)

                                 selected.oligos.db <- db.info() %>%
                                     slice(input$input.info.db_rows_selected) %>%
                                     dplyr::select(oligo)

                                 incProgress(amount=2/3)

                                 selected.oligos.db <- as.vector(selected.oligos.db[[1]])

                                 incProgress(amount=3/3)

                                 return(selected.oligos.db)
                             })
            }
        })

        buffer.list.db <- reactive({

            withProgress(message = 'Collecting electrolytes',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/3)

                             buffer.collect <- data.frame(buffers = unique(db.CD()$buffer)) %>%
                                 unique()

                             incProgress(amount=2/3)

                             buffer.list.db <- as.vector(buffer.collect$buffer)

                             incProgress(amount=3/3)

                             return(buffer.list.db)
                         })
        })

        buffer.id.list.db <- reactive({

            withProgress(message = 'Collecting buffers',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/3)

                             buffer.id.collect <- data.frame(buffers.id = unique(db.CD()$buffer.id)) %>%
                                 unique()

                             incProgress(amount=2/3)

                             buffer.id.list.db <- as.vector(buffer.id.collect$buffer)

                             incProgress(amount=3/3)

                             return(buffer.id.list.db)
                         })
        })

        cation.list.db <- reactive({

            withProgress(message = 'Collecting cations',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/3)

                             cation.collect <- data.frame(cations = unique(db.CD()$cation)) %>%
                                 unique()

                             incProgress(amount=2/3)

                             cation.list.db <- as.vector(cation.collect$cations)

                             incProgress(amount=3/3)

                             return(cation.list.db)
                         })
        })

        output$select.buffer.db <- renderUI({

            if (is.null(db.file())) {
                return(NULL)
            } else {
                pickerInput("select.buffer.db",
                            label = "Electrolyte",
                            choices = buffer.list.db(),
                            selected = buffer.list.db(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        output$select.buffer.id.db <- renderUI({

            if (is.null(db.file())) {
                return(NULL)
            } else {
                pickerInput("select.buffer.id.db",
                            label = "Buffer",
                            choices = buffer.id.list.db(),
                            selected = buffer.id.list.db(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        output$select.cation.db <- renderUI({
            if (is.null(db.file())) {
                return(NULL)
            } else {
                pickerInput("select.cation.db",
                            label = "Cation",
                            choices = cation.list.db(),
                            selected = cation.list.db(),
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        #b/tables----

        output$input.info.db <- DT::renderDT({

            if (is.null(input$select.oligo.db)) {
                return(NULL)
            } else {
                db.info() %>%
                    filter(oligo %in% input$select.oligo.db) %>%
                    setcolorder(c('oligo', 'DOI', 'submitted_by', 'depo.date',
                                  'sequence', 'nbN', 'averagemw', 'monomw',
                                  'ext.coeff.260')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = F, #need to be false to get hyperlink of DOI parsed
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c('Monoisotopic mass' = 'monomw',
                                     'Average mass' = 'averagemw',
                                     'Length' = 'nbN',
                                     'Adenine' = 'nbA',
                                     'Guanine' = 'nbG',
                                     'Cytosine' = 'nbC',
                                     'Thymine' = 'nbT',
                                     'Phosphorus' = 'nP',
                                     'Hydrogen' = 'nH',
                                     'Carbon' = 'nC',
                                     'Nitrogen' = 'nN',
                                     'Oxygen' = 'nO',
                                     'Extinction coefficient (260 nm)' = 'ext.coeff.260',
                                     'Deposition date' = 'depo.date',
                                     'Oligonucleotide' = 'oligo',
                                     'Sequence' = 'sequence',
                                     'Submitted by' = 'submitted_by'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(10:18)))
                        )
                    ) %>%
                    formatRound(c('Monoisotopic mass'),
                                digits = 5) %>%
                    formatRound(c('Average mass'),
                                digits = 2)
            }
        })

        output$input.CD.db <- DT::renderDT(server=FALSE,{
            if (is.null(selected.oligos.db())) {
                return(NULL)
            } else {
                db.cd.select() %>%
                    filter(oligo %in% selected.oligos.db()) %>%
                    setcolorder(c('oligo', 'buffer.id')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        escape = T,
                        rownames = F,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Wavelength (nm)' = 'wl',
                            'CD (mdeg)' = 'CD',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Buffer' = 'buffer.id',
                            'Oligonucleotide' = 'oligo',
                            'Path length (cm)' = 'l',
                            'Concentration (microM)' = 'con',
                            'Delta Epsilon (M-1 cm-1)' = 'delta.epsilon',
                            'Plotted data' = 'plot.y'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(4, 5, 6, 7, 9)))
                        )
                    ) %>%
                    formatRound(c('CD (mdeg)', 'Delta Epsilon (M-1 cm-1)', 'Plotted data'),
                                digits = 2)
            }
        })

        output$input.NMR.db <- DT::renderDT(server=FALSE,{
            if (is.null(selected.oligos.db())) {
                return(NULL)
            } else {
                db.nmr.select() %>%
                    filter(oligo %in% selected.oligos.db()) %>%
                    setcolorder(c('oligo', 'buffer.id', 'shift', 'int')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Intensity' ='int',
                            'Chemical shift (ppm)'='shift',
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'buffer.id',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(4, 5, 6, 7)))
                        )
                    )
            }
        })

        output$input.UV.db <- DT::renderDT(server=FALSE,{
            if (is.null(selected.oligos.db())) {
                return(NULL)
            } else {
                db.UV.select() %>%
                    filter(oligo %in% selected.oligos.db()) %>%
                    setcolorder(c('id', 'oligo', 'comment', 'ramp', 'T.K', 'abs.melt', 'folded.fraction.base', 'abs.raw')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'comment',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Ramp' = 'ramp',
                            'T (K)' = 'T.K',
                            'Folded fraction' = 'folded.fraction',
                            'Epsilon' =  'abs.melt',
                            'Model' = 'folded.fraction.base',
                            'Absorbance' = 'abs.raw'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(0,6,7,8:35, 37:39)))
                        )
                    ) %>%
                    formatRound(c('Model', 'Folded fraction', 'Absorbance'), digits = 3)
            }
        })

        output$input.ms.db <- DT::renderDT(server=FALSE,{
            if (is.null(selected.oligos.db())) {
                return(NULL)
            } else {
                db.ms.select() %>%
                    setcolorder(c('oligo', 'buffer.id', 'tune', 'rep', 'mz', 'norm.int')) %>%
                    datatable(
                        extensions = c('Buttons', 'Responsive', 'Scroller'),
                        rownames = F,
                        escape = T,
                        filter = 'top',
                        autoHideNavigation = T,
                        colnames = c(
                            'm/z' = 'mz',
                            'Intensity' = 'int',
                            'Oligonucleotide' = 'oligo',
                            'Buffer' = 'buffer.id',
                            'Electrolyte' = 'buffer',
                            'Cation' = 'cation',
                            'Normalized intensity' = 'norm.int',
                            'Tune' = 'tune',
                            'Replicate' = 'rep'
                        ),
                        options = list(
                            deferRender = TRUE,
                            scrollY = 200,
                            scroller = F,
                            pageLength = 25,
                            autoWidth = F,
                            dom = 'Bfrtip', #button position
                            buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                            columnDefs = list(list(visible=FALSE, targets=c(7:13)))
                        )
                    ) %>%
                    formatRound(c('Normalized intensity'), digits = 3)
            }
        })

        #c/figures----

        #extracts number of rows in CD plot to adjust UI output dimension
        row.p.CD.db <- reactive({
            gg_facet_nrow_ng(p.CD.db())
        })

        #extracts number of columns in cD plot to adjust UI output dimension
        col.p.CD.db <- reactive({
            gg_facet_ncol_ng(p.CD.db())
        })

        output$p.CD.db <- renderPlot({
            if (is.null(selected.oligos.db())) {return(NULL)}else{
                return(p.CD.db())
            }
        })

        p.CD.db <- reactive({

            withProgress(message = 'Plotting CD',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/7)

                             p.CD.db <- db.cd.select() %>%
                                 filter(oligo %in% selected.oligos.db()) %>%
                                 filter(wl > min(input$slide.cd.db) & wl < max(input$slide.cd.db)) %>%
                                 group_by(oligo, buffer.id, wl, l, con, CD, delta.epsilon) %>%
                                 mutate(plot.y = if_else(isTRUE(input$cd.data.select.db), delta.epsilon, CD)) %>%
                                 ggplot(aes(x = wl, y = plot.y, color = buffer.id, shape = oligo)) +
                                 geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey70', size = 0.7) +
                                 geom_point(size = input$cd.size.pt.db,
                                            alpha = input$cd.alpha.pt.db) +
                                 geom_line(size = input$cd.size.line.db,
                                           alpha = input$cd.alpha.pt.db) +
                                 theme(
                                     panel.background = element_blank(),
                                     legend.background = element_blank(),
                                     legend.box.background = element_blank(),
                                     legend.key = element_blank(),
                                     legend.text = element_text(size = 10),
                                     legend.title = element_text(size = 12, face = 'bold'),
                                     axis.text = element_text(size = 14, face = 'italic'),
                                     axis.title.x = element_text(size = 16),
                                     axis.title.y = element_text(size = 16),
                                     axis.line = element_line(size = 1),
                                     axis.ticks = element_line(size = 1, colour = 'black'),
                                     panel.grid.major.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     panel.grid.minor.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     strip.background = element_blank(),
                                     strip.text = element_text(size = 14, face = 'bold', colour = 'grey25')
                                 ) +
                                 xlab("Wavelength (nm)") +
                                 labs(colour = "buffer",
                                      shape = 'oligo') +
                                 guides(color = guide_legend(title = "Buffer"),
                                        shape = guide_legend(title = 'Oligonucleotide'))

                             incProgress(amount=2/7)

                             if (input$cd.superimpose.db == "none") {
                                 if (input$cd.free.db == 'free') {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(buffer.id~oligo,
                                                    scales = 'free_y')
                                 } else {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(buffer.id~oligo)
                                 }
                             }

                             incProgress(amount=3/7)

                             if (input$cd.superimpose.db == "oligos") {
                                 if (input$cd.free.db == 'free') {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(~oligo,
                                                    scales = 'free_y')
                                 } else {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(~oligo)
                                 }
                             }

                             incProgress(amount=4/7)

                             if (input$cd.superimpose.db == "buffer") {
                                 if (input$cd.free.db == 'free') {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(~buffer.id,
                                                    scales = 'free_y')
                                 } else {
                                     p.CD.db <- p.CD.db +
                                         facet_grid(~buffer.id)
                                 }
                             }

                             incProgress(amount=5/7)

                             if (isTRUE(input$cd.data.select.db)) {
                                 p.CD.db <- p.CD.db + ylab(expression(paste(Delta*epsilon, ' (M'^{-1},'cm'^{-1}, ')')))
                             } else {
                                 p.CD.db <- p.CD.db + ylab('mdeg')
                             }

                             incProgress(amount=6/7)

                             p.CD.db <- palette.modifier.db(plot = p.CD.db)

                             incProgress(amount=7/7)

                             return(p.CD.db)

                         })
        })

        output$p.CD.db.ui <- renderUI({
            if (is.null(selected.oligos.db())) {return(NULL)}else{
                plotOutput("p.CD.db",
                           height = 300 * row.p.CD.db())
            }
        })

        #extracts number of rows in NMR plot to adjust UI output dimension
        row.p.NMR.db <- reactive({
            gg_facet_nrow_ng(p.NMR.db())
        })

        #extracts number of columns in NMR plot to adjust UI output dimension
        col.p.NMR.db <- reactive({
            gg_facet_ncol_ng(p.NMR.db())
        })

        output$p.NMR.db <- renderPlot({
            if (is.null(selected.oligos.db())) {return(NULL)}else{
                return(p.NMR.db())
            }
        })

        p.NMR.db <- reactive({

            withProgress(message = 'Plotting NMR',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/8)

                             nmr.bounds <- db.nmr.select() %>%
                                 filter(oligo %in% selected.oligos.db()) %>%
                                 filter(shift > min(input$slide.nmr.db) & shift < max(input$slide.nmr.db)) %>% #x-scale range selection
                                 group_by(oligo) %>% #y-scale normalization (helps with labelling y-scale limits and spectra comparisons)
                                 mutate(int = (int - min(int))/(max(int) - min(int)))

                             incProgress(amount=2/8)

                             limits <- c(0.8*max(nmr.bounds$int), 1.3*max(nmr.bounds$int)) #y-scale labelling limits

                             incProgress(amount=3/8)

                             p.NMR.db <- nmr.bounds %>%
                                 mutate(peak.number = if_else(is.na(peak.number), "", peak.number)) %>% #assigns empty labels to avoid label over data points
                                 ggplot(aes(x = shift, y = int, color = oligo)) +
                                 geom_line(size = input$nmr.size.line.db) +
                                 geom_text_repel(
                                     aes(x = shift, y = int, label = peak.number,
                                         color = oligo, segment.color = oligo),
                                     force = 4,
                                     direction = 'y',
                                     min.segment.length = 0.15,
                                     segment.size = 0.5,
                                     box.padding = 1,
                                     alpha = 1,
                                     size = 5,
                                     fontface = 'bold',
                                     show.legend = F,
                                     ylim = limits
                                 ) +
                                 scale_x_reverse() + #inverted x scale for chemical shift
                                 theme(
                                     panel.background = element_blank(),
                                     legend.position = 'none',
                                     axis.text.x = element_text(size = 14, face = 'italic'),
                                     axis.text.y = element_blank(),
                                     axis.title.x = element_text(size = 16),
                                     axis.title.y = element_blank(),
                                     axis.line.x = element_line(size = 1),
                                     axis.ticks.x = element_line(size = 1, colour = 'black'),
                                     axis.ticks.y = element_blank(),
                                     strip.background = element_blank(),
                                     strip.text = element_text(size = 14, face = 'bold', colour = 'grey25')
                                 ) +
                                 xlab(bquote(""^1*H~"Chemical shift (ppm)")) +
                                 #allows for some extra space on the y-scale for labelling
                                 coord_cartesian(ylim = c(min(nmr.bounds$int), max(nmr.bounds$int)*1.2))

                             incProgress(amount=4/8)

                             if (length(selected.oligos.db()>1)) {
                                 if (input$nmr.superimpose.db == "none") {
                                     if (input$nmr.free.db == 'free') {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(buffer.id~oligo,
                                                        scales = 'free_y')
                                     } else {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(oligo~buffer.id)
                                     }
                                 }

                                 incProgress(amount=5/8)

                                 if (input$nmr.superimpose.db == "oligos") {
                                     if (input$nmr.free.db == 'free') {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(~oligo,
                                                        scales = 'free_y')
                                     } else {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(~oligo)
                                     }
                                 }

                                 incProgress(amount=6/8)

                                 if (input$nmr.superimpose.db == "buffer") {
                                     if (input$nmr.free.db == 'free') {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(~buffer.id,
                                                        scales = 'free_y')
                                     } else {
                                         p.NMR.db <- p.NMR.db +
                                             facet_grid(~buffer.id)
                                     }
                                 }
                             }

                             if (length(selected.oligos.db() == 1)) {

                                 p.NMR.db <- p.NMR.db +
                                     theme(
                                         strip.text = element_blank()
                                     )
                             }

                             incProgress(amount=7/8)

                             p.NMR.db <- palette.modifier.db(plot = p.NMR.db)

                             incProgress(amount=8/8)

                             return(p.NMR.db)
                         })
        })

        output$p.NMR.ui.db <- renderUI({
            if (is.null(selected.oligos.db())) {return(NULL)}else{
                plotOutput("p.NMR.db",
                           height = 300 * row.p.NMR.db())
            }
        })

        db.ms.select <- eventReactive(input$plotMS.db,{

            withProgress(message = 'Filtering MS',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/2)

                             db.MS() %>%
                                 filter(buffer.id %in% input$select.buffer.id.db) %>%
                                 filter(buffer %in% input$select.buffer.db) %>%
                                 filter(cation %in% input$select.cation.db) %>%
                                 filter(oligo %in% selected.oligos.db()) %>%
                                 filter(mz > min(input$slide.ms.db) & mz < max(input$slide.ms.db))

                         })
        })

        output$select.tune.db <- renderUI({
            if(is.null(selected.oligos.db())) {
                pickerInput("select.tune.db",
                            label = "Tunes",
                            choices = "upload data first",
                            multiple = F
                )
            } else {
                pickerInput("select.tune.db",
                            label = "Tunes",
                            choices = unique(db.ms.select()$tune),
                            selected = unique(db.ms.select()$tune)[[1]], #first tune name selected by default
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        output$select.rep.db <- renderUI({
            if(is.null(selected.oligos.db())) {
                pickerInput("select.rep.db",
                            label = "Replicates",
                            choices = "upload data first",
                            multiple = F
                )
            } else {
                pickerInput("select.rep.db",
                            label = "Replicates",
                            choices = unique(db.ms.select()$rep),
                            selected = unique(db.ms.select()$rep)[[1]], #first rep name selected by default
                            multiple = T,
                            options = pickerOptions(
                                actionsBox = T,
                                liveSearch = T
                            ),
                            choicesOpt = list(
                                style = rep(("color: black; background: white; font-weight: normal;"),10)
                            )
                )
            }
        })

        #extracts number of rows in MS plot to adjust UI output dimension
        row.p.MS.db <- reactive({
            gg_facet_nrow_ng(p.MS.db())
        })

        #extracts number of columns in MS plot to adjust UI output dimension
        col.p.MS.db <- reactive({
            gg_facet_ncol_ng(p.MS.db())
        })


        p.MS.db.0 <- reactive({

            req(db.ms.select())

            p.MS.db.0 <- db.ms.select() %>%
                filter(rep %in% input$select.rep.db) %>%
                filter(tune %in% input$select.tune.db) %>%
                mutate(
                    labels = if_else( #generation of formatted labels
                        is.na(species), species,
                        case_when( #add brackets and charge in superscript
                            species == 'M' ~ paste('"["*M*"]"^{', charge, '-NA}'),
                            species == 'MK' ~ paste('"["*MK*"]"^{', charge, '-NA}'),
                            #if more than 1 K, extract that number of K, use in subscript
                            length(species) > 2 ~ paste('"["*MK', '[',substring(species, 3),']*"]"^{', charge, '-NA}')
                        )
                    )
                )
        })

        p.MS.db.1 <- reactive({

            label.range <- p.MS.db.0() %>%
                filter(charge == input$charge.select) %>%
                group_by(oligo) %>%
                select(c('oligo', 'buffer.id', 'tune', 'rep', 'mz', 'labels')) %>%
                mutate(range.min = min(mz)*0.99,
                       range.max = max(mz)*1.025) %>%
                distinct(range.min, range.max, oligo, buffer.id, tune, rep)

            p.MS.db.1 <- p.MS.db.0() %>%
                left_join(label.range) %>%
                group_by(oligo, buffer.id, tune, rep)

            return(p.MS.db.1)
        })

        p.MS.db <- reactive({

            withProgress(message = 'Plotting MS',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/9)

                             req(p.MS.db.0())

                             incProgress(amount=2/9)

                             p.MS.db <- p.MS.db.0() %>%
                                 ggplot(aes(x = mz, y = norm.int)) +
                                 theme(
                                     panel.background = element_blank(),
                                     legend.background = element_blank(),
                                     legend.box.background = element_blank(),
                                     legend.key = element_blank(),
                                     legend.text = element_text(size = 10),
                                     legend.title = element_text(size = 12, face = 'bold'),
                                     axis.text = element_text(size = 14, face = 'italic'),
                                     axis.title.x = element_text(size = 16),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.line.x = element_line(size = 1),
                                     axis.line.y = element_blank(),
                                     axis.ticks.x = element_line(size = 1, colour = 'black'),
                                     axis.ticks.y = element_blank(),
                                     panel.grid.major.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     panel.grid.minor.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     strip.background = element_blank(),
                                     strip.text = element_text(size = 14, face = 'bold', colour = 'grey25')
                                 ) +
                                 xlab("m/z") +
                                 labs(colour = "legend")

                             incProgress(amount=3/9)

                             if (input$ms.superimpose.db == "oligo x buffer") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~buffer.id,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~oligo,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("replicate:", rep,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "replicate x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = labels, x = mz, y = 1,
                                                        color = paste("replicate:", rep,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }

                             }

                             incProgress(amount=4/9)

                             if (input$ms.superimpose.db == "oligo x tune") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~tune,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(tune~oligo,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("replicate:", rep,
                                                                 ", buffer:", buffer.id))
                                     ) +
                                     guides(color = guide_legend(title = "replicate x buffer"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = labels, x = mz, y = 1,
                                                        color = paste("replicate:", rep,
                                                                      ", buffer:", buffer.id)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=5/9)

                             if (input$ms.superimpose.db == "oligo x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~rep,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~oligo,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("buffer:", buffer.id,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "buffer x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("buffer:", buffer.id,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=6/9)

                             if (input$ms.superimpose.db == "buffer x tune") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~tune,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(tune~buffer.id,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", replicate:", rep))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x replicate"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", replicate:", rep)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=7/9)

                             if (input$ms.superimpose.db == "buffer x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~rep,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~buffer.id,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=8/9)

                             if (input$ms.superimpose.db == "tune x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(tune~rep,
                                                                     scales = "free_y")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~tune,
                                                                     scales = "free_y")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", buffer:", buffer.id))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x buffer"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", buffer:", buffer.id)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=9/9)

                             p.MS.db <- palette.modifier.db(plot = p.MS.db)

                             return(p.MS.db)
                         })
        })

        p.MS.db.2 <- reactive({

            withProgress(message = 'Plotting zoomed MS',
                         detail = 'Please wait', value = 0, {

                             incProgress(amount=1/9)

                             req(p.MS.db.0())

                             incProgress(amount=2/9)

                             p.MS.db <- p.MS.db.1() %>%
                                 group_by(oligo, buffer.id, rep, tune) %>%
                                 filter(mz > range.min & mz < range.max) %>%
                                 mutate(int.min = min(int), int.max = max(int)) %>%
                                 group_by(mz, oligo, buffer.id, tune, rep) %>%
                                 mutate(norm.int = (int - int.min)/(int.max - int.min)) %>%
                                 ggplot(aes(x = mz, y = norm.int)) +
                                 theme(
                                     panel.background = element_blank(),
                                     legend.background = element_blank(),
                                     legend.box.background = element_blank(),
                                     legend.key = element_blank(),
                                     legend.text = element_text(size = 10),
                                     legend.title = element_text(size = 12, face = 'bold'),
                                     axis.text = element_text(size = 14, face = 'italic'),
                                     axis.title.x = element_text(size = 16),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.line.x = element_line(size = 1),
                                     axis.line.y = element_blank(),
                                     axis.ticks.x = element_line(size = 1, colour = 'black'),
                                     axis.ticks.y = element_blank(),
                                     panel.grid.major.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     panel.grid.minor.x = element_line(size = 0.7, colour = 'grey70', linetype = 'dashed'),
                                     strip.background = element_blank(),
                                     strip.text = element_text(size = 14, face = 'bold', colour = 'grey25')
                                 ) +
                                 xlab("m/z") +
                                 labs(colour = "legend")

                             incProgress(amount=3/9)

                             if (input$ms.superimpose.db == "oligo x buffer") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~buffer.id,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~oligo,
                                                                     scales = "free")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("replicate:", rep,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "replicate x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = labels, x = mz, y = 1,
                                                        color = paste("replicate:", rep,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }

                             }

                             incProgress(amount=4/9)

                             if (input$ms.superimpose.db == "oligo x tune") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~tune,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(tune~oligo,
                                                                     scales = "free")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("replicate:", rep,
                                                                 ", buffer:", buffer.id))
                                     ) +
                                     guides(color = guide_legend(title = "replicate x buffer"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = labels, x = mz, y = 1,
                                                        color = paste("replicate:", rep,
                                                                      ", buffer:", buffer.id)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=5/9)

                             if (input$ms.superimpose.db == "oligo x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(oligo~rep,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~oligo,
                                                                     scales = "free")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("buffer:", buffer.id,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "buffer x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("buffer:", buffer.id,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=6/9)

                             if (input$ms.superimpose.db == "buffer x tune") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~tune,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(tune~buffer.id,
                                                                     scales = "fre")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", replicate:", rep))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x replicate"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", replicate:", rep)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=7/9)

                             if (input$ms.superimpose.db == "buffer x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(buffer.id~rep,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~buffer.id,
                                                                     scales = "free")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", tune:", tune))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x tune"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", tune:", tune)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=8/9)

                             if (input$ms.superimpose.db == "tune x replicate") {
                                 if(isFALSE(input$switch.grid.ms.db)){
                                     p.MS.db <- p.MS.db + facet_grid(tune~rep,
                                                                     scales = "free")
                                 } else {
                                     p.MS.db <- p.MS.db + facet_grid(rep~tune,
                                                                     scales = "free")
                                 }

                                 p.MS.db <- p.MS.db +
                                     geom_line(size = input$ms.size.line.db,
                                               aes(color = paste("oligo:", oligo,
                                                                 ", buffer:", buffer.id))
                                     ) +
                                     guides(color = guide_legend(title = "oligonucleotide x buffer"))

                                 if (isTRUE(input$switch.label.ms.db)) {
                                     p.MS.db <- p.MS.db  +
                                         geom_label(aes(label = deparse(labels), x = mz, y = 1,
                                                        color = paste("oligo:", oligo,
                                                                      ", buffer:", buffer.id)),
                                                    show.legend = F,
                                                    inherit.aes = F,
                                                    parse = T)
                                 }
                             }

                             incProgress(amount=9/9)

                             p.MS.db <- palette.modifier.db(plot = p.MS.db)

                             return(p.MS.db)
                         })
        })

        output$p.MS.db <- renderPlot({
            if(is.null(selected.oligos.db())) {return(NULL)}else{
                return(p.MS.db())
            }
        })

        output$p.MS.db.2 <- renderPlot({
            if(is.null(selected.oligos.db())) {return(NULL)}else{
                return(p.MS.db.2())
            }
        })

        output$p.MS.ui.db <- renderUI({
            plotOutput("p.MS.db",
                       height = 300*row.p.MS.db())
        })

        output$p.MS.ui.db.2 <- renderUI({
            plotOutput("p.MS.db.2",
                       height = 300*row.p.MS.db())
        })

        p.db.UV.select <- reactive({
            db.UV.select() %>%
                filter(oligo %in% selected.oligos.db())
        })



        p.UV.fit.db <- reactive({
            if(is.null(selected.oligos.db())) {return(NULL)}else{

                withProgress(message = 'Plotting raw UV-melting',
                             detail = 'Please wait', value = 0, {

                                 incProgress(amount=1/4)

                                 #simplifies the id for simple cases (only one replicate of a single oligo)
                                 if (length(selected.oligos.db()) == 1) {
                                     p.db.UV.select <- p.db.UV.select() %>%
                                         mutate(id = paste0(comment, '-', ramp))
                                 } else {
                                     p.db.UV.select <- p.db.UV.select()
                                 }

                                 incProgress(amount=2/4)

                                 p.UV.melting.db <- p.db.UV.select %>%
                                     ggplot() +
                                     geom_point(aes(x = T.K-273.15, y = abs.melt, color = id),
                                                size = input$uv.fit.size.pt.db, alpha = input$uv.fit.alpha.pt.db,
                                                shape = 16) +  #plots the experimental data
                                     geom_line(aes(x = T.K-273.15, y = raw.fit.y, color = id),
                                               size = input$uv.fit.size.line.db, alpha = input$uv.fit.alpha.line.db) +
                                     ylab(bquote(epsilon*' ('*M^-1~cm^-1*')')) + #modifies axes titles
                                     xlab("Temperature (°C)") +
                                     labs(color="id") +
                                     theme(
                                         panel.background = element_blank(),
                                         legend.background = element_blank(),
                                         legend.box.background = element_blank(),
                                         legend.key = element_blank(),
                                         legend.text = element_text(size = 10),
                                         legend.title = element_text(size = 12, face = 'bold'),
                                         axis.text = element_text(size = 14, face = 'italic'),
                                         axis.title.x = element_text(size = 16),
                                         axis.title.y = element_text(size = 16),
                                         axis.line = element_line(size = 1),
                                         axis.ticks = element_line(size = 1, colour = 'black'),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.minor.x = element_blank()
                                     ) +
                                     guides(color = guide_legend(title = "Melting id"))

                                 incProgress(amount=3/4)

                                 p.UV.fit.db <- palette.modifier.db(plot = p.UV.melting.db)

                                 incProgress(amount=4/4)

                                 return(p.UV.fit.db)

                             })
            }
        })

        output$p.UV.fit.db <- renderPlot({
            p.UV.fit.db()
        })

        p.UV.melting.db <- reactive({
            if(is.null(selected.oligos.db())) {return(NULL)}else{

                withProgress(message = 'Plotting processed UV-melting',
                             detail = 'Please wait', value = 0, {

                                 incProgress(amount=1/5)

                                 #simplifies the id for simple cases (only one replicate of a single oligo)
                                 if (length(selected.oligos.db()) == 1) {
                                     p.db.UV.select <- p.db.UV.select() %>%
                                         mutate(id = paste0(comment, '-', ramp))
                                 } else {
                                     p.db.UV.select <- p.db.UV.select()
                                 }

                                 incProgress(amount=2/5)

                                 #creation of Tm labels
                                 labels <- p.db.UV.select %>%
                                     group_by(id) %>%
                                     dplyr::distinct(P2, .keep_all = T) %>%
                                     group_by(oligo, comment) %>%
                                     # mutate(label = round(mean(P2), 1)) ##to have a single label for both ramps (remove the repel as well below)
                                     mutate(label = round((P2-273.15), 1))

                                 incProgress(amount=3/5)

                                 p.UV.melting.db <- p.db.UV.select %>%
                                     ggplot() +
                                     geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'grey70', size = 0.7) +
                                     geom_point(aes(x = (T.K-273.15), y = folded.fraction.base, color = id),
                                                size = input$uv.size.pt.db, alpha = input$uv.alpha.pt.db,
                                                shape = 16) +  #plots the experimental data
                                     geom_label_repel(
                                         size = 5,
                                         inherit.aes = F,
                                         show.legend = F,
                                         data = labels,
                                         aes(label = label, x = label, y = 0.5, color = id)
                                     ) +
                                     ylab(bquote(bold("folded fraction"))) + #modifies axes titles
                                     xlab("Temperature (°C)") +
                                     labs(color="id") +
                                     theme(
                                         panel.background = element_blank(),
                                         legend.background = element_blank(),
                                         legend.box.background = element_blank(),
                                         legend.key = element_blank(),
                                         legend.text = element_text(size = 10),
                                         legend.title = element_text(size = 12, face = 'bold'),
                                         axis.text = element_text(size = 14, face = 'italic'),
                                         axis.title.x = element_text(size = 16),
                                         axis.title.y = element_text(size = 16),
                                         axis.line = element_line(size = 1),
                                         axis.ticks = element_line(size = 1, colour = 'black'),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.minor.x = element_blank()
                                     ) +
                                     guides(color = guide_legend(title = "Melting id"))

                                 incProgress(amount=4/5)

                                 p.UV.melting.db <- palette.modifier.db(plot = p.UV.melting.db)

                                 incProgress(amount=5/5)

                                 return(p.UV.melting.db)

                             })
            }
        })

        output$p.UV.melting.db <- renderPlot({
            p.UV.melting.db()
        })

        output$p.UV.melting.ui.db <- renderUI({
            plotOutput("p.UV.melting.db")
        })

        output$p.UV.fit.ui.db <- renderUI({
            plotOutput("p.UV.fit.db")
        })

        #6-Write to db----

        output$downloadData <- downloadHandler(
            filename = function() {
                paste("Database-", Sys.Date(), ".Rda", sep="")
            },
            content = function(file) {

                withProgress(message = 'Database edition',
                             detail = 'Please wait', value = 0, {

                                 incProgress(amount=1/8)

                                 # info

                                 info.db <- as.data.frame(info.epsilon())
                                 # rownames(info.db) <- info.db$oligo

                                 db.info <- info.db %>%
                                     filter(oligo %in% selected.oligos()) %>%
                                     as.data.frame()

                                 initial.info <- as.data.frame(db.info())

                                 db.info <- rbind.data.frame(db.info, initial.info)
                                 # make.row.names = T)

                                 db.info <- db.info[!duplicated(db.info$oligo), ]

                                 # db.info <- as.data.frame(db.info)

                                 incProgress(amount=2/8)

                                 # CD
                                 if (isTRUE(input$exp.CD)) { #only write to database if switch is on

                                     db.CD.0 <- db.cd.select() %>%
                                         filter(oligo %in% selected.oligos())

                                     db.CD <- rbind(calc.cd(), db.CD.0)

                                     # rbind.data.frame(db.CD(), calc.cd())

                                     db.CD <- db.CD[!duplicated(paste(db.CD$oligo, db.CD$wl, db.CD$buffer.id)), ]

                                 } else {
                                     db.CD <- db.CD()
                                 }

                                 incProgress(amount=3/8)

                                 #UV-melting
                                 if (isTRUE(input$exp.melt)) { #only write to database if switch is on
                                     db.UV <- calc.UV() %>%
                                         filter(oligo %in% input$select.oligo) %>%
                                         rbind(db.UV())

                                     db.UV <- db.UV[!duplicated(paste(db.UV$id, db.UV$T.K)), ]

                                 } else {
                                     db.UV <- db.UV()
                                 }

                                 incProgress(amount=4/8)

                                 #NMR
                                 if (isTRUE(input$exp.NMR)) { #only write to database if switch is on

                                     NMR.to.db <- input.NMR() %>%
                                         filter(oligo %in% selected.oligos()) %>%
                                         filter(shift > min(input$slide.nmr) & shift < max(input$slide.nmr))

                                     db.NMR <- rbind.data.frame(db.NMR(), NMR.to.db)

                                     #removes rows with duplicated data -> no duplicates in db
                                     db.NMR <- db.NMR[!duplicated(paste(db.NMR$oligo, db.NMR$shift, db.NMR$buffer.id)), ]

                                 } else {
                                     db.NMR <- db.NMR()
                                 }

                                 incProgress(amount=5/8)

                                 #MS
                                 if (isTRUE(input$exp.MS)) { #only write to database if switch is on

                                     filtered.MS <- input.MS() %>%
                                         filter(tune %in% input$select.tune) %>%
                                         filter(rep %in% input$select.rep) %>%
                                         filter(oligo %in% selected.oligos())

                                     incProgress(amount=6/8)

                                     db.MS <- rbind.data.frame(db.MS(), filtered.MS)
                                     #removes rows with duplicated data -> no duplicates in db
                                     db.MS <- db.MS[!duplicated(paste(db.MS$oligo, db.MS$mz, db.MS$buffer.id,
                                                                      db.MS$tune, db.MS$rep)), ]

                                 } else {
                                     db.MS <- db.MS()
                                 }

                                 incProgress(amount=7/8)
                                 #File save
                                 save(db.info,
                                      db.CD,
                                      db.NMR,
                                      db.MS,
                                      db.UV,
                                      file = file)

                                 incProgress(amount=8/8)
                             })

            }
        )

        #6bis-Erasing db----

        output$erase.db <- downloadHandler(
            filename = function() {
                paste("Modified database-", Sys.Date(), ".Rda", sep="")
            },
            content = function(file) {

                withProgress(message = 'Writing a modified database',
                             detail = 'Please wait', value = 0, {

                                 incProgress(amount=1/7)

                                 db.collection <- database.eraser(db.to.erase = db.file()$datapath,
                                                                  remove.oligos =  input$select.oligo.db,
                                                                  erase.CD = input$erase.CD,
                                                                  erase.NMR = input$erase.NMR,
                                                                  erase.MS = input$erase.MS,
                                                                  erase.UV = input$erase.UV)
                                 incProgress(amount=2/7)

                                 db.CD <- db.collection$db.CD
                                 incProgress(amount=3/7)
                                 db.info <- db.collection$db.info
                                 incProgress(amount=4/7)
                                 db.NMR <- db.collection$db.NMR
                                 incProgress(amount=5/7)
                                 db.MS <- db.collection$db.MS
                                 incProgress(amount=6/7)
                                 db.UV <- db.collection$db.UV
                                 incProgress(amount=7/7)

                                 save(db.info,
                                      db.CD,
                                      db.NMR,
                                      db.MS,
                                      db.UV,
                                      file = file)
                             })
            })

        oligos.to.remove <- reactive({

            if(length(input$select.oligo.db) == length(unique(db.info()$oligo))) {
                oligos.to.remove <- 'All of them!'
            } else {
                oligos.to.remove <- paste(input$select.oligo.db, collapse=", ")
            }

        })

        output$oligos.to.remove <- renderText({

            validate(
                need(db.file(), 'load a database first')
            )

            oligos.to.remove()
        })

        #7-Report download----

        output$downloadReport <- downloadHandler(
            filename = function() {
                paste(paste('g4db report -', paste(selected.oligos.db(), collapse=", ")), sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            content = function(file) {

                withProgress(message = 'Report generation',
                             detail = 'Please wait', value = 0, {

                                 if (isTRUE(input$report.choice)) {
                                     report <- "report.Rmd"
                                 } else {
                                     report <- "report_SI.Rmd"
                                 }

                                 incProgress(amount=1/7)

                                 src <- system.file(paste0("rmarkdown/", report), package = 'g4dbr')

                                 # src <- system.file("rmarkdown/report_SI.Rmd", package = 'g4dbr')

                                 incProgress(amount=2/7)

                                 # temporarily switch to temp dir, in case there is no write permission to current wording dir
                                 owd <- setwd(tempdir())
                                 incProgress(amount=3/7)
                                 on.exit(setwd(owd))
                                 incProgress(amount=4/7)
                                 file.copy(src, report, overwrite = TRUE)
                                 incProgress(amount=5/7)
                                 # "word_document" used instead of word_document() so that template is taken into account
                                 # To modify template, locate it with:
                                 # run system.file("rmarkdown/word-styles-reference.docx", package = "g4dbr")
                                 out <- rmarkdown::render(report, switch(
                                     input$format,
                                     Word = "word_document", HTML = html_document(),  PDF = pdf_document()
                                 ))
                                 incProgress(amount=6/7)
                                 file.rename(out, file)
                                 incProgress(amount=7/7)
                             }
                )
            }
        )

        #################X__X#################

        #8-Palettes-------
        #palettes for import data
        palette.modifier <- function(plot = NULL){
            if (input$select.import.palette.fam == 'd3') {
                plot <- plot + scale_color_d3(palette = input$select.import.palette)
            } else {
                if (input$select.import.palette.fam == "Brewer - qualitative") {
                    plot <- plot + scale_color_brewer(palette = input$select.import.palette)
                } else{
                    if (input$select.import.palette.fam == "Brewer - sequential") {
                        plot <- plot + scale_color_brewer(palette = input$select.import.palette)
                    } else {
                        if (input$select.import.palette.fam == "Brewer - diverging") {
                            plot <- plot + scale_color_brewer(palette = input$select.import.palette)
                        } else {
                            if (input$select.import.palette.fam == "NPG") {
                                plot <- plot + scale_color_npg()
                            } else {
                                if (input$select.import.palette.fam == "AAAS") {
                                    plot <- plot + scale_color_aaas()
                                } else {
                                    if (input$select.import.palette.fam == "NEJM") {
                                        plot <- plot + scale_color_nejm()
                                    } else {
                                        if (input$select.import.palette.fam == "Lancet") {
                                            plot <- plot + scale_color_lancet()
                                        } else {
                                            if (input$select.import.palette.fam == "JAMA") {
                                                plot <- plot + scale_color_jama()
                                            } else {
                                                if (input$select.import.palette.fam == "JCO") {
                                                    plot <- plot + scale_color_jco()
                                                } else {
                                                    if (input$select.import.palette.fam == "UCSCGB") {
                                                        plot <- plot + scale_color_ucscgb()
                                                    } else {
                                                        if (input$select.import.palette.fam == "LocusZoom") {
                                                            plot <- plot + scale_color_locuszoom()
                                                        } else {
                                                            if (input$select.import.palette.fam == "IGV") {
                                                                plot <- plot + scale_color_igv(palette = input$select.import.palette)
                                                            } else {
                                                                if (input$select.import.palette.fam == "UChicago") {
                                                                    plot <- plot + scale_color_uchicago(palette = input$select.import.palette)
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        #palettes for database
        palette.modifier.db <- function(plot = NULL){
            if (input$select.import.palette.fam.db == 'd3') {
                plot <- plot + scale_color_d3(palette = input$select.import.palette.db)
            } else {
                if (input$select.import.palette.fam.db == "Brewer - qualitative") {
                    plot <- plot + scale_color_brewer(palette = input$select.import.palette.db)
                } else{
                    if (input$select.import.palette.fam.db == "Brewer - sequential") {
                        plot <- plot + scale_color_brewer(palette = input$select.import.palette.db)
                    } else {
                        if (input$select.import.palette.fam.db == "Brewer - diverging") {
                            plot <- plot + scale_color_brewer(palette = input$select.import.palette.db)
                        } else {
                            if (input$select.import.palette.fam.db == "NPG") {
                                plot <- plot + scale_color_npg()
                            } else {
                                if (input$select.import.palette.fam.db == "AAAS") {
                                    plot <- plot + scale_color_aaas()
                                } else {
                                    if (input$select.import.palette.fam.db == "NEJM") {
                                        plot <- plot + scale_color_nejm()
                                    } else {
                                        if (input$select.import.palette.fam.db == "Lancet") {
                                            plot <- plot + scale_color_lancet()
                                        } else {
                                            if (input$select.import.palette.fam.db == "JAMA") {
                                                plot <- plot + scale_color_jama()
                                            } else {
                                                if (input$select.import.palette.fam.db == "JCO") {
                                                    plot <- plot + scale_color_jco()
                                                } else {
                                                    if (input$select.import.palette.fam.db == "UCSCGB") {
                                                        plot <- plot + scale_color_ucscgb()
                                                    } else {
                                                        if (input$select.import.palette.fam.db == "LocusZoom") {
                                                            plot <- plot + scale_color_locuszoom()
                                                        } else {
                                                            if (input$select.import.palette.fam.db == "IGV") {
                                                                plot <- plot + scale_color_igv(palette = input$select.import.palette.db)
                                                            } else {
                                                                if (input$select.import.palette.fam.db == "UChicago") {
                                                                    plot <- plot + scale_color_uchicago(palette = input$select.import.palette.db)
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        #Palette family selector
        #for import data
        output$select.import.palette.fam <- renderUI({
            pickerInput("select.import.palette.fam",
                        label = "Choose palette family",
                        choices = list("d3",
                                       "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                       "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                       "UChicago", "UCSCGB"),
                        multiple = F
            )
        })

        #for database
        output$select.import.palette.fam.db <- renderUI({
            pickerInput("select.import.palette.fam.db",
                        label = "Choose palette family",
                        choices = list("d3",
                                       "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                       "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                       "UChicago", "UCSCGB"),
                        multiple = F
            )
        })

        #Palette subcategory selector
        #for import data
        output$select.import.palette <- renderUI({
            if (input$select.import.palette.fam == 'd3') {
                pickerInput("select.import.palette",
                            label = "Choose palette",
                            choices = list("d3a" = "category20",
                                           "d3b" = "category20b",
                                           "d3c" = "category20c"),
                            multiple = F
                )
            } else {
                if (input$select.import.palette.fam == 'Brewer - qualitative') {
                    pickerInput("select.import.palette",
                                label = "Choose palette",
                                choices = list("Accent", "Dark2", "Paired",
                                               "Pastel1", "Pastel2", "Set1",
                                               "Set2", "Set3"),
                                multiple = F
                    )
                } else {
                    if (input$select.import.palette.fam == 'Brewer - diverging') {
                        pickerInput("select.import.palette",
                                    label = "Choose palette",
                                    choices = list("BrBG", 'PiYG', 'PRGn',
                                                   'PuOr', 'RdBu', 'RdGy',
                                                   'RdYlBu', 'RdYlGn', 'Spectral'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.import.palette.fam == 'Brewer - sequential') {
                            pickerInput("select.import.palette",
                                        label = "Choose palette",
                                        choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                       'Greens', 'Greys', 'Oranges', 'OrRd',
                                                       'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                       'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                        multiple = F
                            )
                        } else {
                            if (input$select.import.palette.fam == 'IGV') {
                                pickerInput("select.import.palette",
                                            label = "Choose palette",
                                            choices = list("default", "alternating"),
                                            multiple = F
                                )
                            } else {
                                if (input$select.import.palette.fam == 'UChicago') {
                                    pickerInput("select.import.palette",
                                                label = "Choose palette",
                                                choices = list("default", "light", "dark"),
                                                multiple = F
                                    )
                                } else {
                                    return(NULL)
                                }
                            }
                        }
                    }
                }
            }
        })

        #for database
        output$select.import.palette.db <- renderUI({
            if (input$select.import.palette.fam.db == 'd3') {
                pickerInput("select.import.palette.db",
                            label = "Choose palette",
                            choices = list("d3a" = "category20",
                                           "d3b" = "category20b",
                                           "d3c" = "category20c"),
                            multiple = F
                )
            } else {
                if (input$select.import.palette.fam.db == 'Brewer - qualitative') {
                    pickerInput("select.import.palette.db",
                                label = "Choose palette",
                                choices = list("Accent", "Dark2", "Paired",
                                               "Pastel1", "Pastel2", "Set1",
                                               "Set2", "Set3"),
                                multiple = F
                    )
                } else {
                    if (input$select.import.palette.fam.db == 'Brewer - diverging') {
                        pickerInput("select.import.palette.db",
                                    label = "Choose palette",
                                    choices = list("BrBG", 'PiYG', 'PRGn',
                                                   'PuOr', 'RdBu', 'RdGy',
                                                   'RdYlBu', 'RdYlGn', 'Spectral'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.import.palette.fam.db == 'Brewer - sequential') {
                            pickerInput("select.import.palette.db",
                                        label = "Choose palette",
                                        choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                       'Greens', 'Greys', 'Oranges', 'OrRd',
                                                       'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                       'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                        multiple = F
                            )
                        } else {
                            if (input$select.import.palette.fam.db == 'IGV') {
                                pickerInput("select.import.palette.db",
                                            label = "Choose palette",
                                            choices = list("default", "alternating"),
                                            multiple = F
                                )
                            } else {
                                if (input$select.import.palette.fam.db == 'UChicago') {
                                    pickerInput("select.import.palette.db",
                                                label = "Choose palette",
                                                choices = list("default", "light", "dark"),
                                                multiple = F
                                    )
                                } else {
                                    return(NULL)
                                }
                            }
                        }
                    }
                }
            }
        })

        # #MELTING PALETTE
        melt.palette.modifier <- function(plot = NULL){
            if (input$select.melting.palette.fam == 'd3') {
                plot <- plot + scale_color_d3(palette = input$select.melting.palette,
                                              labels = tm.init.change()$legend)
            } else {
                if (input$select.melting.palette.fam == "Brewer - qualitative") {
                    plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                      labels = tm.init.change()$legend)
                } else{
                    if (input$select.melting.palette.fam == "Brewer - sequential") {
                        plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                          labels = tm.init.change()$legend)
                    } else {
                        if (input$select.melting.palette.fam == "Brewer - diverging") {
                            plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                              labels = tm.init.change()$legend)
                        } else {
                            if (input$select.melting.palette.fam == "NPG") {
                                plot <- plot + scale_color_npg(labels = tm.init.change()$legend)
                            } else {
                                if (input$select.melting.palette.fam == "AAAS") {
                                    plot <- plot + scale_color_aaas(labels = tm.init.change()$legend)
                                } else {
                                    if (input$select.melting.palette.fam == "NEJM") {
                                        plot <- plot + scale_color_nejm(labels = tm.init.change()$legend)
                                    } else {
                                        if (input$select.melting.palette.fam == "Lancet") {
                                            plot <- plot + scale_color_lancet(labels = tm.init.change()$legend)
                                        } else {
                                            if (input$select.melting.palette.fam == "JAMA") {
                                                plot <- plot + scale_color_jama(labels = tm.init.change()$legend)
                                            } else {
                                                if (input$select.melting.palette.fam == "JCO") {
                                                    plot <- plot + scale_color_jco(labels = tm.init.change()$legend)
                                                } else {
                                                    if (input$select.melting.palette.fam == "UCSCGB") {
                                                        plot <- plot + scale_color_ucscgb(labels = tm.init.change()$legend)
                                                    } else {
                                                        if (input$select.melting.palette.fam == "LocusZoom") {
                                                            plot <- plot + scale_color_locuszoom(labels = tm.init.change()$legend)
                                                        } else {
                                                            if (input$select.melting.palette.fam == "IGV") {
                                                                plot <- plot + scale_color_igv(palette = input$select.melting.palette,
                                                                                               labels = tm.init.change()$legend)
                                                            } else {
                                                                if (input$select.melting.palette.fam == "UChicago") {
                                                                    plot <- plot + scale_color_uchicago(palette = input$select.melting.palette,
                                                                                                        labels = tm.init.change()$legend)
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        #Palette family selector
        output$select.melting.palette.fam <- renderUI({
            pickerInput("select.melting.palette.fam",
                        label = "Choose palette family",
                        choices = list("d3",
                                       "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                       "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                       "UChicago", "UCSCGB"),
                        multiple = F
            )
        })

        #Palette subcategory selector
        output$select.melting.palette <- renderUI({

            if (input$select.melting.palette.fam == 'd3') {
                pickerInput("select.melting.palette",
                            label = "Choose palette",
                            choices = list("d3a" = "category20",
                                           "d3b" = "category20b",
                                           "d3c" = "category20c"),
                            multiple = F
                )
            } else {
                if (input$select.melting.palette.fam == 'Brewer - qualitative') {
                    pickerInput("select.melting.palette",
                                label = "Choose palette",
                                choices = list("Accent", "Dark2", "Paired",
                                               "Pastel1", "Pastel2", "Set1",
                                               "Set2", "Set3"),
                                multiple = F
                    )
                } else {
                    if (input$select.melting.palette.fam == 'Brewer - diverging') {
                        pickerInput("select.melting.palette",
                                    label = "Choose palette",
                                    choices = list("BrBG", 'PiYG', 'PRGn',
                                                   'PuOr', 'RdBu', 'RdGy',
                                                   'RdYlBu', 'RdYlGn', 'Spectral'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.melting.palette.fam == 'Brewer - sequential') {
                            pickerInput("select.melting.palette",
                                        label = "Choose palette",
                                        choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                       'Greens', 'Greys', 'Oranges', 'OrRd',
                                                       'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                       'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                        multiple = F
                            )
                        } else {
                            if (input$select.melting.palette.fam == 'IGV') {
                                pickerInput("select.melting.palette",
                                            label = "Choose palette",
                                            choices = list("default", "alternating"),
                                            multiple = F
                                )
                            } else {
                                if (input$select.melting.palette.fam == 'UChicago') {
                                    pickerInput("select.melting.palette",
                                                label = "Choose palette",
                                                choices = list("default", "light", "dark"),
                                                multiple = F
                                    )
                                } else {
                                    return(NULL)
                                }
                            }
                        }
                    }
                }
            }

        })

    }

    # Run the application
    shinyApp(ui = ui, server = server)

}
