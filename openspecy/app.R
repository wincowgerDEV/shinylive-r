# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("s3_cred.csv") #file.exists("data/droptoken.rds") #remove for prototyping with maps
db <- file.exists("mongo.txt") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")
config_exists <- file.exists("config.yml")

#remotes::install_github("wincowgerDEV/OpenSpecy-package@vignettes")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(digest)
#library(curl)
library(config)
#if(db) library(mongolite)
library(loggit)
#webr::install("OpenSpecy") #Remove if not running with webr
library(OpenSpecy)
library(bs4Dash)
library(glmnet)
library(ggplot2)

#if(droptoken) library(aws.s3)


# Global config ----
if(config_exists){
  conf <- config::get() #Add config = "shinyapps" for ec2 server
} else{
  conf <- list(log = FALSE, share = FALSE)
}


# Logging ----
if(isTruthy(conf$log)) {
  if(db) {
    database <- mongo(url = readLines("mongo.txt"))
  } else {
    set_logfile(file.path(tempdir(), "OpenSpecy.log"))
  }
}

#if(is(tryCatch(check_lib(),error=function(e) e, warning=function(w) w), "warning") & !all(file.exists("data/mediod.rds"), file.exists("data/model.rds"), file.exists("data/nobaseline.rds"), file.exists("data/derivative.rds"))){
#  get_lib()
#}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")
  
  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                          intensity = raman_hdpe$spectra$intensity)
  
  if(droptoken) {
    creds <- read.csv("s3_cred.csv")
    
    Sys.setenv(
      "AWS_ACCESS_KEY_ID" = creds$Access.key.ID,
      "AWS_SECRET_ACCESS_KEY" = creds$Secret.access.key,
      "AWS_DEFAULT_REGION" = "us-east-2"
    )
  }
  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}

# Name keys for human readable column names ----

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))
citation <- HTML(
  "Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
  Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
  Classification Needs an Open Source Community: Open Specy to the Rescue!”
  <i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:
  <a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>."
)
ui <- dashboardPage(dark = T, 
                    help = T, 
                    fullscreen = T,
                    #Header ----
                    dashboardHeader( 
                      title = tags$a(href="https://www.openanalysis.org", 
                                     target="_blank",
                                     tags$img(src = "logo.png", 
                                              style = 'width: 15vw; padding:1rem;'))),
                    #Sidebar ----
                    dashboardSidebar(
                      sidebarUserPanel(
                        #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
                        name = "Welcome!"
                      ),
                      sidebarMenu(
                        id = "sidebarmenu",
                        menuItem(
                          "Analyze Spectra",
                          tabName = "analyze",
                          icon = icon("bar-chart")
                        ),
                        #sidebarHeader("Header 1"),
                        menuItem(
                          "About",
                          tabName = "about",
                          icon = icon("sliders-h")
                        ),
                        menuItem(
                          "Partner With Us",
                          tabName = "partner",
                          icon = icon("hands-helping")
                        ),
                        menuItem(
                          "Contract Us",
                          tabName = "contract",
                          icon = icon("file-contract")
                        )
                      )
                    ),
                    #Body ----
                    dashboardBody(
                      #Script for all pages ----
                      # Required for any of the shinyjs functions.
                      shinyjs::useShinyjs(),
                      
                      tags$head(
                        tags$script(async = T, src = "https://buttons.github.io/buttons.js"),
                        tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
                        HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>'),
                        tags$link(rel = "icon", type = "image/png", href = "favicon.png")
                        #This is for the error messages.
                      ),
                      tabItems(
                        # About Tab ----
                        tabItem(
                          tabName = "about",
                          accordion(
                            id = "accordion_welcome",
                            accordionItem(
                              title = "Welcome",
                              status = "info",
                              collapsed = F,
                              fluidRow(
                                column(6,
                                       p(class = "lead", "Join the hundreds of
                               researchers from around the world who are part of
                               the Open Specy community by
                               analyzing, sharing, processing, and identifying
                               their Raman and IR spectra."),
                                       p(class = "lead",
                                         HTML("<span style='position: relative; top:.6ex;'><a
                                      href='https://twitter.com/OpenSpecy?ref_src=twsrc%5Etfw'
                                      class='twitter-follow-button' data-size='large' data-dnt='true'
                                      data-show-count='false'>
                                      Follow @OpenSpecy</a></span>
                                      on Twitter")
                                       ),
                                       p(class = "lead",
                                         HTML("<span style='position: relative; top:.8ex;'><a
                                    class='github-button' href='https://github.com/wincowgerDEV/OpenSpecy/subscription'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-size='large' aria-label='Watch wincowgerDEV/OpenSpecy'>Watch</a></span>
                                    us develop Open Specy on GitHub, file an
                                    <span style='position: relative; top:.8ex;'><a
                                    class='github-button'
                                    href='https://github.com/wincowgerDEV/OpenSpecy/issues'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-icon='octicon-issue-opened' data-size='large'
                                    aria-label='Issue wincowgerDEV/OpenSpecy on GitHub'>Issue</a></span>,
                                    or request a feature")
                                       ),
                                       p(class = "lead",
                                         HTML("Or just e-mail <a href='mailto:wincowger@gmail.com?subject=Open Specy mailing list'>
                                          wincowger@gmail.com</a>
                                          to be added to the Open Specy mailing list")
                                       ),
                                       br(),
                                       p(class = "lead", "Open Specy is free and open
                               source thanks to our partners."),
                                       br(),
                                       p(class = "lead", "Looking for the classic version of OpenSpecy? Go to wincowger.shinyapps.io/openspecy-classic")),
                                column(6, HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/3RKufDxzriE' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                                )
                              )
                            )
                          ),
                          accordion(
                            id = "accordion_instructions",
                            accordionItem(
                              title = "Detailed Instructions",
                              status = "info",
                              collapsed = TRUE,
                              fluidRow(
                                column(6,
                                       HTML("<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/oWwRWwXf0sc' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>"),
                                       HTML("<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/cZZ3hgvIcao' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                                ),
                                column(6,
                                       tags$ol(
                                         tags$li("Upload a .zip, .csv, .0, .asp, .jdx, .spc, or .spa file to the upload file tab."),
                                         tags$li("Process your data using smoothing, derivative, baseline correction, flattening, range selection, and intensity adjustment."),
                                         tags$li("Identify your spectra using onboard reference libraries and/or AI"),
                                         tags$li("Download your results"),
                                         tags$li("For more details click the button below for the SOP or watch the detailed instructional videos.")
                                       ),
                                       a("SOP",
                                         onclick = "window.open('http://wincowger.com/OpenSpecy-package/articles/app.html', '_blank')",
                                         class="btn btn-primary btn-lg", 
                                         style = "width: 100%;")
                                )
                              )
                            )
                          ),
                          accordion(
                            id = "accordion_links",
                            accordionItem(
                              title = "Useful Links",
                              status = "info",
                              collapsed = TRUE,
                              a(href = "https://simple-plastics.eu/", "Free FTIR Software: siMPle microplastic IR spectral identification software", class = "lead"),
                              br(),
                              a(href = "https://gitlab.ipfdd.de/GEPARD/gepard", "Free Raman and FTIR Software: GEPARD (Gepard-Enabled PARticle Detection for Raman microscopes) Designed for particle-based microplastic analysis", class = "lead"),
                              br(),
                              a(href = "https://molview.org/", "Free chemical modeling tool with built in spectral query, MolView.", class = "lead"),
                              br(),
                              a(href = "https://webbook.nist.gov/", "Free spectroscopy and chemical database NIST Chemistry WebBook", class = "lead"),
                              br(),
                              a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher", class = "lead"),
                              br(),
                              a(href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University", class = "lead"),
                              br(),
                              a(href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.", class = "lead")   
                            )
                          )
                        ),
                        #Analyze Spectra Tab ----
                        tabItem("analyze", 
                                br(),
                                fluidRow(
                                  column(2,
                                         ##Upload/download ----
                                         #tags$label("Upload File"),
                                         fluidRow(style = "display: flex; align-items: flex-end;",
                                                  column(12, 
                                                         fileInput("file", NULL, multiple = T,
                                                                   placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, .0",
                                                                   accept=c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv", ".asp", ".spc", ".jdx", ".spa", ".0", ".zip", 
                                                                            ".json", ".rds", ".yml")) %>%
                                                           bs4Dash::popover(
                                                             title = "Upload Raman or FTIR spectrum files as a csv, zip, asp, jdx, spc, 0, or spa. A csv file is preferred. If a csv, the file must contain one column labeled wavenumber in units of (1/cm) and another column labeled intensity in absorbance units. If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). If zip, batch upload using a zip file with multiple spectral files that all have the same wavenumbers or a map file formatted as .hdr and .dat. Hit the Sample button to download a sample Raman spectrum.",
                                                             content = "File Upload", placement = "right"
                                                           ),
                                                         prettySwitch("share_decision",
                                                                      label = "Share Your Data?",
                                                                      inline = T,
                                                                      value = T,
                                                                      status = "success",
                                                                      fill = T) %>%
                                                           popover(
                                                             title = "If you like, we share your uploaded spectra and settings with the spectroscopy community. By default, all data will be licensed under Creative Commons Attribution 4.0 International (CC BY 4.0). Uploaded spectra will appear here: https://osf.io/rjg3c. If you have spectra of known identities you can share, please upload a JDX file titled with the name of the material it is.",
                                                             content = "Share Decision", placement = "right"
                                                           )
                                                  )
                                         )
                                  ),
                                  column(8,
                                         fluidRow( 
                                           column(6, 
                                                  ## Preprocessing ----
                                                  fluidRow(
                                                    box(width = 12,
                                                        collapsed = T,
                                                        style = "height: 50vh; overflow-y: auto;",
                                                        footer = tags$small("Options for processing the spectra."),
                                                        title = prettySwitch(inputId = "active_preprocessing",
                                                                             label = "Preprocessing",
                                                                             inline = T,
                                                                             value = T,
                                                                             status = "success",
                                                                             fill = T),
                                                        fluidRow(
                                                          box(width = 12,
                                                              footer = tags$small("Signal thresholding technique, value, and histogram threshold plot."),
                                                              title = prettySwitch("threshold_decision",
                                                                                   label = "Threshold Signal-Noise",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              collapsed = T,
                                                              numericInput(
                                                                "MinSNR",
                                                                "Minimum Value",
                                                                value = 4,
                                                                min = -10000,
                                                                max = 10000,
                                                                step = 1
                                                              ),
                                                              br(),
                                                              selectInput(inputId = "signal_selection", 
                                                                          label = "Signal Thresholding Technique", 
                                                                          choices = c("Signal Over Noise" = "run_sig_over_noise", 
                                                                                      "Signal Times Noise" = "sig_times_noise", 
                                                                                      "Total Signal" = "log_tot_sig")), 
                                                              br(), 
                                                              plotOutput("snr_plot", height = "10vh")
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              footer = tags$small("Min-Max normalization improves comparability, for many applications, between spectra except in cases
                                                                                                where raw intensity values are necessary for interpreation. For example raw values can be useful for thresholding. 
                                                                                                Min-Max normalization rescales spectral intensity values between 0-1"),
                                                              title = prettySwitch("make_rel_decision",
                                                                                   label = "Min-Max Normalize",
                                                                                   inline = T,
                                                                                   value = T,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              collapsed = T
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Smoothing can enhance signal to noise and uses the SG filter with the polynomial order specified, 3 default usually works well. 
                                                                            Derivative transformation uses the order specified. 
                                                                            If doing identification with a derivative library, 1 is required, 0 should be used if no derivative transformation is desired. 
                                                                            Smoothing uses the SG filter on an window of points, specifying the wavenumber window larger will make the spectra more smooth.
                                                                            The absolute value does something similar to intensity correction to make the spectra more absorbance-like."),
                                                              title =  prettySwitch(inputId = "smooth_decision",
                                                                                    label = "Smoothing/Derivative",
                                                                                    inline = T,
                                                                                    value = T,
                                                                                    status = "success",
                                                                                    fill = T),
                                                              sliderInput("smoother", "Polynomial", min = 0, max = 5, value = 3),
                                                              sliderInput("derivative_order", "Derivative Order", min = 0, max = 3, value = 1),
                                                              sliderInput("smoother_window", "Wavenumber Window", min = 50, max = 200, value = 90, step = 5),
                                                              prettySwitch("derivative_abs", 
                                                                           label = "Absolute Value",  
                                                                           inline = T,
                                                                           value = T,
                                                                           status = "success",
                                                                           fill = T))),
                                                        fluidRow(
                                                          box(width = 12,
                                                              footer = tags$small("Options for conforming spectra to a new wavenumber resolution.
                                                                                                Conformation technique specifies the strategy for performing the conformation. 
                                                                                                Nearest will use the nearest value to the wavenumber resolution specified, this is 
                                                                                                faster but less accurate. Linear Interpolation will perform a linear regression between 
                                                                                                the nearest points to identify the intensity values at the new wavenumbers. Wavenumber Resolution 
                                                                                                will set the step size in wavenumbers for the new wavenumber values."),
                                                              title = prettySwitch("conform_decision",
                                                                                   label = "Conform Wavenumbers",
                                                                                   inline = T,
                                                                                   value = T,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              collapsed = T,
                                                              selectInput(inputId = "conform_selection", 
                                                                          label = "Conformation Technique", 
                                                                          choices = c("Linear Interpolation" = "interp",
                                                                                      "Nearest" = "roll")), 
                                                              br(),
                                                              sliderInput("conform_res", "Wavenumber Resolution", min = 4, max = 16, value = 8)
                                                              
                                                          )
                                                        ),
                                                        fluidRow(
                                                          box(
                                                            width = 12,
                                                            collapsed = T,
                                                            footer = tags$small("Open Specy assumes spectra are in Absorbance units. If the uploaded spectrum is not in absorbance units, 
                                                                    use this input to specify the units to convert from.The transmittance adjustment uses the log10(1/T) calculation 
                                                                    which does not correct for system and particle characteristics. The reflectance adjustment uses the Kubelka-Munk 
                                                                    equation (1-R)2/(2*R). We assume that the reflectance is formatted as a percent from 1-100 and first correct the 
                                                                    intensity by dividing by 100 so that it fits the form expected by the equation. If none is selected, Open Specy
                                                                    assumes that the uploaded data is an absorbance spectrum."),
                                                            title =  prettySwitch(inputId = "intensity_decision",
                                                                                  label = "Intensity Adjustment",
                                                                                  value = F,
                                                                                  inline = T,
                                                                                  status = "success",
                                                                                  fill = T),
                                                            radioButtons("intensity_corr", "Intensity Units",
                                                                         c("Absorbance" = "none", "Transmittance" = "transmittance", "Reflectance" = "reflectance"))
                                                          )),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("This algorithm automatically fits to the baseline by fitting 
                                                                                     polynomials of the provided order to the whole spectrum using the iModPolyFit algorithm."),
                                                              title = prettySwitch("baseline_decision",
                                                                                   label = "Baseline Correction",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              sliderInput("baseline", "Baseline Correction Polynomial", min = 1, max = 20, value = 8)
                                                          )),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching.
                                                                                     These options control the maximum and minimum wavenumbers in the range to crop the spectra."),
                                                              title =  prettySwitch("range_decision",
                                                                                    label = "Range Selection",
                                                                                    inline = T,
                                                                                    value = F,
                                                                                    status = "success",
                                                                                    fill = T),
                                                              numericInput(
                                                                "MinRange",
                                                                "Minimum Wavenumber",
                                                                value = 300,
                                                                min = NA,
                                                                max = NA,
                                                                step = NA,
                                                                width = NULL
                                                              ),
                                                              numericInput(
                                                                "MaxRange",
                                                                "Maximum Wavenumber",
                                                                value = 2000,
                                                                min = NA,
                                                                max = NA,
                                                                step = NA,
                                                                width = NULL
                                                              ))),
                                                        fluidRow(
                                                          box(width = 12,
                                                              collapsed = T,
                                                              footer = tags$small("Sometimes peaks are undersireable. 
                                                                                     These options will replace peak regions with the mean of their edges. 
                                                                                     Specify the edge locations of the peaks minimum and maximum wavenumbers to use for flattening.
                                                                                     Defaults are set to flatten the CO2 region in infrared spectra."),
                                                              title = prettySwitch("co2_decision",
                                                                                   label = "Flatten Region",
                                                                                   inline = T,
                                                                                   value = F,
                                                                                   status = "success",
                                                                                   fill = T),
                                                              numericInput(
                                                                "MinFlat",
                                                                "Minimum Wavenumber",
                                                                value = 2200,
                                                                min = 1,
                                                                max = 6000,
                                                                step = 1
                                                              ),
                                                              numericInput(
                                                                "MaxFlat",
                                                                "Maximum Wavenumber",
                                                                value = 2400,
                                                                min = 1,
                                                                max = 6000,
                                                                step = 1
                                                              )))
                                                        
                                                    )
                                                  )
                                           ),
                                           ## Identification ----
                                           column(6, 
                                                  fluidRow(
                                                    box(width = 12,
                                                        collapsed = T,
                                                        footer = tags$small("These options define the strategy for identification.
                                                                                    The ID Library will inform which library is used. Both (default) will search both
                                                                                    FTIR and Raman libraries. Deriv will search against a derivative transformed library. 
                                                                                    No Baseline will search against a baseline corrected library. This should be in line 
                                                                                    with how you choose to process your spectra. Cor options use a simple Pearson correlation
                                                                                    search algorithm. AI is uses either a multinomial model (experimental) or 
                                                                                    correlation on mediod (default) spectra from the library. Correlation thresholding will set the minimum 
                                                                                    value from matching to use as a 'positive identification'"),
                                                        title = prettySwitch(inputId = "active_identification",
                                                                             label = "Identification",
                                                                             inline = T,
                                                                             value = F,
                                                                             status = "success",
                                                                             fill = T),
                                                        pickerInput(inputId = "id_strategy", label =  "ID Library",
                                                                    choices =  c("Cor: Both Deriv" = "both_deriv",
                                                                                 "Cor: Both No Baseline" = "both_nobaseline",
                                                                                 "Cor: FTIR Deriv" = "ftir_deriv",
                                                                                 "Cor: Raman Deriv" = "raman_deriv",
                                                                                 "Cor: FTIR No Baseline" = "ftir_nobaseline",
                                                                                 "Cor: Raman No Baseline" = "raman_nobaseline",
                                                                                 "AI: FTIR Deriv Multinomial" = "ai", 
                                                                                 "AI: Both Deriv Mediod" = "mediod")),
                                                        fluidRow(
                                                          box(width = 12, 
                                                              collapsed = T,
                                                              title = prettySwitch("cor_threshold_decision",
                                                                                   label = "Threshold Correlation",
                                                                                   inline = T,
                                                                                   value = T,
                                                                                   status = "success",
                                                                                   fill = T), 
                                                              numericInput(
                                                                "MinCor",
                                                                "Minimum Value",
                                                                value = 0.7,
                                                                min = 0,
                                                                max = 1,
                                                                step = 0.1#,
                                                                #width = '25%'
                                                              ),
                                                              plotOutput("cor_plot", height = "10vh")
                                                              
                                                          )
                                                        )
                                                    )  
                                                  )
                                           ))),
                                  column(2,
                                         selectInput(inputId = "download_selection",
                                                     label = downloadButton("download_data",
                                                                            style = "background-color: rgb(0,0,0); color: rgb(255,255,255);"),
                                                     choices = c("Test Data",
                                                                 "Test Map",
                                                                 "Your Spectra",
                                                                 "Library Spectra",
                                                                 "Top Matches",
                                                                 "Thresholded Particles")) %>%
                                           popover(
                                             title = "Options for downloading spectra and metadata from the analysis.
                                          Test Data is a Raman HDPE spectrum in csv format. Test Map is an FTIR ENVI file of a CA particle.
                                          Your Spectra will download your data with whatever processing options are active. Library Spectra
                                          will download the current library selected. Top Matches downloads the top identifications in the
                                          active analysis. Thresholded Particles will download a version of your spectra using the active
                                          thresholds selected to infer where particles are in spectral maps, particle spectra are collapsed
                                          to their medians and locations to their centroids.",
                                             content = "Download Options", placement = "left"
                                           )
                                  )
                                ),
                                ## Plot ----
                                fluidRow(
                                  #verbatimTextOutput("event_test"),
                                  box(title = HTML(paste0("Spectra")), 
                                      maximizable = T,
                                      width = 12,
                                      #background = "black",
                                      label = uiOutput("correlation_head"),
                                      h4(id = "placeholder1", "Upload some data to get started..."),
                                      uiOutput("progress_bars"),
                                      fluidRow(
                                        plotlyOutput("heatmap",inline = T),
                                        plotlyOutput("MyPlotC", inline = T),
                                        div(style = "overflow-x: scroll",
                                            DT::dataTableOutput("eventmetadata")   
                                        )),
                                      dropdownMenu = boxDropdown(
                                        boxDropdownItem("Bad Processing or Library Spectra", id = "bad_spec", icon = icon("face-sad-tear"))
                                      ),
                                      sidebar = boxSidebar(
                                        id = "mycardsidebar",
                                        fluidRow(style = "padding:1rem; overflow-x: scroll",
                                                 DT::dataTableOutput("event"))
                                      )
                                  )
                                )
                        ),
                        tabItem("partner", 
                                #Partner With Us tab ----
                                titlePanel(h4("Help us reach our goal to revolutionize spectroscopy.")),
                                br(),
                                accordion(
                                  id = "accordion_partners",
                                  accordionItem(
                                    title = "Partners",
                                    status = "info",
                                    collapsed = T,
                                    fluidRow(
                                      column(6,
                                             h3("Monetary Partners"),
                                             panel(style = "align: centre",
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                                                       h3("Thriving (10,000–100,000$)"),
                                                       img(src = "https://mooreplasticresearch.org/wp-content/uploads/2021/06/HorizontalLogo-FullName-1.png", style = "width:20vw"),
                                                       img(src = "https://www.helmholtz-hida.de/typo3conf/ext/hida_site_package/Resources/Public/dest/images/logos/hida-logo.svg", style = "width:20vw"),
                                                       img(src = "https://infrastructure.der-lab.net/wp-content/uploads/2017/05/logo_nrel_c.jpg", style = "width:20vw"),
                                                       img(src = "https://mcpzfoundation.org/wp-content/uploads/2021/07/McPZ-Logo-Horizontal-RGB.png", style = "width:20vw")
                                                   ),
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                                                       h3("Maintaining (1,000–10,000$)"),
                                                       img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/UC_Riverside_logo.svg/1024px-UC_Riverside_logo.svg.png", style = "width:10vw"),
                                                       img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7e/NSF_logo.png", style = "width:10vw"),
                                                       img(src = "https://www.awi.de/typo3conf/ext/sms_boilerplate/Resources/Public/Images/AWI/awi_logo.svg", style = "width:10vw"),
                                                       img(src = "https://www.hpu.edu/_global/images/header-logo.png", style = "width:10vw"),
                                                       img(src = "https://www.nist.gov/libraries/nist-component-library/dist/img/logo/nist_logo_sidestack_rev.svg", style = "width:10vw"),
                                                       img(src = "https://www.utoronto.ca/sites/all/themes/uoft_stark/img/U-of-T-logo.svg", style = "width:10vw"),
                                                       img(src = "https://www.uni-koblenz-landau.de/logo.png", style = "width:10vw"),
                                                       img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Thermo_Fisher_Scientific_logo.svg/2560px-Thermo_Fisher_Scientific_logo.svg.png", style = "width:10vw")
                                                   ),
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                                                       h3("Supporting (100–1,000$)"),
                                                       h5( "Jennifer Gadd")
                                                   ),
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(128, 0, 128, 0.5)",
                                                       h3("Saving (<100$)"),
                                                       h6("Anne Jefferson, Heather Szafranski, Gwendolyn Lattin, Collin Weber, Gregory Gearhart, Anika Ballent, Shelly Moore, Susanne Brander (Oregon State University), Jeremy Conkle (TEXAS  A&M  UNIVERSITY  CORPUS  CHRISTI)")
                                                   )
                                             )
                                      ),
                                      column(6,
                                             h3("In-Kind Partners"),
                                             panel(style = "align: centre",
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(205, 127, 50, 0.5)",
                                                       h3("Thriving (10,000–100,000$)"),
                                                       h4("Win Cowger, Zacharias Steinmetz")
                                                   ),
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(3, 252, 15, 0.5)",
                                                       h3("Maintaining (1,000–10,000$)"),
                                                       h5("Garth Covernton, Jamie Leonard, Shelly Moore, Rachel Kozloski, Katherine Lasdin, Aleksandra Karapetrova, Laura Markley, Walter Yu, Walter Waldman, Vesna Teofilovic, Monica Arienzo, Mary Fey Long Norris, Cristiane Vidal, Scott Coffin, Charles Moore, Aline Carvalho, Shreyas Patankar, Andrea Faltynkova, Sebastian Primpke, Andrew Gray, Chelsea Rochman, Orestis Herodotu, Hannah De Frond, Keenan Munno, Hannah Hapich, Jennifer Lynch")
                                                   ),
                                                   div(class = "jumbotron",
                                                       style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(0, 0, 255, 0.5)",
                                                       h3("Supporting (100–1,000$)"),
                                                       h6("Alexandre Dehaut, Gabriel Erni Cassola")
                                                   )
                                             )
                                      )
                                    )
                                  ),
                                  accordionItem(
                                    title = "Donate Cash",
                                    status = "info",
                                    collapsed = TRUE,
                                    #img(src = "https://p.turbosquid.com/ts-thumb/rX/Wm1eqB/t5/currencysymbolsgoldensetc4dmodel000/jpg/1613802168/300x300/sharp_fit_q85/a31625492ce9c8009ab3e4281ad752006e1163ec/currencysymbolsgoldensetc4dmodel000.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                    actionButton(inputId = "ab1", label = "Donate", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                 icon = icon("donate"),
                                                 onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')")
                                  ),
                                  accordionItem(
                                    title = "Buy Merch",
                                    status = "info",
                                    collapsed = TRUE,
                                    img(src = "https://image.spreadshirtmedia.com/image-server/v1/products/T813A823PA3132PT17X42Y46D1038541132FS4033/views/1,width=650,height=650,appearanceId=823/updated-logo-for-open-specy-designed-by-alex-mcgoran.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                    actionButton(inputId = "ab2", label = "Shop", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                 icon = icon("shopping-cart"),
                                                 onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")
                                  ),
                                  accordionItem(
                                    title = "Contribute Time",
                                    status = "info",
                                    collapsed = T,
                                    img(src = "https://health.sunnybrook.ca/wp-content/uploads/2020/02/healthy-hands-810x424.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                    actionButton(inputId = "ab3", label = "Guidelines", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                 icon = icon("clock"),
                                                 onclick ="window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')")
                                  ),
                                  
                                  accordionItem(
                                    title = "Contribute Spectra",
                                    status = "info",
                                    collapsed = TRUE,
                                    p(class = "lead", "To share spectra upload a file to the upload file tab.
                             If you selected Share a copy of your spectra will be sent to the Community
                             Data Warehouse on Open Science Framework. To add additional metadata,
                             fill in the avaliable metadata fields and click -Share Data-. The
                             spectra file that you uploaded along with your responses will be copied
                             to the a -With Metadata- subfolder at the link below. All shared data holds
                             a Creative Commons Attribution License 4.0."),
                                    div(
                                      a("Community Data Warehouse",
                                        onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                        class="btn btn-primary btn-lg",
                                        style = "width: 100%;")
                                    )
                                  )
                                )
                        ),
                        tabItem("contract",
                                div(
                                  h2("We are a group of experienced spectroscopists and can provide a variety of services for hire, please contact wincowger@gmail.com to inquire about any of the services below.", style = "color: lightblue;"),
                                  h3(tags$ul(
                                    tags$li("Adding new features to OpenSpecy"),
                                    tags$li("Creating spectroscopy software"),
                                    tags$li("Microplastic sample analysis"),
                                    tags$li("Spectral identification"),
                                    tags$li("Study design"),
                                    tags$li("So much more...")
                                  ), style = "color: lightyellow;"), 
                                  style = "padding: 50px"
                                )
                                
                        )
                      )
                    ),
                    
                    #Footer ----
                    footer = dashboardFooter(
                      left = p(citation),
                      right = HTML(paste0(uiOutput("translate"), 
                                          a(href = "TOS.txt", "Terms And Conditions", class = "lead"),
                                          br(),
                                          a(href = "privacy_policy.txt", "Privacy Policy", class = "lead")
                      ),
                      #Ethical Ads
                      HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="text" data-ea-style="fixedfooter"></div>')
                      )
                    )
)



server <- function(input, output, session) {
  
  #Setup ----
  
  #Set upload size
  if(isTruthy(conf$share) && conf$share != "system"){options(shiny.maxRequestSize = 1000*1024^2)} else{options(shiny.maxRequestSize = 10000*1024^2)}
  
  #create a random session id
  session_id <- digest(runif(10))
  
  # Loading overlay
  load_data()
  hide(id = "loading_overlay", anim = TRUE, animType = "fade")
  show("app_content")
  
  preprocessed <- reactiveValues(data = NULL)
  data_click <- reactiveValues(data = NULL)
  
  
  #Read Data ----
  #Sending data to a remote repo. 
  observeEvent(input$file, {
    # Read in data when uploaded based on the file type
    req(input$file)
    data_click$data <- 1
    preprocessed$data <- NULL
    
    if (!all(grepl("(\\.json$)|(\\.rds$)|(\\.yml$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
                   ignore.case = T, as.character(input$file$datapath)))) {
      show_alert(
        title = "Data type not supported!",
        text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
        type = "warning")
      return(NULL)
    }
    
    if (input$share_decision & curl::has_internet()) {
      progm <- "Sharing Spectrum to Community Library"
    } else {
      progm <- "Reading Spectrum"
    }
    
    withProgress(message = progm, value = 3/3, {
      
      rout <- tryCatch(expr = {
        read_any(file = as.character(input$file$datapath)) |>
          c_spec(os, range = "common", res = 8) |>
          manage_na(ig = c(NA, 0), type = "remove")},
        error = function(e){
          class(e$message) <- "simpleWarning"
          e$message
        }#,
        #warning = function(w){
        #class(w$message) <- "simpleWarning"
        #    w$message
        #}
      )
      #print(rout)
      
      checkit <- tryCatch(expr = {check_OpenSpecy(rout)},
                          error = function(e){
                            class(e$message) <- "simpleWarning"
                            e$message
                          },
                          warning = function(w){
                            class(w$message) <- "simpleWarning"
                            w$message
                          })
      #print(checkit)
      if (inherits(rout, "simpleWarning") | inherits(checkit, "simpleWarning")) {
        show_alert(
          title = "Something went wrong with reading the data :-(",
          text =  paste0(if(inherits(rout, "simpleWarning")){paste0("There was an error during data loading that said ", 
                                                                    rout, ".")} else{""},
                         if(inherits(checkit, "simpleWarning")){paste0(" There was an error during data checking that said ", 
                                                                       checkit, ".")} else{""},
                         ". If you uploaded a text/csv file, make sure that the columns are numeric and named 'wavenumber' and 'intensity'."),
          type =  "error"
        )
        reset("file")
        preprocessed$data <- NULL
      }
      
      else {
        if(length(input$file$datapath) == 1){
          if(droptoken & input$share_decision & input$file$size < 10^7 & curl::has_internet()){
            put_object(
              file = file.path(as.character(input$file$datapath)),
              object = paste0("users/", session_id, "/", digest(rout), "/", gsub(".*/", "", as.character(input$file$name))),
              bucket = "openspecy"
            )
          }
        }
        preprocessed$data <- rout 
        #print(preprocessed$data)
      }
    })
  })
  
  #The matching library to use. 
  libraryR <- reactive({
    req(input$active_identification)
    if(input$id_strategy == "mediod"){
      if(file.exists("data/mediod.rds")){
        library <- read_any("data/mediod.rds")
      }
      else{
        library <- load_lib("mediod")
      }
      return(library)
    }
    else if(grepl("ai$", input$id_strategy)) {
      if(file.exists("data/model.rds")){
        library <- read_any("data/model.rds")
      }
      else{
        library <- load_lib("model")
      }
      return(library)
    }
    else if(grepl("nobaseline$", input$id_strategy)) {
      if(file.exists("data/nobaseline.rds")){
        library <- read_any("data/nobaseline.rds")
      }
      else{
        library <- load_lib("nobaseline")
      }
    }
    else if(grepl("deriv$", input$id_strategy)){
      if(file.exists("data/derivative.rds")){
        library <- read_any("data/derivative.rds")
      }
      else{
        library <- load_lib("derivative")
      }
    }
    if(grepl("^both", input$id_strategy)) {
      library
    }
    else if (grepl("^ftir", input$id_strategy)){
      filter_spec(library, logic = library$metadata$spectrum_type == "ftir")
    }
    else if (grepl("^raman", input$id_strategy)){
      filter_spec(library, logic = library$metadata$spectrum_type == "raman")
    }
  })
  
  # Corrects spectral intensity units using the user specified correction
  
  # Redirecting preprocessed data to be a reactive variable. Not totally sure why this is happening in addition to the other. 
  data <- reactive({
    req(input$file)
    preprocessed$data
  })
  
  #Preprocess ----
  
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
    req(!is.null(preprocessed$data))
    req(input$active_preprocessing)
    process_spec(x = data(),
                 active = input$active_preprocessing,
                 adj_intens = input$intensity_decision, 
                 adj_intens_args = list(type = input$intensity_corr),
                 conform_spec = input$conform_decision, 
                 conform_spec_args = list(range = seq(100, 
                                                      4000, 
                                                      by = input$conform_res), 
                                          res = NULL, 
                                          type = input$conform_selection),
                 restrict_range = input$range_decision,
                 restrict_range_args = list(min = input$MinRange, 
                                            max = input$MaxRange),
                 flatten_range = input$co2_decision,
                 flatten_range_args = list(min = input$MinFlat, 
                                           max = input$MaxFlat),
                 subtr_baseline = input$baseline_decision, 
                 subtr_baseline_args = list(type = "polynomial", 
                                            degree = input$baseline, 
                                            raw = FALSE, 
                                            baseline = NULL),
                 smooth_intens = input$smooth_decision, 
                 smooth_intens_args = list(polynomial = input$smoother, 
                                           window = calc_window_points(if(input$conform_decision){seq(100, 
                                                                                                      4000, 
                                                                                                      by = input$conform_res)} 
                                                                       else{data()}, 
                                                                       input$smoother_window), 
                                           derivative = input$derivative_order, 
                                           abs = input$derivative_abs),
                 make_rel = input$make_rel_decision)
  })
  
  
  # Choose which spectra to use for matching and plotting. 
  DataR <- reactive({
    req(!is.null(preprocessed$data))
    if(input$active_preprocessing) {
      baseline_data()
    }
    else {
      data()
    }
  })
  
  #The data to use in the plot. 
  DataR_plot <- reactive({
    if(is.null(preprocessed$data)){
      list(wavenumber = numeric(), spectra = data.table(empty = numeric()))
    }
    else{
      filter_spec(DataR(), logic = 1:ncol(DataR()$spectra) == data_click$data)
    }
  })
  
  # SNR ----
  #The signal to noise ratio
  signal_to_noise <- reactive({
    req(!is.null(preprocessed$data))
    sig_noise(x = DataR(), step = 10, metric = input$signal_selection, abs = F)
  })
  
  MinSNR <- reactive({
    req(!is.null(preprocessed$data))
    if(!input$threshold_decision){
      -Inf
    }
    else{
      input$MinSNR
    }
  })
  
  
  
  #Identification ----
  output$correlation_head <- renderUI({
    req(!is.null(preprocessed$data), (input$threshold_decision | input$cor_threshold_decision))
    
    good_cor <- max_cor()[[data_click$data]] > MinCor() & signal_to_noise()[[data_click$data]] > MinSNR()
    good_sig <- signal_to_noise()[[data_click$data]] > MinSNR()
    good_match <- good_cor & good_sig
    
    boxLabel(text = if(input$cor_threshold_decision & input$threshold_decision & input$active_identification) {"Match"} else if(input$cor_threshold_decision & input$active_identification) {"Cor"} else if (input$threshold_decision){"SNR"} else{""}, 
             status = if(input$cor_threshold_decision & input$threshold_decision & input$active_identification) {
               if(good_match){
                 "success"
               } 
               else{
                 "error"
               }
             }
             else if(input$cor_threshold_decision & input$active_identification){
               if(good_cor){
                 "success"
               } 
               else {
                 "error"
               }
             }
             else if(input$threshold_decision){
               if(good_sig){
                 "success"
               } 
               else{
                 "error"
               }
             }
             else{NULL}, 
             tooltip = "This tells you whether the signal to noise ratio or the match observed is above or below the thresholds.")
  })
  
  #The correlation matrix between the unknowns and the library. 
  correlation <- reactive({
    req(!is.null(preprocessed$data))
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    withProgress(message = 'Analyzing Spectrum', value = 1/3, {
      cor_spec(x = DataR(), 
               library = libraryR(),
               conform = T, 
               type = "roll")
    })
  })
  
  #The output from the AI classification algorithm. 
  ai_output <- reactive({ #tested working. 
    req(!is.null(preprocessed$data))
    req(input$active_identification)
    req(input$id_strategy == "ai")
    rn <- runif(n = length(unique(libraryR()$variables_in)))
    fill <- as_OpenSpecy(as.numeric(unique(libraryR()$variables_in)),
                         spectra = data.frame(rn))
    match_spec(DataR(), library = libraryR(), na.rm = T, fill = fill)
  })
  
  #The maximum correlation or AI value. 
  max_cor <- reactive({
    req(!is.null(preprocessed$data))
    #req(input$active_identification)
    if(isTruthy(input$active_identification)){
      if(!grepl("^ai$", input$id_strategy)){
        max_cor_named(correlation())
      }
      else if(input$id_strategy == "ai"){
        ai <- signif(ai_output()[["value"]], 2)
        names(ai) <- ai_output()[["name"]]
        ai
      }
    }
    else{
      NULL
    }
  })
  
  MinCor <- reactive({
    req(!is.null(preprocessed$data))
    if(!input$cor_threshold_decision){
      -Inf
    }
    else{
      input$MinCor
    }
  })
  
  output$cor_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    ggplot() +
      geom_histogram(aes(x = max_cor())) +
      scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
      geom_vline(xintercept = MinCor(), color = "red") +
      theme_minimal()
  })
  
  #Metadata for all the top correlations.
  top_correlation <- reactive({
    req(!is.null(preprocessed$data))
    req(input$active_identification)
    data.table(object_id = names(DataR()$spectra), 
               library_id = names(max_cor()),
               match_val = max_cor(), 
               match_threshold = MinCor(),
               good_correlations = max_cor() > MinCor(),
               signal_to_noise = signal_to_noise(), 
               signal_threshold = MinSNR(),
               good_signal = signal_to_noise() > MinSNR(), 
               good_matches = max_cor() > MinCor() & signal_to_noise() > MinSNR()) %>%
      {if(!grepl("^ai$", input$id_strategy)){bind_cols(., DataR()$metadata)} else{.}} %>%
      {if(!grepl("^ai$", input$id_strategy)){left_join(., libraryR()$metadata, by = c("library_id" = "sample_name"))} else{.}}
    
  })
  
  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    if(is.null(preprocessed$data)){
      libraryR()$metadata %>%
        mutate("Pearson's r" = NA) %>%
        rename("Material" = "spectrum_identity",
               "Plastic Pollution Category" = "material_class",
               "library_id" = "sample_name")
    }
    else{
      data.table(object_id = names(DataR()$spectra)[data_click$data], 
                 library_id = names(libraryR()$spectra),
                 match_val = c(correlation()[,data_click$data]))[order(-match_val),] %>%
        left_join(libraryR()$metadata, by = c("library_id" = "sample_name")) %>%
        mutate(match_val = signif(match_val, 2)) %>%
        rename("Material" = "spectrum_identity",
               "Pearson's r" = "match_val",
               "Plastic Pollution Category" = "material_class")
    }
  })
  
  #Spectral data for the selected match. 
  match_selected <- reactive({# Default to first row if not yet clicked
    #req(input$file)
    #req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    if(!input$active_identification) {
      as_OpenSpecy(x = numeric(), spectra = data.table(empty = numeric()))
    }
    else{
      #need to make reactive
      id_select <-  ifelse(is.null(input$event_rows_selected),
                           matches_to_single()[[1,"library_id"]],
                           matches_to_single()[[input$event_rows_selected,"library_id"]])#"00087f78d45c571524fce483ef10752e"	#matches_to_single[[1,column_name]]
      
      # Get data from filter_spec
      filter_spec(libraryR(), logic = id_select)
    }
  })
  
  #All matches table for the current selection
  top_matches <- reactive({
    #req(input$file)
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    if(is.null(preprocessed$data)){
      matches_to_single() %>%
        dplyr::select("Material",
                      "Plastic Pollution Category", 
                      "organization",
                      "library_id")
    }
    else{
      matches_to_single() %>%
        dplyr::select("Pearson's r",
                      "Material",
                      "Plastic Pollution Category", 
                      "organization",
                      "library_id")
    }
  })
  
  #Create the data table that goes below the plot which provides extra metadata. 
  match_metadata <- reactive({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    matches_to_single()[input$event_rows_selected,] %>%
      .[, !sapply(., OpenSpecy::is_empty_vector), with = F] #%>%
    # dplyr::select("Material",
    #                  "Plastic Pollution Category", 
    #                  "library_id", everything())
  })
  
  # Display ----
  
  #Histogram of SNR
  output$snr_plot <- renderPlot({
    req(!is.null(preprocessed$data))
    ggplot() +
      geom_histogram(aes(x = signal_to_noise())) +
      scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
      geom_vline(xintercept = MinSNR(), color = "red") +
      theme_minimal()
  })
  
  #Table of metadata for the selected library value
  output$eventmetadata <- DT::renderDataTable({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    datatable(match_metadata(),
              escape = FALSE,
              options = list(dom = 't', bSort = F,
                             scrollX = TRUE,
                             lengthChange = FALSE,
                             info = FALSE),
              rownames = FALSE,
              style = 'bootstrap', caption = "Selection Metadata",
              selection = list(mode = 'none'))
  })
  
  # Create the data tables for all matches
  output$event <- DT::renderDataTable({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    datatable(top_matches() %>%
                mutate(organization = as.factor(organization),
                       `Plastic Pollution Category` = as.factor(`Plastic Pollution Category`)),
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              rownames = FALSE,
              filter = "top", caption = "Selectable Matches",
              style = "bootstrap",
              selection = list(mode = "single", selected = c(1)))
  })
  
  # Progress Bars
  output$progress_bars <- renderUI({
    req(ncol(preprocessed$data$spectra) > 1)
    req(input$threshold_decision | (input$cor_threshold_decision & input$active_identification))
    
    if(input$threshold_decision & (!input$cor_threshold_decision | !input$active_identification)){
      tagList(
        fluidRow(
          column(6, selectInput(inputId = "map_color", label = "Map Color", choices = "Signal/Noise"))
        ),
        fluidRow(
          column(4, 
                 shinyWidgets::progressBar(id = "signal_progress", value = sum(signal_to_noise() > MinSNR())/length(signal_to_noise()) * 100, status = "success", title = "Good Signal", display_pct = TRUE)
          )
        )
      )
    } else if(!input$threshold_decision & input$cor_threshold_decision & input$active_identification){
      tagList(
        fluidRow(
          column(6, selectInput(inputId = "map_color", label = "Map Color", choices = c("Match Name", "Correlation", "Match ID")))
        ), 
        fluidRow(
          column(4, 
                 shinyWidgets::progressBar(id = "correlation_progress", value = sum(max_cor() > MinCor())/length(max_cor()) * 100, status = "success", title = "Good Correlations", display_pct = TRUE)
          )
        )
      )
    } else {
      tagList(
        fluidRow(
          column(6, selectInput(inputId = "map_color", label = "Map Color", choices = c("Match Name", "Correlation", "Match ID", "Signal/Noise")))
        ),
        fluidRow(
          column(4, 
                 shinyWidgets::progressBar(id = "signal_progress", value = sum(signal_to_noise() > MinSNR())/length(signal_to_noise()) * 100, status = "success", title = "Good Signal", display_pct = TRUE)
          ),
          column(4, 
                 shinyWidgets::progressBar(id = "correlation_progress", value = sum(max_cor() > MinCor())/length(max_cor()) * 100, status = "success", title = "Good Correlations", display_pct = TRUE)
          ),
          column(4,
                 shinyWidgets::progressBar(id = "match_progress", value = sum(signal_to_noise() > MinSNR() & max_cor() > MinCor())/length(signal_to_noise()) * 100, status = "success", title = "Good Identifications", display_pct = TRUE)
          )
        )
      )
    }
  })
  
  output$MyPlotC <- renderPlotly({
    #req(input$id_strategy == "correlation")
    #req(preprocessed$data)
    plotly_spec(x = if(!is.null(preprocessed$data)){DataR_plot()} else{match_selected()},x2 = if(!is.null(preprocessed$data) & !grepl("^ai$", input$id_strategy)) {match_selected()} else{NULL}, source = "B") %>%
      config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
  })
  
  #Heatmap ----
  #Display the map or batch data in a selectable heatmap. 
  output$heatmap <- renderPlotly({
    req(!is.null(preprocessed$data))
    req(ncol(preprocessed$data$spectra) > 1)
    #req(input$map_color)
    
    heatmap_spec(x = DataR(), 
                 z = if(!is.null(max_cor()) & !isTruthy(input$map_color)){
                   max_cor()
                 }
                 else if(!is.null(signal_to_noise()) & !isTruthy(input$map_color)){
                   signal_to_noise()
                 }
                 else if(!is.null(max_cor()) & input$map_color == "Match ID"){
                   names(max_cor())
                 }
                 else if(!is.null(max_cor()) & input$map_color == "Correlation"){
                   max_cor()
                 }
                 else if(!is.null(signal_to_noise()) & input$map_color == "Signal/Noise"){
                   signal_to_noise()
                 }
                 else if(!is.null(max_cor()) & input$map_color == "Match Name"){
                   libraryR()$metadata$material_class[match(names(max_cor()), libraryR()$metadata$sample_name)]
                 }
                 else{NULL},
                 sn = signif(signal_to_noise(), 2), 
                 cor = if(is.null(max_cor())){max_cor()} else{signif(max_cor(), 2)}, 
                 min_sn = MinSNR(),
                 min_cor = MinCor(),
                 select = data_click$data,
                 source = "heat_plot") %>%
      event_register("plotly_click")
  })
  
  thresholded_particles <- reactive({
    if(input$active_identification){
      particles_logi <- signal_to_noise() > MinSNR() & max_cor() > MinCor()
    }
    else{
      particles_logi <- signal_to_noise() > MinSNR()
    }
    collapse_spec(
      def_features(DataR(), features = particles_logi)
    ) %>%
      filter_spec(., logic = .$metadata$feature_id != "-88")
  })
  
  # Data Download options ----
  output$download_data <- downloadHandler(
    filename = function() {if(input$download_selection == "Test Map") {paste0(input$download_selection, human_ts(), ".zip")} else{paste0(input$download_selection, human_ts(), ".csv")}},
    content = function(file) {
      if(input$download_selection == "Test Data") {fwrite(testdata, file)}
      if(input$download_selection == "Test Map") {file.copy(read_extdata("CA_tiny_map.zip"), file)}
      if(input$download_selection == "Your Spectra") {write_spec(DataR(), file)}
      if(input$download_selection == "Library Spectra") {write_spec(libraryR(), file)}
      if(input$download_selection == "Top Matches") {fwrite(top_correlation(), file)}
      if(input$download_selection == "Thresholded Particles") {write_spec(thresholded_particles(), file = file)}
    })
  
  # Hide functions or objects when the shouldn't exist. 
  observe({
    toggle(id = "heatmap", condition = !is.null(preprocessed$data))
    toggle(id = "placeholder1", condition = is.null(preprocessed$data))
    if(!is.null(preprocessed$data)){
      toggle(id = "heatmap", condition = ncol(preprocessed$data$spectra) > 1)
    }
    if(is.null(event_data("plotly_click", source = "heat_plot"))){
      data_click$data <- 1
    }
    else{
      data_click$data <- event_data("plotly_click", source = "heat_plot")[["pointNumber"]] + 1
    }
  })
  
  #Google translate. 
  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })
  
  # Log events ----
  
  observeEvent(input$bad_spec, {
    if(droptoken & input$share_decision & curl::has_internet() & !is.null(preprocessed$data)){
      file_name <- tempfile(pattern = "issue_report_", fileext = ".rds")
      report_inputs = list(lib_spec = match_selected(),
                           raw_user_spec = preprocessed$data,
                           proc_user_spec = DataR_plot(),
                           user_metadata = user_metadata())
      saveRDS(report_inputs,
              file = file_name)
      put_object(
        file = file_name,
        object = paste0("issues/", gsub(".*\\\\", "", as.character(file_name))),
        bucket = "openspecy"
      )
      toast(
        title = "Report Successful",
        body = "Sorry for the issue, we are on it.", 
        options = list(
          autohide = FALSE,
          class = "bg-pink",
          position = "topRight"
        )
      )
    }
    else{
      toast(
        title = "Not Submitted",
        body = "Submitting issues outside of the web app or without uploading data or without selecting to share data is not currently supported.", 
        options = list(
          autohide = FALSE,
          class = "bg-black",
          position = "topRight"
        )
      )    
    }
  })
  
  user_metadata <- reactive({
    list(
      #user_name = input$fingerprint,
      time = human_ts(),
      session_name = session_id,
      data_id = digest::digest(preprocessed$data, algo = "md5"),
      active = input$active_preprocessing,
      adj_intens = input$intensity_decision, 
      type = input$intensity_corr,
      restric_range = input$range_decision,
      restric_range_min = input$MinRange, 
      restric_range_max = input$MaxRange,
      flatten_range = input$co2_decision,
      flatten_range_min = input$MinFlat, 
      flatten_range_max = input$MaxFlat,
      baseline_decision = input$baseline_decision, 
      subtr_baseline = input$baseline,
      smooth_intens = input$smooth_decision, 
      polynomial = input$smoother, 
      window = input$smoother_window, 
      derivative = input$derivative_order, 
      abs = input$derivative_abs,
      download_selection = input$download_selection,
      id_strategy = input$id_strategy,
      cor_threshold_decision = input$cor_threshold_decision,
      min_cor = input$MinCor,
      threshold_decision = input$threshold_decision, 
      min_sn = input$MinSNR,
      signal_selection = input$signal_selection
    )
  })
  
  observe({
    req(!is.null(preprocessed$data))
    req(input$share_decision)
    if(isTruthy(conf$log)) {
      if(db) {
        database$insert(user_metadata())
      } else {
        loggit("INFO", "trigger",
               user_metadata())
      }
    }
    
  })
  
  #output$event_test <- renderPrint({
  #    list(
  #        conform_spec = input$conform_decision, 
  #        conform_args = list(range = NULL, res = input$conform_res, type = input$conform_selection)
  #    )
  #})
  
}

shinyApp(ui, server)