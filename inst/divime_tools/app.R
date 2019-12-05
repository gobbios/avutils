library(shiny)
library(shinyFiles)
library(avutils)


set_binaries()

ui <- fluidPage(
    tags$head(tags$style(HTML('* {font-family: Baskerville; Times;}'))),
    titlePanel("Application Title"),

    navlistPanel(widths = c(3, 9),
        "prelims",
        tabPanel("read me",
                 fluidPage(
                     includeMarkdown("readme.Rmd")
            )
        ),
        tabPanel("setup", icon = icon("cogs"),
                 fluidPage(
                     includeMarkdown("instructions-setup.Rmd"),
                     hr(),
                     fluidRow(
                         column(3,
                                shinyFilesButton("ffmpeg_path", label = "select ffmpeg file", title = "please select", multiple = FALSE),
                                textOutput("ffmpeg_path_loc"),
                                actionButton("ffmpeg_refresh", label = "apply ffmpeg path", icon = icon("sync-alt")),
                                # textOutput("ffmpeg_available")
                                htmlOutput("ffmpeg_available")
                         ),
                         column(3,
                                shinyFilesButton("sox_path", label = "select sox file", title = "please select", multiple = FALSE),
                                textOutput("sox_path_loc"),
                                actionButton("sox_refresh", label = "apply sox path", icon = icon("sync-alt")),
                                # textOutput("sox_available")
                                htmlOutput("sox_available")
                         ),
                         column(3,
                                shinyFilesButton("git_path", label = "select git file", title = "please select", multiple = FALSE),
                                textOutput("git_path_loc"),
                                actionButton("git_refresh", label = "apply git path", icon = icon("sync-alt")),
                                # textOutput("git_available")
                                htmlOutput("git_available")
                         ),
                         column(3,
                                shinyFilesButton("vagrant_path", label = "select vagrant file", title = "please select", multiple = FALSE),
                                textOutput("vagrant_path_loc"),
                                actionButton("vagrant_refresh", label = "apply vagrant path", icon = icon("sync-alt")),
                                # textOutput("vagrant_available")
                                htmlOutput("vagrant_available")
                         )
                     ),
                     hr(),
                     fluidRow(
                         column(4,
                                shinyDirButton("divime_path", label = "select path to DiViMe", title = "please select"),
                                textOutput("divime_path_loc"),
                                # actionButton("divime_verify", label = "verify", icon = icon("user-check")),
                                htmlOutput("divime_available")
                         ),
                         column(4,
                                shinyDirButton("data_path_button", label = "select path to audio files", title = "please select"),
                                htmlOutput("data_path_info"),
                                htmlOutput("data_path_audio")
                                ),
                         column(4,
                                actionButton("check_python_button", label = "check for python/reticulate"),
                                htmlOutput("check_python_path"),
                                htmlOutput("check_python_result")
                         )


                     ),
                     br(),
                     br(),
                     hr()
                 )


        ),
        tabPanel("tests",
                 fluidPage(
                     fluidRow(
                         column(6, actionButton("divime_state_button", label = "check VM state")),
                         column(6, textOutput("divime_state_result"))
                     ),
                     br(),
                     hr(),
                     fluidRow(
                         column(4, actionButton("divime_start_button", "start VM")),
                         column(4, actionButton("divime_suspend_button", "sleep VM")),
                         column(4, actionButton("divime_halt_button", "shut down VM"))
                     ),
                     hr(),
                     fluidRow(
                         column(12, actionButton("run_test", "run the test script"))
                     ),
                     verbatimTextOutput("test_results")
                 )
        ),
        "data summaries",
        tabPanel("folder content",
                 fluidPage(
                     column(4, actionButton("data_files_refresh", "refresh")),
                     br(),
                     br(),
                     column(12, verbatimTextOutput("data_files"))
                     )
                 ),
        tabPanel("audio info",
                 fluidPage(
                     column(4, actionButton("data_files_audio_info", "get audio info (requires sox)")),
                     br(),
                     br(),
                     # column(8, p("requires sox")),
                     column(12, tableOutput("audio_info"))
                 )
        ),
        tabPanel("results summary",
                 fluidPage(
                     column(12, actionButton("data_files_overview_refresh", "refresh")),
                     br(),
                     br(),
                     h4("speech detection files"),
                     column(12, tableOutput("data_files_sad")),
                     h4("talker diarization files"),
                     column(12, tableOutput("data_files_diartk")),
                     h4("talker type files"),
                     column(12, tableOutput("data_files_yuni")),
                     h4("vocal maturity files"),
                     column(12, tableOutput("data_files_vcm")),
                     h4("word count files"),
                     column(12, tableOutput("data_files_wce"))

                 )
                 ),


        "DiViMe tools",
        tabPanel("speech detection",
                 fluidPage(
                     fluidRow(
                         column(12, checkboxGroupInput("sad_choice", "select modules", choices = c("noisemes" = "nois", "opensmile" = "open", "tocombo" = "toco", "noisemes full" = "noisfull"), inline = TRUE)),
                         column(12, checkboxInput("sad_overwrite", "overwrite exisiting results", value = FALSE))
                     ),
                     hr(),
                     fluidRow(column(3, actionButton("run_sad", "run speech detection"))),
                     hr(),
                     fluidRow(column(12, tableOutput("results_sad")))
                 )),
        tabPanel("talker diarization",
                 fluidPage(
                     fluidRow(
                         column(12, checkboxGroupInput("diartk_choice", "select modules", choices = c("noisemes" = "nois", "opensmile" = "open", "tocombo" = "toco"), inline = TRUE)),
                         column(12, checkboxInput("diartk_overwrite", "overwrite exisiting results", value = FALSE))
                     ),
                     hr(),
                     fluidRow(column(3, actionButton("run_diartk", "run talker diarization"))),
                     hr(),
                     fluidRow(column(12, tableOutput("results_diartk")))
                 )),
        tabPanel("talker type",
                 fluidPage(
                     fluidRow(
                         column(12, checkboxInput("yuni_choice", "use Marvinator", value = FALSE)),
                         column(12, checkboxInput("yuni_overwrite", "overwrite exisiting results", value = FALSE))
                     ),
                     hr(),
                     fluidRow(column(3, actionButton("run_yuni", "run talker type"))),
                     hr(),
                     fluidRow(column(12, tableOutput("results_yuni")))
                 )),
        tabPanel("vocal maturity",
                 fluidPage(
                     fluidRow(
                         column(12, checkboxInput("vcm_choice", "use Marvinator", value = FALSE)),
                         column(12, checkboxInput("vcm_overwrite", "overwrite exisiting results", value = FALSE))
                     ),
                     hr(),
                     fluidRow(column(3, actionButton("run_vcm", "run vocal maturity"))),
                     hr(),
                     fluidRow(column(12, tableOutput("results_vcm")))
                 )),
        tabPanel("word count",
                 fluidPage(
                     fluidRow(
                         column(12, checkboxGroupInput(inputId = "wce_choice",
                                                       label = "select modules",
                                                       choices = c("noisemes" = "nois",
                                                                   "opensmile" = "open",
                                                                   "tocombo" = "toco",
                                                                   "marvinator" = "marv"),
                                                       inline = TRUE)),
                         column(12, checkboxInput("wce_appendsilence", "append 1 sec silence to audio (requires sox)", value = FALSE)),
                         column(12, checkboxInput("wce_overwrite", "overwrite exisiting results", value = FALSE))
                     ),
                     hr(),
                     fluidRow(column(3, actionButton("run_wce", "run word count"))),
                     hr(),
                     fluidRow(column(12, tableOutput("results_wce")))
                 )),
        "-----",
        tabPanel("Component 5")
    )
)


server <- function(input, output, session) {
    # app setting and reactive values ----------------------------
    ROOTS <- c(volumes = getVolumes()(), home = fs::path_home())
    binaries <- reactiveValues(sox_path = ifelse(Sys.which("sox") == "", "~/", Sys.which("sox")),
                               sox_working = test_binaries(printmessages = FALSE)["sox"],
                               ffmpeg_path = ifelse(Sys.which("ffmpeg") == "", "~/", Sys.which("ffmpeg")),
                               ffmpeg_working = test_binaries(printmessages = FALSE)["ffmpeg"],
                               git_path = ifelse(Sys.which("git") == "", "no path available", Sys.which("git")),
                               git_working = test_binaries(printmessages = FALSE)["git"],
                               vagrant_path = ifelse(Sys.which("vagrant") == "", "no path available", Sys.which("vagrant")),
                               vagrant_working = test_binaries(printmessages = FALSE)["vagrant"],
                               python_working = file.exists(Sys.which("python")),
                               python_path = ifelse(Sys.which("python") == "", "no path available", Sys.which("python")),
                               divime_loc = "",
                               divime_check1 = FALSE,
                               data_loc = "",
                               data_has_audio = FALSE
    )





    # test VM and VM state -----------------------------------
    tests <- reactiveValues(VM_state = "")

    observeEvent(input$divime_state_button, {
        withProgress(message = 'busy with checking', value = 0, {
            tests$VM_state <- avutils::divime_vagrant_state(divime_loc = binaries$divime_loc, silent = TRUE)
            output$divime_state_result <- renderText(tests$VM_state)
        })
    })
    observeEvent(input$divime_start_button, {
        withProgress(message = 'starting up', value = 0, {
            avutils::divime_vagrant_state(divime_loc = binaries$divime_loc, what = "start", silent = TRUE)
        })
    })
    observeEvent(input$divime_suspend_button, {
        withProgress(message = 'putting to sleep', value = 0, {
            avutils::divime_vagrant_state(divime_loc = binaries$divime_loc, what = "suspend", silent = TRUE)
        })
    })
    observeEvent(input$divime_halt_button, {
        withProgress(message = 'shutting down', value = 0, {
            avutils::divime_vagrant_state(divime_loc = binaries$divime_loc, what = "halt", silent = TRUE)
        })
    })

    # run test script --------------------------------------------
    observeEvent(input$run_test, {
        withProgress(message = 'running the test', value = 0, {
            WD <- getwd()
            setwd(binaries$divime_loc)
            res <- system2(command = binaries$vagrant_path, args = "ssh -c 'launcher/test.sh'", stdout = TRUE)
            setwd(WD)
            output$test_results <- renderText(paste(res, collapse = "\n"))
        })
    })

    # setup DIVIME location ------------------------------
    shinyDirChoose(input = input, id = "divime_path", session = session, roots = ROOTS, defaultPath = "")

    observeEvent(input$divime_path, {
        inFile <- parseDirPath(roots = ROOTS, input$divime_path)
        binaries$divime_loc <- as.character(inFile)
        x <- list.files(binaries$divime_loc)
        if ("launcher" %in% x && "utils" %in% x && "LICENSE" %in% x) {
            binaries$divime_check1 <- TRUE
        }
    })
    observe(output$divime_path_loc <- renderText(binaries$divime_loc))

    observe({
        check <- binaries$divime_check1
        if (check) {
            output$divime_available <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$divime_available <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })

    # setup data folder ------------------------------------------
    shinyDirChoose(input = input, id = "data_path_button", session = session, roots = ROOTS, defaultPath = "")
    observeEvent(input$data_path_button, {
        inFile <- parseDirPath(roots = ROOTS, input$data_path_button)
        binaries$data_loc <- as.character(inFile)
        output$data_path_info <- renderUI(HTML(paste("data are here: <font color=red family='Times New Roman'>", binaries$data_loc, "</font>")))
        allfiles <- list.files(binaries$data_loc)
        binaries$data_has_audio <- sum(grepl(pattern = ".wav", x = allfiles, ignore.case = TRUE)) > 0
    })
    observe({
        check <- binaries$data_has_audio
        if (check) {
            output$data_path_audio <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$data_path_audio <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })

    # check python -------------------------------

    observeEvent(input$check_python_button, {
        binaries$python_working <- reticulate::py_available(TRUE)
    })

    observe({
        check <- binaries$python_working
        if (check) {
            output$check_python_result <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$check_python_result <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })
    output$check_python_path <- renderText(Sys.which("python"))


    # folder content ----------------------------------------------
    observe({
        res <- list.files(binaries$data_loc)
        output$data_files <- renderText(paste(res, collapse = "\n"))
    })
    observeEvent(input$data_files_refresh, {
        res <- list.files(binaries$data_loc)
        output$data_files <- renderText(paste(res, collapse = "\n"))
    })

    # audio info for data files --------------------------------
    observeEvent(input$data_files_audio_info, {
        res <- list.files(binaries$data_loc, full.names = TRUE)
        res <- res[grep(pattern = ".wav", res, ignore.case = TRUE)]
        res <- audio_info(res)
        res$filename <- basename(as.character(res$filename))
        res$channels <- as.character(res$channels)
        res$filesize <- NULL
        res$samples <- as.character(res$samples)
        res$resol <- as.character(res$resol)
        res$samplerate <- as.character(res$samplerate)
        output$audio_info <- renderTable(res)
    })

    # folder content - tables ---------------------------------
    observeEvent(input$data_files_overview_refresh, {
        temp <- binaries$data_loc
        binaries$data_loc <- "~/"
        binaries$data_loc <- temp
    })

    observe({
        loc <- as.character(binaries$data_loc)
        res <- NULL
        if (!is.null(loc)) {
            X <- list.files(loc)
            audio <- X[grep(pattern = ".wav", X, ignore.case = TRUE)]
            if (length(audio) > 0) {
                res <- data.frame(audio = audio,
                                  noisemes = NA, opensmile = NA, tocombo = NA,
                                  dia_noisemes = NA, dia_opensmile = NA, dia_tocombo = NA,
                                  vcm = NA, yuni_old = NA, yuni_new = NA, noisemes_full = NA,
                                  wce_noisemes = NA, wce_opensmile = NA, wce_tocombo = NA, wce_marvinator = NA)
                # prefixes for rttm
                prefixes <- c("noisemesSad_", "opensmileSad_", "tocomboSad_", "diartk_noisemesSad_", "diartk_opensmileSad_", "diartk_tocomboSad_", "vcm_", "yunitator_old_", "yunitator_english_", "noisemesFull_", "WCE_noisemesSad_", "WCE_opensmileSad_", "WCE_tocomboSad_", "WCE_yunitator_english_")
                # file roots (sans extension)
                rootnames <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(audio))

                for (i in 1:nrow(res)) {
                    for (k in 1:length(prefixes)) {
                        if (paste0(prefixes[k], rootnames[i], ".rttm") %in% X) {
                            res[i, k + 1] <- TRUE
                        } else {
                            res[i, k + 1] <- FALSE
                        }
                    }
                }
            }
        }
        output$data_files_sad <- renderTable(res[, c("audio", "noisemes", "opensmile", "tocombo")])
        output$data_files_diartk <- renderTable(res[, c("audio", "dia_noisemes", "dia_opensmile", "dia_tocombo")])
        output$data_files_yuni <- renderTable(res[, c("audio", "yuni_old", "yuni_new")])
        output$data_files_vcm <- renderTable(res[, c("audio", "vcm")])
        output$data_files_wce <- renderTable(res[, c("audio", "wce_noisemes", "wce_opensmile", "wce_tocombo", "wce_marvinator")])
    })


    # setup GIT --------------------------------------
    observe({
        check <- binaries$git_working
        if (check) {
            output$git_available <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$git_available <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })
    output$git_path_loc <- renderText(binaries$git_path)

    # setup VAGRANT ----------------------------
    observe({
        check <- binaries$vagrant_working
        if (check) {
            output$vagrant_available <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$vagrant_available <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })
    output$vagrant_path_loc <- renderText(binaries$vagrant_path)


    # setup FFMPEG -----------------------
    shinyFileChoose(input = input, id = "ffmpeg_path", session = session, roots = c(home = fs::path_home()), defaultPath = "/Documents/utilities")

    observeEvent(input$ffmpeg_path, {
        inFile <- parseFilePaths(roots = c(home = fs::path_home()), input$ffmpeg_path)
        binaries$ffmpeg_path <- as.character(inFile$datapath)
    })

    observe({
        check <- binaries$ffmpeg_working
        if (check) {
            output$ffmpeg_available <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$ffmpeg_available <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })

    observe(output$ffmpeg_path_loc <- renderText({binaries$ffmpeg_path}))

    observeEvent(input$ffmpeg_refresh, {
        set_binaries(pathtosox = NULL, pathtoffmpeg = binaries$ffmpeg_path, printmessages = FALSE)
        binaries$ffmpeg_working <- test_binaries(printmessages = FALSE)["ffmpeg"]
    })

    # setup SOX -----------------------------
    shinyFileChoose(input = input, id = "sox_path", session = session, roots = c(home = fs::path_home()), defaultPath = "/Documents/utilities")

    observeEvent(input$sox_path, {
        inFile <- parseFilePaths(roots = c(home = fs::path_home()), input$sox_path)
        binaries$sox_path <- as.character(inFile$datapath)
    })
    observe({
        check <- binaries$sox_working
        if (check) {
            output$sox_available <- renderUI(HTML("<font color=green size=200%;> &#10004; </font>"))
        } else {
            output$sox_available <- renderUI(HTML("<font color=red size=200%;> &#10008; </font>"))
        }
    })

    observe(output$sox_path_loc <- renderText({binaries$sox_path}))

    observeEvent(input$sox_refresh, {
        set_binaries(pathtosox = binaries$sox_path, pathtoffmpeg = NULL, printmessages = FALSE)
        binaries$sox_working <- test_binaries(printmessages = FALSE)["sox"]
    })



    # speech detection ------------------

    observeEvent(input$run_sad, {
        allres <- c()
        if ("nois" %in% input$sad_choice) {
            withProgress(message = 'running noisemes', value = 0, {
                res <- divime_sad(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  module = "noisemes",
                                  messages = TRUE,
                                  overwrite = input$sad_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("open" %in% input$sad_choice) {
            withProgress(message = 'running opensmile', value = 0, {
                res <- divime_sad(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  module = "opensmile",
                                  messages = TRUE,
                                  overwrite = input$sad_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("toco" %in% input$sad_choice) {
            withProgress(message = 'running tocombo', value = 0, {
                res <- divime_sad(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  module = "tocombo",
                                  messages = TRUE,
                                  overwrite = input$sad_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("noisfull" %in% input$sad_choice) {
            withProgress(message = 'running complete noisemes', value = 0, {
                res <- divime_fullnoisemes(audio_loc = binaries$data_loc,
                                           divime_loc = binaries$divime_loc,
                                           messages = TRUE,
                                           overwrite = input$sad_overwrite)
            })
            allres <- rbind(allres, res)
        }
        allres <- allres[, -c(which(colnames(allres) %in% c("audiocopy", "audioremove", "resultscopy", "resultsremove", "yuniproblem")))]
        colnames(allres)[colnames(allres) == "ptime"] <- "process time"
        colnames(allres)[colnames(allres) == "outlines"] <- "lines in output"
        output$results_sad <- renderTable(allres)
    })

    # talker diarization ------------------

    observeEvent(input$run_diartk, {
        allres <- c()
        if ("nois" %in% input$diartk_choice) {
            withProgress(message = 'running talker diarization (noisemes)', value = 0, {
                res <- divime_diarization(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  speech_annos = "noisemes",
                                  messages = TRUE,
                                  overwrite = input$diartk_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("open" %in% input$diartk_choice) {
            withProgress(message = 'running talker diarization (opensmile)', value = 0, {
                res <- divime_diarization(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  speech_annos = "opensmile",
                                  messages = TRUE,
                                  overwrite = input$diartk_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("toco" %in% input$diartk_choice) {
            withProgress(message = 'running talker diarization (tocombo)', value = 0, {
                res <- divime_diarization(audio_loc = binaries$data_loc,
                                  divime_loc = binaries$divime_loc,
                                  speech_annos = "tocombo",
                                  messages = TRUE,
                                  overwrite = input$diartk_overwrite)
            })
            allres <- rbind(allres, res)
        }

        allres <- allres[, -c(which(colnames(allres) %in% c("sadcopy", "sadremove", "audiocopy", "audioremove", "resultscopy", "resultsremove")))]
        colnames(allres)[colnames(allres) == "ptime"] <- "process time"
        colnames(allres)[colnames(allres) == "outlines"] <- "lines in output"
        colnames(allres)[colnames(allres) == "sadfile"] <- "SAD file"
        output$results_diartk <- renderTable(allres)
    })

    # talker type ------------------

    observeEvent(input$run_yuni, {

        withProgress(message = 'running talker type', value = 0, {
            res <- divime_talkertype(audio_loc = binaries$data_loc,
                                     divime_loc = binaries$divime_loc,
                                     marvinator = input$yuni_choice,
                                     messages = TRUE,
                                     overwrite = input$yuni_overwrite)
        })
        res <- res[, -c(which(colnames(res) %in% c("audiocopy", "audioremove", "resultscopy", "resultsremove", "yuniproblem")))]
        colnames(res)[colnames(res) == "ptime"] <- "process time"
        colnames(res)[colnames(res) == "outlines"] <- "lines in output"

        output$results_yuni <- renderTable(res)
    })

    # vocal maturity ------------------

    observeEvent(input$run_vcm, {

        withProgress(message = 'running vocal maturity', value = 0, {
            res <- divime_classify_vox(audio_loc = binaries$data_loc,
                                     divime_loc = binaries$divime_loc,
                                     marvinator = input$vcm_choice,
                                     messages = TRUE,
                                     overwrite = input$vcm_overwrite)
        })
        res <- res[, -c(which(colnames(res) %in% c("audiocopy", "audioremove", "resultscopy", "resultsremove", "yunifile", "yunicopy", "yuniremove")))]
        colnames(res)[colnames(res) == "ptime"] <- "process time"
        colnames(res)[colnames(res) == "outlines"] <- "lines in output"

        output$results_vcm <- renderTable(res)
    })

    # word count ------------------

    observeEvent(input$run_wce, {
        allres <- c()
        if ("nois" %in% input$wce_choice) {
            withProgress(message = 'running word count (noisemes)', value = 0, {
                res <- divime_wordcount(audio_loc = binaries$data_loc,
                                          divime_loc = binaries$divime_loc,
                                          speech_annos = "noisemes",
                                          messages = TRUE,
                                          overwrite = input$wce_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("open" %in% input$wce_choice) {
            withProgress(message = 'running word count (opensmile)', value = 0, {
                res <- divime_wordcount(audio_loc = binaries$data_loc,
                                          divime_loc = binaries$divime_loc,
                                          speech_annos = "opensmile",
                                          messages = TRUE,
                                          overwrite = input$wce_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("toco" %in% input$wce_choice) {
            withProgress(message = 'running word count (tocombo)', value = 0, {
                res <- divime_wordcount(audio_loc = binaries$data_loc,
                                          divime_loc = binaries$divime_loc,
                                          speech_annos = "tocombo",
                                          messages = TRUE,
                                          overwrite = input$wce_overwrite)
            })
            allres <- rbind(allres, res)
        }
        if ("marv" %in% input$wce_choice) {
            appendsilence <- as.numeric(input$wce_appendsilence)
            if (appendsilence == 0) appendsilence <- NULL
            withProgress(message = 'running word count (marvinator)', value = 0, {
                res <- divime_wordcount(audio_loc = binaries$data_loc,
                                          divime_loc = binaries$divime_loc,
                                          speech_annos = "yunitator_english",
                                          messages = TRUE,
                                          overwrite = input$wce_overwrite,
                                          appendsilence = appendsilence)
            })
            allres <- rbind(allres, res)
        }

        allres <- allres[, -c(which(colnames(allres) %in% c("sadcopy", "sadremove", "audiocopy", "audioremove", "resultscopy", "resultsremove")))]
        colnames(allres)[colnames(allres) == "ptime"] <- "process time"
        colnames(allres)[colnames(allres) == "outlines"] <- "lines in output"
        colnames(allres)[colnames(allres) == "sadfile"] <- "SAD file"
        colnames(allres)[colnames(allres) == "empty_sad"] <- "SAD file empty"

        output$results_wce <- renderTable(allres)
    })
}
#
# Run the application
shinyApp(ui = ui, server = server)
