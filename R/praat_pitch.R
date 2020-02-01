#' extract some pitch variables via Praat from audio (segments)
#'
#' @param sound_file character, path to audio file
#' @param child logical, should pitch settings be set for child (default),
#' or adult
#' @param time_range numeric of length 2, optional start and end time in
#' seconds. If left blank (\code{NULL}) the entire file is processed.
#' @param pathtopraat character, the path to the Praat binary
#' @param unit character, the unit for the frequency measures (\code{"Hertz"},
#' \code{"ERB"}, ...)
#' @param pathtosox character, optional path to sox binary (only required if
#' \code{time_range} is \emph{not} \code{NULL})
#' @details The output reflect measure of F0: mean, 5% percentile and 95%
#' percentile.
#'
#' The pitch settings for child versus adult are the following. Child:
#' floor = 150 Hz, ceiling = 650 Hz. Adult: floor = 75 Hz, ceiling = 400 Hz.
#' Time step for both is 0.01 s.
#'
#' For this function to work, you need Praat, and you need to know where the
#' \code{Praat} binary is located. On a Mac the location is likely to be
#' "/Applications/Praat.app/Contents/MacOS/Praat", and on Windows
#' "C:/Program Files/Praat.exe".
#'
#' @return a data frame with one row
#' @export
#'
#' @examples
#' \dontrun{
#' # requires Praat installed
#' # location to Praat on my machine:
#' ploc <- "/Applications/Praat.app/Contents/MacOS/Praat"
#' # example sound
#' sound_file <- system.file("synthetic_speech.wav", package = "avutils")
#' praat_pitch(sound_file, child = FALSE, pathtopraat = ploc)
#' praat_pitch(sound_file, child = TRUE, pathtopraat = ploc)
#' # analyse only subset requires sox installed
#' sloc <- "/usr/local/bin/sox"
#' praat_pitch(sound_file, time_range = c(0.3, 3.1), child = FALSE,
#'             pathtopraat = ploc, pathtosox = sloc)
#' }

praat_pitch <- function(sound_file,
                        child = TRUE,
                        time_range = NULL,
                        pathtopraat = NULL,
                        unit = "Hertz",
                        pathtosox = NULL) {

  # praat location
  if (is.null(pathtopraat)) {
    pathtopraat <- normalizePath(Sys.which("Praat"), winslash = "/")
  } else {
    pathtopraat <- normalizePath(pathtopraat, winslash = "/")
  }
  if (pathtopraat == "") stop("Praat binary not found")

  # sox location (if time-range is desired)
  if (!is.null(time_range)) {
    if (is.null(pathtosox)) {
      pathtosox <- normalizePath(Sys.which("sox"), winslash = "/")
    } else {
      pathtosox <- normalizePath(pathtosox, winslash = "/")
    }
    if (pathtosox == "") stop("sox binary not found")
  }

  # create temp folder
  process_loc <- normalizePath(tempdir(), winslash = "/")
  process_loc <- file.path(process_loc, "praat_temp")
  if (!dir.exists(process_loc)) dir.create(process_loc)

  # handle file splitting and copy audio to temp folder
  temploc <- file.path(process_loc, basename(sound_file))
  if (is.null(time_range)) {
    file.copy(from = sound_file, to = temploc)
  } else {
    cm <- paste(shQuote(sound_file),
                shQuote(temploc),
                "trim",
                time_range[1],
                paste0("=", time_range[2]))
    system2(command = pathtosox, args = cm, stdout = TRUE, stderr = TRUE)
  }

  # pitch settings according to Alex set differently for child versus adults
  if (child) {
    pitch_setting <- 'To Pitch: 0.01, 150, 650'
  } else {
    pitch_setting <- 'To Pitch: 0.01, 75, 400'
  }

  # write praat script to temp folder
  scripttext <- c('Read from file: "', temploc, '"\n',
                  pitch_setting, '\n',
                  'Get total duration', '\n',
                  'Get mean: 0, 0, "', unit, '"\n',
                  'Get quantile: 0, 0, 0.95, "', unit, '"\n',
                  # 'Get minimum... 0 0 Hertz Parabolic', '\n',
                  'Get quantile: 0, 0, 0.05, "', unit, '"\n'
  )
  scriptloc <- file.path(process_loc, "praatscript.praat")
  writeLines(text = scripttext, con = scriptloc, sep = "")

  # run script
  res <- system2(command = pathtopraat,
          args = paste0("--run ", scriptloc), stdout = TRUE)

  # clean up (delete files)
  file.remove(scriptloc)
  file.remove(temploc)

  # process results
  res <- strsplit(res, split = " ", fixed = TRUE)
  nums <- unlist(lapply(res, function(x)x[1]))
  # replace undefined values with NA
  nums <- gsub(pattern = "--undefined--", replacement = NA, x = nums)
  nums <- round(as.numeric(nums), 3)
  units <- unlist(lapply(res, function(x)x[2]))
  if (is.null(time_range)) time_range <- c(0, nums[1])
  out <- data.frame(xsource = basename(sound_file),
                    from = time_range[1],
                    to = time_range[2],
                    child_pitch = child,
                    praat_dur = nums[1],
                    freq_unit = unit,
                    f_mean = nums[2],
                    f_quant95 = nums[3],
                    f_quant05 = nums[4],
                    stringsAsFactors = FALSE)
  out
}

