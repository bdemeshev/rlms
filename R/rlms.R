#' rlms
#'
#' @name rlms
#' @docType package
#' @author Boris Demeshev
#' @import foreign
NULL

#' Read rlms data (deprecated function)
#'
#' Read rlms data and all meta information (deprecated function)
#'
#' Read rlms data and all the meta information. Destroy useless attributes.
#' This function is deprecated. Please use rlms_read instead of.
#'
#' @param file the filename
#' @param suppress logical, if true the default message is suppressed
#' @param nine2na automatically convert 99999999 to NA for numeric variables
#' @export
#' @return dataframe
#' @examples
#' # read.rlms("r21i_os24a.sav")
read.rlms <- function(file, suppress = FALSE, nine2na = TRUE) {
  message("This function is deprecated. Please use rlms_read instead of.")
  df <- rlms_read(file = file,
                  suppress = suppress,
                  nine2na = nine2na,
                  yesno = FALSE,
                  apostrophe = FALSE)
  return(df)
}


#' Remove apostrophes
#'
#' Remove apostrophes
#'
#' Remove apostrophes
#'
#' @param x character vector
#' @export
#' @return character vector with apostrophes removed
rlms_remove_apostrophe <- function(x) {
  x_new <- stringr::str_replace_all(x, "\\u2018", "")
  x_new <- stringr::str_replace_all(x_new, "\\u2019.\\u2019", "")
  x_new <- stringr::str_replace_all(x_new, "\\u2019", "")
  return(x_new)
}

#' Show variable labels
#'
#' Show variable labels
#'
#' Show variable labels
#'
#' @param df data.frame read from rlms file
#' @export
#' @return data frame with variable labels
rlms_show_variable_labels <- function(df) {
  return(attr(df, "var_meta"))
}

#' Show value labels
#'
#' Show value labels
#'
#' Show value labels
#'
#' @param df data.frame read from rlms file
#' @export
#' @return data frame with value labels
rlms_show_value_labels <- function(df) {
  return(attr(df, "value_meta"))
}


#' Standartize yes/no
#'
#' Standartize yes/no
#'
#' Standartize yes/no
#'
#' @param x character vector
#' @export
#' @return character vector with standartized yes/no
rlms_yesno_standartize <- function(x) {
  x_simple <- tolower(rlms_remove_apostrophe(x))

  x_new <- x

  yes <- "\u0434\u0430" # this is yes in russian
  no <- "\u043d\u0435\u0442" # this is no in russian
  # explicit codes are used to avoid warning in check()

  x_new[x_simple == yes] <- yes
  x_new[x_simple == no] <- no
  return(x_new)
}


#' Read rlms data
#'
#' Read rlms data and all meta information
#'
#' Read rlms data and all the meta information. Destroy useless attributes.
#'
#' @param file the filename
#' @param haven use haven package: "no" - (default) do not use "haven" package,
#' "labelled" - return labelled variables,
#' "factor" - return factor or numeric variables,
#' "numeric" - return numeric variables.
#' @param yesno convert yes/no answers to lowercase yes/no without apostrophes
#' @param apostrophe trim apostrophes, TRUE by default
#' @param remove_empty remove empty labels, TRUE by default
#' @param suppress logical, if true the default message is suppressed
#' @param nine2na automatically convert 99999999 to NA for numeric variables
#' @export
#' @return dataframe
#' @examples
#' # rlms_read("r21i_os24a.sav")
rlms_read <- function(file,
                      suppress = TRUE,
                      nine2na = TRUE,
                      yesno = TRUE,
                      apostrophe = TRUE,
                      remove_empty = TRUE,
                      haven = c("no", "labelled", "factor", "numeric")) {

  haven <- match.arg(haven) # check no/labelled/factor

  if (!haven == "") {
    df <- haven::read_spss(file)
    # check the rest of the file with this option!
    varlabel <- unlist(lapply(df, function(x) attr(x, "label")))
    var_meta <- data.frame(var = names(df),
                           varlabel = varlabel,
                           stringsAsFactors = FALSE)
    value_meta <- NULL
    for (var in names(df)) {
      value <- attr(df[[var]], "labels")
      if (length(value) > 0) {
        # NULL and numeric(0) are ignored
        
        
        
        vallabel <- names(value)
        
        
        temp <- data.frame(value = value, vallabel = vallabel,
                           var = var, stringsAsFactors = FALSE)
        
        
        if ((remove_empty) & (!"" %in% df[[var]])) {
          # remove "" in value labels
          # we play on the safe side and check that variable has no empty values
          temp <- temp[!temp$vallabel == "", ]
        }

        value_meta <- rbind(value_meta, temp)
      }
    }

  }

  if (haven == "factor") {

    message("The option haven = 'factor' is experimental and subject to change.")

    for (var in names(df)) {
      if (is_labelled(df[[var]])) { 
        # Rule 1: All labelled means factor
        if (all_labelled(df[[var]])) {
          df[[var]] <- haven::as_factor(df[[var]])
        }
        # Rule 2: All but one in the middle labelled means factor
        # Not implemented yet.
      } 
    }      
  }
  
  if (haven == "numeric") {
    for (var in names(df)) {
      if (is_labelled(df[[var]])) {
        df[[var]] <- as.numeric(df[[var]])  
      }
    }
  }

  if (haven == "no") {
    df <- foreign::read.spss(file, to.data.frame = TRUE, reencode = TRUE)
    attr(df, "codepage") <- NULL

    # get variable labels
    varlabel <- attr(df, "variable.labels")
    names(varlabel) <- NULL
    var_meta <- data.frame(var = names(df), varlabel = varlabel,
                           stringsAsFactors = FALSE)
    attr(df, "variable.labels") <- NULL

    # get value labels
    value_meta <- NULL

    for (i in 1:ncol(df)) {
      value <- attr(df[, i], "value.labels")
      if (length(value) > 0) {
        # NULL and numeric(0) are ignored
        vallabel <- names(value)
        attr(value, "names") <- NULL
        temp <- data.frame(value = value,
                           vallabel = vallabel,
                           var = names(df)[i],
                           stringsAsFactors = FALSE)
        value_meta <- rbind(value_meta, temp)
        attr(df[, i], "value.labels") <- NULL
      }
    }
  }


  for (var in names(df)) {
    var_class <- class(df[[var]])
    if ((nine2na) & (var_class == "numeric")) {
      # replace 99999990+ for numeric variables
      df[[var]] <- ifelse(df[[var]] > 99999990, NA, df[[var]])
    }
    if ((apostrophe) & (var_class == "factor")) {
      # trim apostrophes \\u2018 and \\u2019
      levels(df[[var]]) <- rlms_remove_apostrophe(levels(df[[var]]))
    }
    if ((yesno) & (var_class == "factor")) {
      # convert yes/no to lowercase without apostrophes
      levels(df[[var]]) <- rlms_yesno_standartize(levels(df[[var]]))
    }

    # remove "" in levels
    if (var_class == "factor") {
      if (sum(df[[var]] == "", na.rm = TRUE) == 0) {
        levels <- levels(df[[var]])
        levels_new <- setdiff(levels, "")
        df[[var]] <- factor(df[[var]], levels = levels_new)
      }
    }
  }


  # add wave-level-sample:
  fileinfo <- rlms_fileinfo(file)

  df$wave <- fileinfo$wave
  df$level <- fileinfo$level
  df$sample <- fileinfo$sample

  if (yesno) {
    value_meta <-
      dplyr::mutate(value_meta, vallabel = rlms_yesno_standartize(vallabel))
  }
  if (apostrophe) {
    value_meta <-
      dplyr::mutate(value_meta, vallabel = rlms_remove_apostrophe(vallabel))
  }

  attr(df, "var_meta") <- var_meta
  attr(df, "value_meta") <- value_meta

  if (!suppress) {
    message("Variable labels: rlms_show_variable_labels(df). ")
    message("Value labels: rlms_show_value_labels(df). ")
    message("You may extract meta information now.")
    message("Later some functions may destroy meta information. ")
    message("This message may be turned off with option: suppress = TRUE. ")
  }


  # to avoid long waiting time for occasional "df + enter":
  df <- dplyr::as.tbl(df)

  return(df)
}




#' Convert string with a number in Russian tradition in numeric
#'
#' Convert string with a number in Russian tradition in numeric
#'
#' Russian standards prescribes to use comma as a decimal separator.
#' This function removes spaces and converts string to number.
#'
#' @param x the string with the number
#' @return numeric the number converted from the string
#' @export
#' @examples
#' rus2num("34 345,34")
rus2num <- function(x) {
  x <- gsub(",", ".", x)
  x <- gsub(" ", "", x)
  return(as.numeric(x))
}


#' Convert excel numeric date encoding to date
#'
#' Convert excel numeric date encoding to date
#'
#' While reading excel files dates are sometimes replaced by their numeric codes.
#' This function recovers original dates from these codes.
#'
#' @param x the vector of numeric date codes
#' @return the date
#' @export
#' @examples
#' excel2date(12345)
excel2date <- function(x) {
  ans <- as.Date(
    as.POSIXct( (x - 25569) * 86400,
               tz = "GMT",
               origin = "1970-01-01"))
  return(ans)
}





#' Detect wave, sample and level from filename of rlms file
#'
#' Detect wave, sample and level from filename of rlms file
#'
#' RLMS filenames contain info about the number of wave, sample (representative or all) and
#' level (household, individual and a special case of reproductive)
#'
#' @param flist the string vector of filenames with or without path
#' @return data.frame containing short filename, wave, level and sample columns
#' @export
#' @examples
#' rlms_fileinfo("r06hall23.sav")
#' # specify rlms folder first
#' # flist_long <- list.files("~/Documents/rlms_data/", recursive = TRUE, pattern = "*.sav")
#' # rlms_fileinfo(flist_long)
rlms_fileinfo <- function(flist) {
  flist_short <- basename(flist) # transform filenames with path to short filenames

  flist_sep <- stringr::str_match(flist_short,
                  "r([0-9]{2})([ih])(all|_os)[a-zA-Z0-9]*.sav")
  df <- data.frame(cbind(flist_short, matrix(flist_sep[, -1], ncol = 3)),
                   stringsAsFactors = FALSE)
  # here we need to specify ncol=3 to correctly work with scalar flist

  names(df) <- c("file_short", "wave", "level", "sample")

  df$wave <- as.numeric(df$wave)

  recode <- c(individual = "i", household = "h", reproductive = "r")
  df$level <- factor(df$level, levels = recode, labels = names(recode))

  recode <- c(all = "all", representative = "_os")
  df$sample <- factor(df$sample, levels = recode, labels = names(recode))

  women_rep <- df$file_short == "r19PHv2.sav"
  df$wave[women_rep] <- 19
  df$level[women_rep] <- "reproductive"
  df$sample[women_rep] <- NA

  return(df)
}

#' Display some RLMS related hints
#'
#' Display some RLMS related hints
#'
#' Display some RLMS related hints
#'
#' @return nothing, just prints some messages
#' @export
#' @examples
#' rlms_hints()
rlms_hints <- function() {
  message("macos specific:")
  message("To extract zip archive with correct cyrillic folder names one may use 'The unarchiver' (free), see http://unarchiver.c3.cx/")
}



#' Convert all RLMS files from .sav to .Rds
#'
#' Convert all RLMS files from .sav to .Rds
#'
#' Convert all RLMS files from .sav to .Rds
#'
#' @param rlms_folder path to rlms data
#' @param flatten a logical value indicating whether to flatten folder structure, default is TRUE
#' @return nothing
#' @export
#' @examples
#' # rlms_sav2rds("~/Documents/rlms_data/")
rlms_sav2rds <- function(rlms_folder, flatten = TRUE) {
  # remove trailing "/" if present
  if (stringr::str_sub(rlms_folder, start = -1) == "/") {
    rlms_folder <- stringr::str_sub(rlms_folder, end = -2)
  }

  flist_in <- list.files(path = rlms_folder,
                         recursive = TRUE, pattern = "*.sav", full.names = TRUE)

  flist_info <- rlms_fileinfo(flist_in)

  flist_out <- stringr::str_replace(flist_in, ".sav", ".Rds")
  if (flatten) flist_out <- paste0(rlms_folder, "/", basename(flist_out)) # remove path if we flatten folder structure


  for (j in 1:length(flist_in)) {
    message("Processing ", flist_info$file_short[j],
            ", wave: ", flist_info$wave[j],
            ", level: ", flist_info$level[j],
            ", sample: ", flist_info$sample[j],
            ", ", (100 * j) %/% length(flist_in), "% done")
    temp <- read.rlms(flist_in[j], suppress = TRUE)
    saveRDS(temp, file = flist_out[j])
  }
}


#' Load RLMS data of specified wave/level/sample
#'
#' Load RLMS data of specified wave/level/sample
#'
#' Load RLMS data of specified wave/level/sample. This function automatically
#' determines the file name. The function tries to load .Rds file.
#' If .Rds file is missing then .sav file is loaded.
#'
#' @param wave the number of wave
#' @param rlms_folder path to rlms data
#' @param level the level (individual/household/reproductive)
#' @param sample the sample (all/representative)
#' @return data.frame with RLMS data
#' @export
#' @examples
#' # rlms_load("~/Documents/rlms_data/", wave = 20, level = "individual", sample = "rep" )
rlms_load <- function(rlms_folder, wave,
                         level=c("individual", "household", "reproductive"),
                         sample=c("all", "representative")) {
  level <- match.arg(level)
  sample <- match.arg(sample)

  if ( (wave == 19) & (level == "reproductive") ) {
       filename <- "r19PHv2"
  }  else {
      filename <- "r"

      # add wave
      if (nchar(wave) == 1) {
        filename <- paste0(filename,"0")
      }
      filename <- paste0(filename, wave)

      # add level
      if (level == "individual") {
        filename <- paste0(filename, "i")
      }
      if (level == "household") {
        filename <- paste0(filename, "h")
      }

      # add sample
      if (sample == "all") {
        filename <- paste0(filename, "all")
      }
      if (sample == "representative") {
        filename <- paste0(filename, "_os")
      }
  }

  # remove trailing "/" if present
  if (stringr::str_sub(rlms_folder, start = -1) == "/") {
    rlms_folder <- stringr::str_sub(rlms_folder, end = -2)
  }


  flist_sav <- list.files(path = rlms_folder, recursive = TRUE, pattern = "*.sav", full.names = TRUE)
  flist_rds <- list.files(path = rlms_folder, recursive = TRUE, pattern = "*.Rds", full.names = TRUE)

  rds_index <- stringr::str_detect(flist_rds, filename)
  sav_index <- stringr::str_detect(flist_sav, filename)

  if (sum(rds_index) > 0) {
    # if Rds file is present...
    df <- readRDS(flist_rds[rds_index]) # load it
  } else {
    # load original .sav file
    df <- read.rlms(flist_sav[sav_index])
  }

  return(df)
}




#' Check whether the variable is labelled
#'
#' Check whether the variable is labelled
#'
#' Check whether the variable is labelled
#'
#' @param x a vector
#' @export
#' @return TRUE/FALSE
is_labelled <- function(x) {
  return(class(x) == "labelled")
}


#' Get unlabelled values of a labelled vector
#'
#' Get unlabelled values of a labelled vector
#'
#' Get unlabelled values of a labelled vector
#'
#' @param x a vector
#' @param na.rm a logical value indicating whether NA values should be stripped
#' @export
#' @return vector of values without labels
unlabelled_values <- function(x, na.rm = FALSE) {
  if (is_labelled(x)) {
    actual_values <- unique(x)
    
    if (na.rm) {
      actual_values <- na.omit(actual_values)
    }
    
    labelled_values <- attr(x, "labels")
    unlabelled_values_answer <- setdiff(actual_values, labelled_values)
  } else {
    warning("The argument of `unlabelled_values` is not a labelled vector: zero length vector returned.")
    unlabelled_values_answer <- setdiff(actual_values, actual_values)
  }
  
  return(unlabelled_values_answer)
}




#' Check whether all values have labels
#'
#' Check whether all values have labels
#'
#' Check whether all non-NA values of a labelled variable have labels.
#'
#' @param x labelled vector
#' @export
#' @return TRUE/FALSE
all_labelled <- function(x) {
  if (is_labelled(x)) {
    all_labelled_answer <- length(unlabelled_values(x)) == 0
  } else {
    warning("The argument of `all_labelled` is not a labelled vector: TRUE returned.")
    all_labelled_answer <- TRUE
  }
  return(all_labelled_answer)
}


