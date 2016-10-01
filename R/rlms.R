
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


#' Extract variable labels
#'
#' Extract variable labels
#'
#' Extract variable labels
#'
#' @param df data.frame read from rlms file
#' @export
#' @return data frame with variable labels
rlms_extract_variable_labels <- function(df) {
  
  var_meta <- data.frame(var = names(df), varlabel = "", spss_format = "", stringsAsFactors = FALSE)
  
  for (i in 1:ncol(df)) {
    spss_format <- attr(df[[i]], "format.spss")
    if (!is.null(spss_format)) {
      var_meta$spss_format[i] <- spss_format
    }
    
    varlabel <- attr(df[[i]], "label")
    if (!is.null(varlabel)) {
      var_meta$varlabel[i] <- varlabel
    }
  }
  return(var_meta)
}

#' Extract value labels
#'
#' Extract value labels
#'
#' Extract value labels
#'
#' @param df data.frame read from rlms file
#' @export
#' @return data frame with value labels
rlms_extract_value_labels <- function(df) {

  value_meta <- NULL
  for (var in names(df)) {
    value <- get_labels(df[[var]])
    
    if ("" %in% value) {
      message("Variable ", var, " contains empty value label ''. Empty value label was removed.")
      value <- setdiff(value, "")
    }

    # sometimes class is "labelled" but there are no labels :)
    if (length(value) > 0) {
      # message(var)
      temp_value_meta <- data.frame(value = value, vallabel = names(value),
                         var = var, stringsAsFactors = FALSE, row.names = NULL)
      value_meta <- dplyr::bind_rows(value_meta, temp_value_meta)
    }
  }

  return(value_meta)
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

#' Clean rlms data
#'
#' Clean rlms data and all meta information
#'
#' Clean rlms data and all the meta information. Destroy useless attributes.
#'
#' @param df data.frame read by haven::read.spss
#' @param yesno convert yes/no answers to lowercase yes/no without apostrophes
#' @param apostrophe trim apostrophes, TRUE by default
#' @param remove_empty remove empty labels, TRUE by default
#' @param suppress logical, if true the default message is suppressed
#' @param nine2na convert 99999990+ to NA for numeric variables
#' @param empty2na convert empty character values to NA
#' @param colnames_tolower a logical value, indicating whether variable names should be converted to lowercase.
#' @param verbose add some debugging output
#' TRUE by default.
#' @export
#' @return dataframe
rlms_cleanup <- function(df, suppress = TRUE,
                         empty2na = TRUE,
                         nine2na = TRUE,
                         yesno = TRUE,
                         apostrophe = TRUE,
                         remove_empty = TRUE,
                         colnames_tolower = TRUE,
                         verbose = FALSE) {
  
  if (verbose) {
    message("Cleanup options:")
    message("Convert '' to NA, empty2na = ", empty2na)
    message("Convert 99999990+ to NA, nine2na = ", nine2na)
    message("Convert column names to lowercase, colnames_tolower = ", colnames_tolower)
    message("Standartise Yes/NO to yes/no, yesno = ", yesno)
    message("Remove redundant apostrophes, apostrophe = ", apostrophe)
    message("Remove empty value label, remove_empty = ", remove_empty)
  }
  

  if (colnames_tolower) {
    colnames(df) <- stringr::str_to_lower(colnames(df))
  }
  


  for (var in colnames(df)) {
    var_class <- class(df[[var]])
    
    if (verbose) {
      # message("Processing variable: ", var, " of class ", var_class)
    }
    

    if ((nine2na) & (var_class == "numeric")) {
      # replace 99999990+ for numeric variables
      df[[var]][df[[var]] > 99999990] <- NA
      # one cannot use ifelse as it destroys attributes!!!
    }
    
    if (empty2na)  {
      df[[var]][df[[var]] == ""] <- NA
    }
    
    if (yesno) {
      if (var_class == "character") {
        df[[var]] <- rlms_yesno_standartize(df[[var]])
      }
      if ((var_class == "labelled") & length(attr(df[[var]], "labels") > 0)) {
        attr(attr(df[[var]], "labels"), "names") <- rlms_yesno_standartize(attr(attr(df[[var]], "labels"), "names"))
      } 
    }
  
    
    if (apostrophe) {
      if (var_class == "character") {
        df[[var]] <- rlms_remove_apostrophe(df[[var]])
      }
      if ((var_class == "labelled") & length(attr(df[[var]], "labels") > 0)) {
        attr(attr(df[[var]], "labels"), "names") <- rlms_remove_apostrophe(attr(attr(df[[var]], "labels"), "names"))
      } 
    }
    
    
    if (remove_empty) {
      # remove "" in value labels

      value_labels <- get_labels(df[[var]])

      labels <- names(value_labels)
      values_with_empty_labels <- value_labels[labels == ""]

      if (length(values_with_empty_labels) > 0) {
        # we play on the safe side and check that variable has no empty values
        values_to_remove <- setdiff(values_with_empty_labels, unique(df[[var]]))
        # setdiff kills names
        attr(df[[var]], "labels") <- value_labels[!value_labels %in% values_to_remove]
      }
    }



  }

  return(df)
}

#' Transform all labelled variables into plain vector variables
#'
#' Transform all labelled variables into plain vector variables
#'
#' Transform all labelled variables into plain vector variables
#'
#' @param df data.frame with labelled variables
#' @export
#' @return df data.frame with numeric variables instead of labelled
#' @examples
#' df_labelled <- data.frame(x = haven::labelled(c(1, 1, 2, NA), c(Male = 1, Female = 2)), y = 1:4)
#' df_new <- rlms_labelled2plain(df_labelled)
rlms_labelled2plain <- function(df) {
  for (var in names(df)) {
    if (is_labelled(df[[var]])) {
      # preserve variable label: it will show automatically in Rstudio
      variable_label <- attr(df[[var]], "label")

      # as.vector works well with both numeric and character variables
      df[[var]] <- as.vector(df[[var]])

      attr(df[[var]], "label") <- variable_label
    }
  }
  return(df)
}



#' Transform all labelled variables into factor or numeric
#'
#' Transform all labelled variables into factor or numeric
#'
#' Transform all labelled variables into factor or numeric
#'
#' @param df data.frame with labelled variables
#' @param verbose add some debugging information
#' @export
#' @return df data.frame with factor or numeric variables instead of labelled
#' @examples
#' df_labelled <- data.frame(x = haven::labelled(c(1, 1, 2, NA), c(Male = 1, Female = 2)), y = 1:4)
#' df_new <- rlms_labelled2factor(df_labelled)
rlms_labelled2factor <- function(df, verbose = FALSE) {

  if (verbose) {
    message("The option haven = 'factor' is experimental and subject to change.")
  }


  for (var in names(df)) {
    # preserve variable label: it will show automatically in Rstudio
    var_class <- class(df[[var]])
    if (verbose) {
      # message("Converting variable ", var, " of class ",  var_class)
    }
    variable_label <- attr(df[[var]], "label")

    if (is_labelled(df[[var]])) {
      if (all_labelled(df[[var]])) {
        # Rule 1: If all values are labelled then type is factor
        df[[var]] <- as_factor_safe(df[[var]])

        # standard conversion will throw warning in the case of duplicate levels:
        # df[[var]] <- haven::as_factor(df[[var]])

      } else if (all_but_one_labelled(df[[var]])) {
        # Rule 2: If all values but one in the middle are labelled then type is factor
        df[[var]] <- as_factor_safe(df[[var]])
        message("Labelled variable ", var, " was considered as factor: it has only one unlabelled value.")
        message("This unlabelled value is neither minimal neither maximal.")

      } else if (all_but_rlmsna_labelled(df[[var]])) {
        # Rule 3: If all unlabelled values of a numeric variable are NA codes then type is factor
        df[[var]] <- as_factor_safe(df[[var]])
        message("Labelled variable ", var, " was considered as factor: all unlabelled values are bigger than 99999990.")

      } else { 
        # numeric will be kept as numeric and character as character
        df[[var]] <- as.vector(df[[var]])
      }
    }

    attr(df[[var]], "label") <- variable_label
  }
  return(df)
}


#' Read rlms data, old legacy code
#'
#' Read rlms data and all meta information, old legacy code
#'
#' Read rlms data and all the meta information. Destroy useless attributes, old legacy code
#'
#' @param file the filename
#' @param yesno convert yes/no answers to lowercase yes/no without apostrophes
#' @param apostrophe trim apostrophes, TRUE by default
#' @param remove_empty remove empty labels, TRUE by default
#' @param suppress logical, if true the default message is suppressed
#' @param nine2na automatically convert 99999999 to NA for numeric variables
#' @param colnames_tolower a logical value, indicating whether variable names should be converted to lowercase.
#' TRUE by default.
#' @export
#' @return dataframe
#' @examples
#' # rlms_legacy_read("r21i_os24a.sav")
rlms_legacy_read <- function(file,
                      suppress = TRUE,
                      nine2na = TRUE,
                      yesno = TRUE,
                      apostrophe = TRUE,
                      remove_empty = TRUE,
                      colnames_tolower = TRUE) {
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

  if (yesno) {
    value_meta <-
      dplyr::mutate(value_meta, vallabel = rlms_yesno_standartize(vallabel))
  }
  if (apostrophe) {
    value_meta <-
      dplyr::mutate(value_meta, vallabel = rlms_remove_apostrophe(vallabel))
  }

  # add wave-level-sample:
  fileinfo <- rlms_fileinfo(file)

  df$wave <- fileinfo$wave
  df$level <- fileinfo$level
  df$sample <- fileinfo$sample


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


#' Read rlms data
#'
#' Read rlms data and all meta information
#'
#' Read rlms data and all the meta information. Destroy useless attributes.
#'
#' @param file the filename
#' @param haven use haven package:
#' "labelled" - return labelled variables,
#' "factor" - return factor or numeric variables,
#' "numeric" - return numeric variables.
#' @param suppress deprecated
#' @param verbose logical verbose output 
#' @param ... further parameters passed to rlms_cleanup() and rlms_labelled2factor() functions
#' @export
#' @return dataframe
#' @examples
#' # rlms_read("r21i_os24a.sav")
rlms_read <- function(file, haven = c("factor", "labelled", "numeric"),
                      suppress, verbose = FALSE, ...) {

  haven <- match.arg(haven) # check numeric/labelled/factor

  df <- haven::read_spss(file)

  df <- rlms_cleanup(df, verbose = verbose, ...)

  attr(df, "var_meta") <- rlms_extract_variable_labels(df)
  attr(df, "value_meta") <- rlms_extract_value_labels(df)

  if (haven == "factor") {
    df <- rlms_labelled2factor(df, verbose = verbose, ...)
  }

  if (haven == "numeric") {
    df <- rlms_labelled2plain(df)
  }


  # add wave-level-sample:
  fileinfo <- rlms_fileinfo(file)

  df$wave <- fileinfo$wave
  df$level <- fileinfo$level
  df$sample <- fileinfo$sample


  if (!missing(suppress)) {
    warning("Option 'supress' is deprecated. Use 'verbose' instead :)")
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
#' @param ... arguments passed to rlms_read
#' @return nothing
#' @export
#' @examples
#' # rlms_sav2rds("~/Documents/rlms_data/")
rlms_sav2rds <- function(rlms_folder = getwd(), flatten = TRUE, ...) {
  # remove trailing "/" if present
  if (stringr::str_sub(rlms_folder, start = -1) == "/") {
    rlms_folder <- stringr::str_sub(rlms_folder, end = -2)
  }

  flist_in <- list.files(path = rlms_folder,
                         recursive = TRUE, pattern = "*.sav", full.names = TRUE)

  flist_info <- rlms_fileinfo(flist_in)

  flist_out <- stringr::str_replace(flist_in, ".sav", ".Rds")
  if (flatten) {
    flist_out <- paste0(rlms_folder, "/", basename(flist_out)) # remove path if we flatten folder structure
  }


  for (j in 1:length(flist_in)) {
    message("Processing ", flist_info$file_short[j],
            ", wave: ", flist_info$wave[j],
            ", level: ", flist_info$level[j],
            ", sample: ", flist_info$sample[j],
            ", ", (100 * j) %/% length(flist_in), "% done")
    temp <- rlms_read(flist_in[j], ...)
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
#' @param ... arguments passed to rlms_read
#' @return data.frame with RLMS data
#' @export
#' @examples
#' # rlms_load("~/Documents/rlms_data/", wave = 20, level = "individual", sample = "rep" )
rlms_load <- function(rlms_folder = getwd(), wave,
                         level = c("individual", "household", "reproductive"),
                         sample = c("all", "representative"), ...) {
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
    df <- rlms_read(flist_sav[sav_index], ...)
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
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' is_labelled(x)
is_labelled <- function(x) {
  return(class(x) == "labelled")
}

#' Get variable label
#'
#' Get variable label
#'
#' Get variable label
#'
#' @param x a vector
#' @export
#' @return character variable label
get_label <- function(x) {
  return(attr(x, "label"))
}

#' Get value labels of a vector
#'
#' Get value labels of a vector
#'
#' Get value labels of a vector
#'
#' @param x a vector
#' @export
#' @return character vector value labels
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' get_labels(x)
get_labels <- function(x) {
  return(attr(x, "labels"))
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
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' unlabelled_values(x)
unlabelled_values <- function(x, na.rm = FALSE) {
  if (is_labelled(x)) {
    actual_values <- unique(x)

    if (na.rm) {
      actual_values <- stats::na.omit(actual_values)
    }

    labelled_values <- get_labels(x)
    unlabelled_values_answer <- setdiff(actual_values, labelled_values)
  } else {
    warning("The argument of `unlabelled_values` is not a labelled vector: NULL returned.")
    unlabelled_values_answer <- NULL
  }

  return(unlabelled_values_answer)
}


#' Get labelled values of a labelled vector
#'
#' Get labelled values of a labelled vector
#'
#' Get labelled values of a labelled vector
#'
#' @param x a vector
#' @param na.rm a logical value indicating whether NA values should be stripped.
#' Normally NA is not labelled and is not returned even with na.rm = FALSE.
#' @export
#' @return vector of values with labels
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' labelled_values(x)
labelled_values <- function(x, na.rm = FALSE) {
  if (is_labelled(x)) {
    actual_values <- unique(x)

    if (na.rm) {
      actual_values <- stats::na.omit(actual_values)
    }

    labelled_values <- get_labels(x)
    labelled_values_answer <- actual_values[actual_values %in% labelled_values]
  } else {
    warning("The argument of `unlabelled_values` is not a labelled vector: NULL returned.")
    labelled_values_answer <- NULL
  }

  return(labelled_values_answer)
}



#' Check whether all values have labels
#'
#' Check whether all values have labels
#'
#' Check whether all values of a labelled variable have labels.
#'
#' @param x labelled vector
#' @param na.rm a logical value indicating whether NA values should be stripped. TRUE by default
#' @export
#' @return TRUE/FALSE
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' all_labelled(x)
all_labelled <- function(x, na.rm = TRUE) {
  if (is_labelled(x)) {
    all_labelled_answer <- length(unlabelled_values(x, na.rm = na.rm)) == 0
  } else {
    warning("The argument of `all_labelled` is not a labelled vector: TRUE returned.")
    all_labelled_answer <- TRUE
  }
  return(all_labelled_answer)
}


#' Check whether all values but rlms na (99999990+)  have labels
#'
#' Check whether all values but rlms na (99999990+)  have labels
#'
#' Check whether all values but rlms na (99999990+)  have labels
#'
#' @param x labelled vector
#' @export
#' @return TRUE/FALSE
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 99999995), c(Male = 1, Male = 2, Female = 3))
#' all_but_rlmsna_labelled(x)
all_but_rlmsna_labelled <- function(x) {
  
  all_but_rlmsna_labelled_answer <- FALSE
  
  if (is_labelled(x)) {
    if (is.numeric(x)) {
      if (min(unlabelled_values(x, na.rm = TRUE)) > 99999990) {
        all_but_rlmsna_labelled_answer <- TRUE
      }
    }
  } else {
    warning("The argument of `all_but_rlmsna_labelled` is not a labelled vector: FALSE returned.")
  }
  return(all_but_rlmsna_labelled_answer)
}



#' Check whether all values but one in the middle have labels
#'
#' Check whether all values but one in the middle have labels
#'
#' Check whether all non-NA but one in the middle values of a labelled variable have labels.
#'
#' @param x labelled vector
#' @export
#' @return TRUE/FALSE
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 4))
#' all_but_one_labelled(x)
all_but_one_labelled <- function(x) {

  all_but_one_labelled_answer <- FALSE

  if (is_labelled(x)) {
    if (is.numeric(x)) {
      unlabelled_x <- unlabelled_values(x, na.rm = TRUE)
      labelled_x <- labelled_values(x, na.rm = TRUE)
      if ((length(unlabelled_x) == 1) & (length(labelled_x) > 0)) {
        if (unlabelled_x > min(labelled_x) & unlabelled_x < max(labelled_x)) {
          all_but_one_labelled_answer <- TRUE
        }
      }
    }
  } else {
    warning("The argument of `all_but_one_labelled` is not a labelled vector: FALSE returned.")
  }
  return(all_but_one_labelled_answer)
}

#' Safe version of as_factor.
#'
#' Safe version of as_factor.
#'
#' Safe version of as_factor. This function keeps unlabelled values and
#' does not replace them with NA as `as_factor` in `haven` package.
#' It also avoids "duplicated levels" warning.
#'
#' @param x labelled vector
#' @export
#' @return TRUE/FALSE
#' @examples
#' x <- haven::labelled(c(1, 1, 2, 3, 4), c(Male = 1, Male = 2, Female = 3))
#' as_factor_safe(x)
as_factor_safe <- function(x) {
  old_labels <- get_labels(x)
  unlabelled_x <- unlabelled_values(x, na.rm = TRUE)

  new_labels <- c(names(old_labels), unlabelled_x)

  labels_df <- data.frame(values = c(old_labels, unlabelled_x),
                          new_labels = new_labels,
                          stringsAsFactors = FALSE)

  # this will throw warning for duplicate labels:
  # x_factor <- haven::as_factor(x)

  # so we use magic of left_join: do we have smth faster?
  x_df <- dplyr::left_join(data.frame(values = as.numeric(x)), labels_df, by = "values")

  x_factor <- factor(x_df$new_labels, levels = unique(new_labels))

  return(x_factor)
}


