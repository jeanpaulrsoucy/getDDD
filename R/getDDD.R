#' Download defined daily dose (DDD) information
#'
#' @param atc Character vector of ATC codes for which to download DDD information.
#' @param comb Logical. Search list of combined product DDDs?
#' @param progress Logical. Report progress in real time?
#' @importFrom RCurl dynCurlReader
#' @importFrom RCurl curlPerform
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom XML readHTMLTable
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#' @export
#' @rdname getDDD
#'
#' @return A data frame with all available DDD information for the selected ATC codes. For combined product DDDs, the information is printed to the console.
#' @examples
#' \dontrun{
#' # Two regular ATC codes
#' getDDD(atc = c("J01CA04", "J01CA05"))
#' # Two regular ATC codes and a combined product ATC code
#' getDDD(atc = c("J01CA04", "J01CA05", "J01CA20"))
#' # Two regular ATC codes, a fake ATC code, and a malformed code
#' getDDD(atc = c("J01CA04", "J01CA05", "B01AC99", "BB1AC99"))
#' }

getDDD <- function(atc, comb = TRUE, progress = FALSE) {

  # Check for internet
  check_internet()

  # Ensure ATC codes are a character vector
  if (!is.character(atc)) stop("Please supply a character vector of ATC codes.")

  # Ensure ATC codes are unique

  ## Get unique codes
  atc_unique <- unique(atc)

  ## Calculate number of codes deleted
  n_deleted <- length(atc) - length(atc_unique)

  ## Report number deleted, if any
  if (n_deleted > 0) {

    message("Number of non-unique codes removed: ", n_deleted)

    }

  # Ensure codes are correctly formed ATC codes

  ## Get malformed codes
  atc_malformed <- grep("[A|B|C|D|G|H|J|L|M|N|P|R|S|V][0-9]{2,}[A-Z]{2,}[0-9]{2,}",
                        atc_unique,
                        value = TRUE,
                        invert = TRUE)

  ## If there any malformed codes, remove them and report them
  if (length(atc_malformed) > 0) {

    ## Save good codes
    codes <- atc_unique[!atc_unique %in% atc_malformed]

    ## Report malformed codes
    atc_malformed <- paste(atc_malformed, collapse = ", ")
    message("Malformed codes removed: ", atc_malformed)

    }

  else {

    ## Save good codes
    codes <- atc_unique

  }

  # Search database for DDD information on supplied ATC codes

  ## Query codes in a loop
  for (code in codes) {

    ## Perform query
    res <- dynCurlReader()

    ## Save the result
    invisible(curlPerform(postfields = paste0("code=", code), url = base_url, verbose = FALSE,
                          post = 1L, writefunction = res$update))

    ## Create table
    res_table <- readHTMLTable(res$value())

    ## Check if DDD information was found (eliminate invalid ATC codes)
    if (length(res_table) != 1) {

      ## Report no DDD information found
      if (progress) {

        message("Invalid ATC code: ", code)

      }

      next

    }

    ## Read information as data frame
    res_table <- as.data.frame(res_table)

    ## Clean introduced special characters from data frame
    if (nrow(res_table) == 1) {

      res_table <- data.frame(as.list(apply(res_table, 2, function(x) substr(x, 1, nchar(x) - 2))), stringsAsFactors = FALSE)

    } else {

      res_table <- as.data.frame(apply(res_table, 2, function(x) substr(x, 1, nchar(x) - 2)), stringsAsFactors = FALSE)

    }

    ## Name columns
    names(res_table) <- c("atc_code", "name", "ddd", "unit", "route", "note")

    ## Ensure DDD information was found (if not, information may be given in combination products table)
    if (res_table[1, "ddd"] == "") {

      ## Report no DDD information found
      if (progress) {

        message("No DDD info. found for: ", code)

      }

      next

    }

    ## Duplicate atc_code and name across columns
    res_table[, "atc_code"] <- res_table[1, "atc_code"]
    res_table[, "name"] <- res_table[1, "name"]

    ## Add information: ATC levels 1 to 4 and last updated
    res_table <- getDDDinfo(code, res, res_table, last_updated = TRUE)

    ## Create/append to output table
    if (!exists("output")) {

      output <- res_table

    } else {

      output <- rbind(output, res_table)

    }

    ## Report progress
    if (progress) {

      message("DDD info. downloaded for: ", code)

    }
  }

  ## Get failed ATCs (if any)
  if (exists("output")) {

  codes_fail <- codes[!codes %in% unique(output$atc_code)]

  }

  ## Search list of combined product DDDs
  if (comb == TRUE & length(codes_fail) > 0) {

  output_comb <- getDDDcomb(atc = codes_fail, progress)

  }

  ## Report successes/failures
  if (exists("output")) {

    message("DDD info. downloaded for ",
            length(unique(output$atc_code)),
            " ATC codes: ",
            paste(unique(output$atc_code), collapse = ", "))

    if (length(codes_fail) > 0) {

      message("DDD info. not downloaded for ",
              length(codes_fail),
              " ATC codes: ",
              paste(codes_fail, collapse = ", "))
      }

    if (exists("output_comb")) {

      if (nrow(output_comb) == 0) {


        message("No DDD info. was found in the combined product DDD table.")

        } else {

          message("However, DDD info. is available in the combined products table for ",
                  length(unique(output_comb$atc_code)),
                  " ATC codes: ",
                  paste(unique(output_comb$atc_code), collapse = ", "))

          message("\n Printing combined product table... \n")
          print(output_comb)
          message("\n See complete combined product table: https://www.whocc.no/ddd/list_of_ddds_combined_products/")

          }

      } else if (comb == TRUE & !exists("output_comb")) {

        message("No DDD information was found in the combined product DDD table.")

        }

  ## Return output table
  output
    } else message("DDD info. not downloaded for all ",
                   length(codes),
                   " ATC codes.")

  }

getDDDinfo <- function(code, res, res_table, last_updated) {

  # Add information: ATC levels 1-4
  res_html <- read_html(res$value())
  res_html <- html_text(res_html)
  res_match <- paste0(".*", "Guidelines", substr(code, 1, 1), " *(.*?) *\n.*")
  res_table[, "atc_level_1"] <- sub(res_match, "\\1", res_html)
  res_match <- paste0(".*", substr(code, 1, 3), "  *(.*?) *\n.*")
  res_table[, "atc_level_2"] <- sub(res_match, "\\1", res_html)
  res_match <- paste0(".*", substr(code, 1, 4), "  *(.*?) *\n.*")
  res_table[, "atc_level_3"] <- sub(res_match, "\\1", res_html)
  res_match <- paste0(".*", substr(code, 1, 5), "  *(.*?) *ATC code.*")
  res_table[, "atc_level_4"] <- sub(res_match, "\\1", res_html)

  # Add information: Last updated
  if (last_updated == TRUE) {

    # Only entries with DDD info in the regular index will have information on when the DDD was last updated
    res_table[, "last_updated"] <- as.Date(sub(".*Last updated: \n *(.*?) *\n.*", "\\1", res_html), format = "%Y-%m-%d")

  } else {

    # If the entry has no DDD info in its regular entry (i.e. info only in combination DDD table), use system date
    res_table[, "last_updated"] <- as.Date(Sys.Date(), format = "%Y-%m-%d")
  }

  # Return table
  res_table

}

getDDDcomb <- function(atc, progress) {

  # Download and process combined product DDD table
  comb <- readHTMLTable(content(GET(comb_url), as = "text"))[[1]]
  names(comb) <- c("atc_code", "brand_name", "dosage_form", "active_ingredients_per_ud", "ddd")
  comb <- comb[!is.na(comb$dosage_form), ] # Remove empty rows
  comb <- comb[comb$atc_code %in% atc, ] # Subset table to relevant ATC codes

  # Report results
  if (progress) {

    message("DDD info. for ",
            length(unique(comb$atc_code)),
            " ATC codes found in combined DDD table.")

  }

  # Return output table
  comb

}
