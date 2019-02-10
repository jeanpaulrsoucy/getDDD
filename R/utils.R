#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet

# Verify internet connection
check_internet <- function() {
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection.")
}

# Define URLs for ATC codes database
base_url <- "https://www.whocc.no/atc_ddd_index/"
comb_url <- "https://www.whocc.no/ddd/list_of_ddds_combined_products/"
