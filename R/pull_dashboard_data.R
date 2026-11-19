#' Copy the most recent dated .Rds file from the mounted shared drive
#'
#' Finds the most recent `{file_stub}_YYYY-MM-DD.Rds` file in one folder of
#' the locally mounted "NMFS NEC READ SSB" shared drive and copies it to
#' `data/main/`.
#'
#' @param fishery `"groundfish"`, `"fluke"`, or `"stripedbass"`. Maps to the `groundfishRDM` or
#'   `flukeRDM` folder.
#' @param management_cycle Management cycle year. Groundfish: 2026 or 2027.
#'   Fluke: 2026 or 2028. Numeric or character (`2027` and `"2027"` both work).
#' @param final_folder Name of the single folder under `{fishery}RDM`
#'   (e.g., `"input_data"`, `"miscellaneous"`). Not checked against a list.
#' @param file_stub File name stub. Matches files named exactly
#'   `{file_stub}_YYYY-MM-DD.Rds`.
#' @param overwrite If `FALSE` (default) and a file with the same name already
#'   exists in `data/main/`, the copy is skipped and the existing path is
#'   returned. If `TRUE`, the local file is replaced.
#'
#' @return A single character string: the local path to the file in
#'   `data/main/`. `NA_character_` (with a warning) if no matching file is
#'   found.
#'
#' @details
#' The caller must have declared the project root with `here::i_am()`.
#'
#' Folder searched:
#' `D:/Shared drives/NMFS NEC READ SSB/socialsci/RecreationalDST/{management_cycle}_management_cycle_data/{fishery}RDM/{final_folder}`
#'
#' Errors (rather than warns) if the mount, the source folder, or `data/main/`
#' does not exist, or if the copy fails.
#'
#' The date in the file name decides which file is most recent.
#'
#' Packages used (called with `pkg::`, so none need to be attached): dplyr,
#' glue, here, lubridate, magrittr, stringr, tibble.
#'
#' @examples
#' \dontrun{
#' here::i_am("stored_scripts/pull_dashboard_data.R")
#' source(here::here("stored_scripts", "pull_dashboard_data.R"))
#'
#' pull_dashboard_data(
#'   fishery = "groundfish",
#'   management_cycle = 2027,
#'   final_folder = "input_data",
#'   file_stub = "WGOM_Cod_projected_NAA"
#' )
#' }
pull_dashboard_data <- function(
  fishery,
  management_cycle,
  final_folder,
  file_stub,
  overwrite = FALSE
) {
  # Local binding for the magrittr pipe so the function works even if the
  # caller hasn't attached magrittr or dplyr
  `%>%` <- magrittr::`%>%`

  # Root of the mounted shared drive (agreed mount location)
  shared_drive_root <- file.path("D:/Shared drives", "NMFS NEC READ SSB")

  # Allowed management cycles by fishery
  allowed_cycles <- list(
    groundfish = c(2026, 2027,2028),
    fluke = c(2026, 2028), 
    stripedbass=c(2028)
  )

  ###################################################
  ### Validate arguments
  ###################################################

  # Each argument must be a single value (one stub per call)
  if (
    length(fishery) != 1 ||
      length(management_cycle) != 1 ||
      length(final_folder) != 1 ||
      length(file_stub) != 1 ||
      length(overwrite) != 1
  ) {
    stop("All arguments must be length 1.", call. = FALSE)
  }

  if (!fishery %in% names(allowed_cycles)) {
    stop(
      glue::glue(
        "fishery set to '{fishery}'. Only 'groundfish', 'fluke', or 'stripedbass' is allowed."
      ),
      call. = FALSE
    )
  }

  # Coerce to numeric so 2027 and "2027" both work
  management_cycle <- suppressWarnings(as.numeric(management_cycle))

  if (is.na(management_cycle)) {
    stop("management_cycle must be a valid numeric value.", call. = FALSE)
  }

  if (!management_cycle %in% allowed_cycles[[fishery]]) {
    stop(
      glue::glue(
        "{fishery} management cycle set to {management_cycle}. ",
        "Only {paste(allowed_cycles[[fishery]], collapse = ' or ')} is allowed."
      ),
      call. = FALSE
    )
  }

  if (!is.character(final_folder) || !is.character(file_stub)) {
    stop("final_folder and file_stub must be character.", call. = FALSE)
  }

  if (!is.logical(overwrite) || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }

  ###################################################
  ### Check folders
  ###################################################

  # A missing mount or folder is an error: usually the drive isn't mounted or
  # the cycle or folder name is wrong
  if (!dir.exists(shared_drive_root)) {
    stop(
      glue::glue("Shared drive mount not found: {shared_drive_root}"),
      call. = FALSE
    )
  }

  source_folder <- file.path(
    shared_drive_root,
    "socialsci",
    "RecreationalDST",
    glue::glue("{management_cycle}_management_cycle_data"),
    glue::glue("{fishery}RDM"),
    final_folder
  )

  if (!dir.exists(source_folder)) {
    stop(glue::glue("Folder not found: {source_folder}"), call. = FALSE)
  }

  destination_folder <- here::here("data", "main")

  if (!dir.exists(destination_folder)) {
    stop(
      glue::glue("Destination folder not found: {destination_folder}"),
      call. = FALSE
    )
  }

  ###################################################
  ### Find the most recent file
  ###################################################

  # Prefix every candidate name must start with
  # NOTE: startsWith() is a literal match, so regex characters in the stub
  # (e.g., ".") are not treated as wildcards
  # NOTE: paste0() rather than glue(); stringr::fixed() warns on glue objects
  stub_prefix <- paste0(file_stub, "_")

  most_recent_file <- tibble::tibble(
    name = list.files(source_folder, pattern = "\\.Rds$")
  ) %>%
    # Keep names that start with the stub plus underscore
    dplyr::filter(startsWith(name, stub_prefix)) %>%
    # Strip the prefix so only "YYYY-MM-DD.Rds" should remain
    dplyr::mutate(
      date_part = stringr::str_remove(name, stringr::fixed(stub_prefix))
    ) %>%
    # Require exactly YYYY-MM-DD.Rds after the prefix. Excludes other files
    # whose names start with the same prefix
    dplyr::filter(
      stringr::str_detect(date_part, "^\\d{4}-\\d{2}-\\d{2}\\.Rds$")
    ) %>%
    # Parse the date. Impossible dates (e.g., 2026-13-01) become NA and are
    # dropped
    dplyr::mutate(
      file_date = lubridate::ymd(
        stringr::str_sub(date_part, 1, 10),
        quiet = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(file_date)) %>%
    # Newest date first, keep one. Names are exactly {stub}_{date}.Rds, so
    # two files cannot share a date
    dplyr::arrange(dplyr::desc(file_date)) %>%
    dplyr::slice(1)

  if (nrow(most_recent_file) == 0) {
    warning(
      glue::glue(
        "No file matching {file_stub}_YYYY-MM-DD.Rds found in {source_folder}. ",
        "Returning NA."
      ),
      call. = FALSE
    )
    return(NA_character_)
  }

  ###################################################
  ### Copy to data/main
  ###################################################

  source_path <- file.path(source_folder, most_recent_file$name)
  local_path <- file.path(destination_folder, most_recent_file$name)

  # Skip the copy if the same file name is already local and overwrite = FALSE.
  # The date is in the name, so the same name should mean the same file.
  if (file.exists(local_path) && !overwrite) {
    message(glue::glue(
      "Already in data/main, skipped: {most_recent_file$name}"
    ))
    return(local_path)
  }

  copied <- file.copy(from = source_path, to = local_path, overwrite = TRUE)

  if (!copied) {
    stop(
      glue::glue("Copy failed: {source_path} -> {local_path}"),
      call. = FALSE
    )
  }

  message(glue::glue("Copied: {most_recent_file$name}"))

  return(local_path)
}
