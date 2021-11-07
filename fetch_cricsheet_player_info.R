# For retrieving player info ---------------------------------------------------

fetch_cricsheet_player_info <- function(
  competition = c(
    "apl", "bbl", "bpl", "cpl", "county", "edwards_cup", "heyhoe_flint_trophy",
    "ipl", "multi_day_matches", "msl", "odi_men", "odi_women", "odm_men",
    "odm_women", "psl", "sheffield_shield", "super_smash_men",
    "super_smash_women", "tests_men", "tests_women", "the_hundred_men",
    "the_hundred_women", "t20_blast", "t20_int_men", "t20_int_women",
    "t20_non_official_men", "t20_non_official_women", "wbbl", "wipl")
  )
{
  
  # Define URL based on function arguments
  possible_urls <- data.frame(
    competition_type = c(
      "apl", "bbl", "bpl", "cpl", "county", "edwards_cup",
      "heyhoe_flint_trophy", "ipl", "multi_day_matches", "msl", "odi_men",
      "odi_women", "odm_men", "odm_women", "psl", "sheffield_shield",
      "super_smash_men", "super_smash_women", "tests_men", "tests_women",
      "the_hundred_men", "the_hundred_women", "t20_blast", "t20_int_men",
      "t20_int_women", "t20_non_official_men", "t20_non_official_women",
      "wbbl", "wipl"),
    url_segment = c(
      "apl", "bbl", "bpl", "cpl", "cch", "cec", "rhf", "ipl", "mdms", "msl",
      rep("odis", 2), rep("odms", 2), "psl", "ssh", rep("ssm", 2),
      rep("tests", 2), rep("hnd", 2), "ntb", rep("t20s", 2), rep("it20s", 2),
      "wbb", "wtc"),
    sex = c(
      rep("male", 5), rep("female", 2), rep("male", 4), "female", "male",
      "female", rep("male", 3), "female", "male", "female", "male", "female",
      rep("male", 2), "female", "male", rep("female", 3))) |>
    dplyr::mutate(
      glued_url = glue::glue(
        "https://cricsheet.org/downloads/{url_segment}_{sex}_csv2.zip"),
      zip_filename = glue::glue(
        "{url_segment}_{sex}_csv2.zip"))
  
  url <- possible_urls |>
    dplyr::filter(competition_type == competition) |>
    dplyr::pull(glued_url)
  
  # TODO: Check if the same file has been downloaded
  # in the current R session
  # zip_filename <- possible_urls |>
  #  dplyr::filter(competition_type == competition) |>
  #  dplyr::pull(zip_filename)
  
  # Download zip file from Cricsheet.org
  temp <- tempfile()
  download.file(url, temp)
  
  # List all files in zip
  check_files <- as.character(
    unzip(temp, exdir = tempdir(), list = TRUE)$Name)
  
  # List all "*_info.csv" files in zip
  info_files <- as.data.frame(check_files) |>
    dplyr::mutate(
      exclude_flag = dplyr::case_when(
        stringr::str_detect(check_files, "_info") ~ "include",
        TRUE                                      ~ "exclude")) |>
    dplyr::filter(exclude_flag == "include") |>
    dplyr::select(-exclude_flag) |>
    dplyr::pull(check_files)
  
  # List match files with full file paths
  info_filepaths <- file.path(tempdir(), info_files)
  
  # Unzip the file and read in only the CSV files that
  # correspond to match info metadata
  unzip(temp, info_files, exdir = tempdir())
  
  # Attempt 1:
  # Read data from multiple CSVs stored in the temp directory
  # Fills rows of unequal length, but haven't figured out
  # how to add a column with the source filepath included
  # all_matches_info <- do.call(
  #  "rbind", lapply(
  #    info_filepaths,
  #    FUN=function(files){
  #      read.table(
  #        files, fill = TRUE, header = FALSE, sep = ",", skip = 1,
  #        col.names = c(
  #          "delete1", "key", "value", "delete2", "delete3"))
  #      }))
  
  # Attempt 2:
  # Read data from multiple CSVs stored in the temp directory
  # Does not fill rows of unequal length,
  # But does add column with the source filepath included
  all_matches_info <- suppressWarnings(
    readr::read_csv(
      info_filepaths, id = "path", guess_max = 100,
      col_names = c("col_to_delete", "key", "value"),
      skip = 1, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()))
  )
  # Note: Warning suppressed because the source data
  # changes format slightly when displaying player metadata.
  # Match metadata is in key-value pairs,
  # while player metadata contains additional value columns
  # We can safely suppress the warning(s) here because
  # we omit player metadata later on in the tidying process.
  
  # Tidy up and subset to match metadata only
  # (i.e., excluding player / people metadata)
  # Note: Warning suppressed again as per note above.
  all_matches_info_tidy <- suppressWarnings(
    all_matches_info |>
      dplyr::select(-col_to_delete) |>
      dplyr::filter(key %in% c("player", "players")) |>
      dplyr::mutate(key = "player") |>
      tidyr::separate(
        value, sep = ",", c("team", "player_name")) |>
      tidyr::separate(
        path, sep = "/", c("path_start", "match_id")) |>
      dplyr::select(-path_start, -key) |>
      dplyr::mutate(
        match_id = stringr::str_replace(
          match_id, "_info.csv", ""))
  )
  
  unlink(temp)
  
  return(all_matches_info_tidy)

}