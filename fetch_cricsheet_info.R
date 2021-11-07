# For retrieving match info ----------------------------------------------------

fetch_cricsheet_match_info <- function(
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
        "https://cricsheet.org/downloads/{url_segment}_{sex}_csv2.zip"))
  
  url <- possible_urls |>
    dplyr::filter(competition_type == competition) |>
    dplyr::pull(glued_url)
  
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
  
  # Read data from multiple CSVs stored in the temp directory
  all_matches_info <- readr::read_csv(
    info_filepaths, col_names = c("col_to_delete", "data"),
    id = "path", skip = 1)
  
  unlink(temp)
  
  # Tidy up and subset to match metadata only
  # (i.e., excluding player / people metadata)
  all_matches_info_tidy <- all_matches_info |>
    dplyr::select(-col_to_delete) |>
    tidyr::separate(
      data, sep = "\\,",
      c("key", "value"),
      extra = "merge", fill = "right") |>
    dplyr::filter(!key %in% c("player", "registry")) |>
    tidyr::separate(
      path, sep = "/",
      c("path_start", "match_id")) |>
    dplyr::select(-path_start) |>
    dplyr::mutate(
      match_id = stringr::str_replace(
        match_id, "_info.csv", ""))
  
  return(all_matches_info_tidy)

}