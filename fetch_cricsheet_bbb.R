# DONE: Bangladesh Premier League, BBL, Caribbean Premier League, Charlotte Edwards Cup, County Championship,
# IPL, Mzansi Super League, Pakistan Super League,
# Rachael Heyhoe Flint Trophy,
# Sheffield Shield, T20 Blast, WBBL, WIPL (Women's T20 Challenge)
# ------------------------------------------------------------------------------

# TODO: Super Smash (men and women), The Hundred (men and women)
#####
# Formats that don't have an "all_matches.csv" file:
# Non-official T20I, T20I, One-day matches, ODIs, Multi-day matches, Tests

fetch_cricsheet_bbb <- function(
  competition = c(
    "bbl", "bpl", "cpl", "county", "edwards_cup", "heyhoe_flint_trophy", "ipl",
    "msl", "psl", "sheffield_shield", "t20_blast", "wbbl", "wipl")
  #sex = c("men", "women")
  )
{
  
  # Set the value for 'sex' - for some leagues / formats this is essential!
  # e.g., women's T20I vs. men's T20I
  
  # Define URL based on function arguments
  possible_urls <- data.frame(
    competition_type = c(
      "bbl", "bpl", "cpl", "county", "edwards_cup", "heyhoe_flint_trophy",
      "ipl", "msl", "psl", "sheffield_shield", "t20_blast", "wbbl", "wipl"),
    url_segment = c(
      "bbl", "bpl", "cpl", "cch", "cec", "rhf", "ipl", "msl", "psl", "ssh",
      "ntb", "wbb", "wtc"),
    sex = c(
      "male", "male", "male", "male", "female", "female", "male", "male",
      "male", "male", "male", "female", "female")) |>
    dplyr::mutate(
      glued_url = glue::glue(
        "https://cricsheet.org/downloads/{url_segment}_{sex}_csv2.zip"))
  
  url <- possible_urls |>
    dplyr::filter(competition_type == competition) |>
    dplyr::pull(glued_url)
  
  # Download zip file from Cricsheet.org
  temp <- tempfile()
  download.file(url, temp)
  
  # Extract the CSV containing collated match data
  # and save to specified file directory (filedir arg)
  unzip(temp, "all_matches.csv", exdir = tempdir())
  
  # Read data from CSV saved locally
  all_matches <- read.csv(paste0(tempdir(), "/all_matches.csv"))
  
  unlink(temp)
  
  return(all_matches)

}