# workspace --------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(tidycensus) # fips_code

census_site <-
  read_html("https://www.census.gov/geographies/reference-files/time-series/geo/block-assignment-files.html")

dropdown_menu <-
  census_site |>
  html_nodes(".statelist .left") |>
  html_children() |>
  html_children()

state_info <-
  tibble(
    url = str_extract(dropdown_menu, '(?<=href=")[^"]+.zip'),
    file = basename(url),
    state = str_extract(dropdown_menu, '(?i)(?<=;">)[[:alpha:] ]+')
  ) |>
  print()

all_states <-
  seq_along(state_info$url) |>
  map(~as.list(state_info[.x, ]))


# download, unzip & combine files ----------------------------------------------
archive <- "files/input"
dir.create(archive) |> suppressWarnings()

# download files
download_zip_file <- function(x, overwrite = FALSE) {
  # x = all_states[[1]]
  message("working on ", x$state, ' - - - - - - -')
  zip_name <- file.path(archive, "raw", x$file)

  if (file.exists(zip_name) & !overwrite) {
    return(
      message(">>> skipping - file already exists for ", x$state, " use 'overwrite = TRUE'")
    )
  }

  download.file(x$url, destfile = zip_name, mode = "wb")
}

# unzip files
unzip_file <- function(x, overwrite = FALSE) {
  message("working on ", x$state, ' - - - - - - -')
  raw_location <- file.path(archive, "raw", x$file)
  new_location <- file.path(archive, "extracted", x$state)
  existing_files <- list.files(path = new_location)
  if (length(existing_files) & !overwrite) {
    return(
      message(">>> skipping - file already exists for ", x$state, " use 'overwrite = TRUE'")
    )
  }

  unzip(zipfile = raw_location, exdir = new_location)
}

# combine txt files
combine_txt_files <- function(x, overwrite = FALSE) {
  message("working on ", x$state, ' - - - - - - -')
  raw_location <- file.path(archive, "extracted", x$state)

  file_name <-
    x$file |>
    tools::file_path_sans_ext()

  new_file <-
    file.path(
      archive,
      "combined",
      paste0(file_name, ".csv")
    )

  if (file.exists(new_file) & !overwrite) {
    return(
      message(">>> skipping - file already exists for ", x$state, " use 'overwrite = TRUE'")
    )
  }

  all_files <- list.files(raw_location, full.names = TRUE)

  file_list <-
    map(
      .x = all_files,
      .f =
        ~read_delim(
          file = .x,
          delim = "|",
          col_types = cols(.default = "c")
        ) |>
        rename_with(
          .cols = -BLOCKID,
          str_c,
          str_extract(.x, "(?<=ST\\d{2}_\\w{2})\\w+(?=.txt)")
        )
    )

  full_table <-
    file_list |>
    reduce(left_join, by = "BLOCKID")

  write_csv(full_table, new_file)
}

# debugonce(download_zip_file); download_zip_file(x = all_states[1,])
# debugonce(unzip_file); unzip_file(x = all_states[1,])

# download all zip files
walk(all_states, possibly(download_zip_file, NULL)); beepr::beep(5)

# unzip all xml files
walk(all_states, possibly(unzip_file, NULL)); beepr::beep(5)

# combine text files
walk(all_states, possibly(combine_txt_files, NULL)); beepr::beep(5)


# write final CSV --------------------------------------------------------------

common_files <-
  tibble(
    file = list.files("files/input/extracted", recursive = TRUE),
    geo =
      str_remove(file, "BlockAssign_ST\\d{2}_\\w{2}_") |>
      basename() |>
      tools::file_path_sans_ext()
  ) |>
  count(geo, sort = TRUE) |>
  print()

# https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/block-assignment-record-layout.html
column_patterns <- c(
  "DISTRICT_CD" = "congressional_district",
  "PLACEFP_INCPLACE_CDP" = "incorporated_place",
  "DISTRICT_SDUNI" = "unified_school_district",
  "DISTRICT_SLDU" = "legislative_district_upper",
  "DISTRICT_SLDL" = "legislative_district_lower",
  "DISTRICT_VTD" = "voting_district"
)

read_states <-
  list.files("files/input/combined", full.names = TRUE) |>
  map(read_csv, col_types = cols(.default = "c"))

stack_tables <-
  read_states |>
  map_dfr(bind_rows)

prep_table <-
  stack_tables |>
  #slice(1:10) |>
  select(
    census_block_fips = BLOCKID,
    one_of(names(column_patterns))
  ) |>
  rename_all(str_replace_all, column_patterns) |>
  print()

addl_fips <-
  prep_table |>
  #sample_n(100) |>
  select(census_block_fips) |>
  #transmute(census_block_id = "012345678901") |>
  mutate(
    state_fips = str_sub(census_block_fips, 1, 2),
    county_fips = str_sub(census_block_fips, 1, 5),
    county_code = str_sub(census_block_fips, 3, 5),
    tract_fips = str_sub(census_block_fips, 1, 11),
    tract_numeric =
      str_sub(census_block_fips, 6, 11) |>
      str_replace("(....)(..)", "\\1.\\2") |>
      str_remove("(\\.)?0+$"),
    block_group = str_sub(census_block_fips, 12, 12),
    block_code = str_sub(census_block_fips, 12, 16)
  ) |>
  left_join(
    tidycensus::fips_codes |>
      #head(10) |>
      rename(
        postal_code = state,
        state_fips = state_code,
        county_name = county
      )
  ) |>
  select(
    census_block_fips,
    starts_with("state"),
    postal_code,
    starts_with("county"), everything()
  ) |>
  print()


final_table <-
  addl_fips |>
  left_join(prep_table) |>
  print()



write_csv(final_table, "files/output/lookup_census_block_crosswalk_2020.csv")

final_table |>
  filter(str_detect(state,  "RI")) |>
  write_csv("files/output/lookup_census_block_crosswalk_2020-sample.csv")

# local_blocks <-
#   blocks |>
#   filter(str_detect(postal_code, "DE|MD|NJ|NY|PA"))
#
# write_cdw_table(
#   "lookup_census_block_crosswalk_2020_LOCAL",
#   local_blocks
# )
