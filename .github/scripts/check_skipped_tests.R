library(dplyr)

extract_issues <- function(file){
  file_lines <- readLines(file)
  pattern <- "(?<=https://github.com/voltrondata/substrait-r/issues/)[0-9]*"
  issues <- unlist(
    stringr::str_extract_all(file_lines, stringr::regex(pattern))
  )
  unique(as.integer(issues))
}

find_issues_in_files <- function(test_files){

  issues_causing_skips <- purrr::map(test_files, ~extract_issues(.x))
  # add filenames
  issues_causing_skips <- rlang::set_names(issues_causing_skips, test_files)
  # remove files without skips
  issues_causing_skips <- issues_causing_skips[lengths(issues_causing_skips) > 0]

  issues_causing_skips
}

check_file_for_skips <- function(values, filename, closed_issues){

  runnable_tests <- intersect(values, closed_issues)
  if(length(runnable_tests) > 0){
    return(
      paste(
        ":zap: Tests skipped in file",
        gsub("tests/testthat/" ,"" , filename),
        "due to issue",
        runnable_tests
      )
    )
  }
}

find_runnable_tests <- function(issues_causing_skips, closed_issues){

  purrr::imap(
    .x = issues_causing_skips,
    .f = check_file_for_skips,
    closed_issues
  ) %>%
    unlist() %>%
    unname()
}

get_closed_issues <- function(){
    # get closed issues
  jsonlite::fromJSON("https://api.github.com/repos/voltrondata/substrait-r/issues?state=closed") %>%
    pull(number)
}

test_files <- list.files("tests/testthat", full.names = TRUE, pattern = "*.R")
runnable_tests <- find_runnable_tests(
  issues_causing_skips = find_issues_in_files(test_files),
  closed_issues = get_closed_issues()
)


# Post to Slack
msg_title <- paste("Tests which can now be unskipped! :tada:")

msg_body <- paste0(runnable_tests, collapse = "\n ")

if (length(runnable_tests) > 0) {
  req <- httr::POST(
    url = Sys.getenv("SLACK_WEBHOOK_URL"),
    body = paste(
      '{"attachments": [{',
      '"pretext": "',
      msg_title,
      '",',
      '"text": "',
      msg_body,
      '", "color": "#f2f2f2"',
      '}]}',
      sep = ''
    )
  )
}

