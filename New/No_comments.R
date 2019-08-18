require(purrr)
No_comments <- function(dir = getwd(), file) {
    if (missing(file)) {
        file <- list.files(dir)
        file <- file[purrr::map_lgl(file, ~substr(.x, nchar(.x) - 1, nchar(.x)) == ".R")]
    }
    dir.create(file.path(dir, "New"), showWarnings = FALSE)
    purrr::map(file, ~writeLines(as.character(parse(.x)), paste0("New/", .x)))
    return(invisible())
}
No_comments(file = "No_comments.R")
