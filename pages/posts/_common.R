
co_box <- function(color, header, contents = "Your text") {
  class <- switch(color,
    b = "note",
    g = "tip",
    r = "important",
    o = "caution",
    stop("Invalid `type`", call. = FALSE)
  )
  switch(color,
    b = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='false'}", "\n\n",
      "## ", header, "\n\n",
      "::: {style='font-size: 0.90em; color: #696969;'}\n\n",
      contents, "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    g = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='true'}", "\n\n",
      "## ", header, "\n\n",
      "::: {style='font-size: 0.90em; color: #696969;'}\n\n",
      contents, "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    o = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='false'}", "\n\n",
      "## ", header, "\n\n",
      "::: {style='font-size: 0.90em; color: #696969;'}\n\n",
      contents, "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    r = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='false'}", "\n\n",
      "## ", header, "\n\n",
      "::: {style='font-size: 0.90em; color: #696969;'}\n\n",
      contents, "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    stop("Invalid `type`", call. = FALSE)
  )
}

my_call_out <- function(...) {
  cat(":::{.callout-note collapse='true'}\n")
  cat("## Additional Resources\n")
  cat(paste0("- ", ..., collapse = "\n\n"))
  cat("\n:::\n")
}

make_my_list <- function() {
  require(stringr)
  require(snakecase)
  words <- sample(snakecase::to_random_case(stringr::words),
              size = 5, replace = FALSE)
  sentences <- sample(snakecase::to_random_case(stringr::sentences), 
              size = 3, replace = FALSE)
  letters <- sample(snakecase::to_random_case(LETTERS),
              size = 10, replace = FALSE)
  list(
    'words' = words,
    'sentences' = sentences,
    'letters' = letters
  )
}

make_mixed_list <- function() {
  list(
    'booleans' = rep(
      x = sample(c(TRUE, FALSE), size = 2, replace = FALSE),
      times = 2
    ),
    'integers' = as.integer(sample(1:10, size = 5, replace = FALSE)),
    'doubles' = round(rnorm(n = 5, mean = 3, sd = 0.2), digits = 3),
    'strings' = sample(x = stringr::words, size = 5, replace = FALSE),
    'dates' = c(lubridate::as_date(lubridate::today() - 10), 
                lubridate::as_date(lubridate::today() - 50),
                lubridate::as_date(lubridate::today() - 100)
      )
  )
}

