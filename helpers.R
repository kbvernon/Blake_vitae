
#' Collect Zotero bibliography with credentials 
#'
#' @param collection `data.frame` with IDs for Zotero collections
#' @param zotero `data.frame` with credentials for connecting to Zotero
#' @param selection which collections to return
#' 
#' @export
#'
bibliography <- function(collection, zotero, selection){
  
  collection[["user"]] <- zotero[["id"]]
  collection[["key"]] <- zotero[["key"]]
  
  get_bibliography <- function(name, id, user, key){
    
    tbl <- RefManageR::ReadZotero(
      user = user,
      .params = list(
        key = key,
        collection = id
      )
    )
    
    tbl <- tibble::as_tibble(tbl)
    
    tbl[["collection"]] <- name
    
    tbl
    
  }
  
  tbl <- collection |> 
    dplyr::filter(name %in% selection) |> 
    purrr::pmap_dfr(get_bibliography) |> 
    dplyr::mutate(
      author = stringr::str_replace_all(
        author,
        pattern = " and ",
        replacement = ", "
      ),
      author = stringr::str_remove_all(author, pattern = "[{}]"),
      title = stringr::str_remove_all(title, pattern = "[{}]"),
      shorttitle = stringr::str_remove_all(shorttitle, pattern = "[{}]"),
      status = get_status(note)
    ) |>
    dplyr::arrange(dplyr::desc(year))
  
  # zotero escapes special characters
  if ("note" %in% names(tbl)){ 
    
    tbl[["note"]] <- gsub("\\\\", "", tbl[["note"]]) 
    
  }
  
  if ("pages" %in% names(tbl)){
    
    tbl[["pages"]] <- stringr::str_replace(
      tbl[["pages"]], 
      pattern = "--", 
      replacement = "-"
    )
    
  }
  
  tbl
  
}

#' Format entries according to template for collection
#' 
#' This also wraps the bibliographic data in `p` tags and creates a `div`
#' with `class="buttons` containing hyperlinks to bib entries
#'
#' @param x a data.frame or tibble with "year" and "item" columns
#' @param buttons logical, should bootstrap buttons be added?
#' 
#' @return a `list` with years, entries, and buttons
#' 
#' @export
#'
format_entries <- function(x, buttons = TRUE){
  
  collection <- unique(x[["collection"]])
  
  if (buttons){
  
    # make buttons!
    buttons <- switch(
      collection,
      "Manuscript" = manuscript_buttons(x),
      "Article" = article_buttons(x),
      "Review" = review_buttons(x),
      "Report" = report_buttons(x),
      "Presentation" = presentation_buttons(x),
      "Funding" = funding_buttons(x),
      "Synergy" = synergy_buttons(x),
      "Fieldwork" = fieldwork_buttons(x)
    )
    
  } else { 
    
    buttons <- rep(NA, nrow(x))
    
  }
  
  if (length(buttons) > 1 && !all(is.na(buttons))){ 
    
    buttons <- purrr::pmap(buttons, sew_buttons) 
    
  }
  
  # format bib entries!
  template <- get_template(collection)
  
  # let {glue} do its magic
  entries <- purrr::map(
    stringr::str_glue_data(x, template, .na = ""), 
    \(x){ htmltools::p(class = "bib-entry", htmltools::HTML(x))}
  )
  
  # dates!
  dates <- purrr::map(
    x[["year"]], 
    \(j){ htmltools::div(class = "date", htmltools::p(j)) }
  )
  
  # return!
  list(
    year = x[["year"]],
    dates = dates,
    entries = entries,
    buttons = buttons
  )
  
}

#' Build a `{glue}` template for a specific bibliographic collection
#' 
#' @param x a `data.frame` with the necessary bibliographic information
#' 
#' @return a character string specifying a `{glue}` template
#' 
get_template <- function(x){
  
  templates <- list(
    Manuscript = paste(
      "{author} ({status}). <strong>{title}</strong>.", 
      "<em>{journal}</em>."
    ),
    Article = paste0(
      "{author} ({year}). <strong>{title}</strong>. ", 
      "<em>{journal}</em>",
      "{ifelse(is.na(volume), '', paste0(' ', volume))}",
      "{ifelse(is.na(number), '', paste0(' (', number, ')'))}",
      "{ifelse(is.na(pages), '', paste0(': ', pages))}", 
      ". DOI: {doi}"
    ),
    Review = paste(
      "{author} ({year}). <strong>{title}</strong>.", 
      "<em>{journal}</em> {volume} ({number}): {pages}.", 
      "DOI: {doi}"
    ),
    Report = paste(
      "{author} ({year}). <strong>{title}</strong>.", 
      "Report submitted to the {institution}.", 
      "{note}"
    ),
    Presentation = paste(
      "{author} ({year}). <strong>{title}</strong>.", 
      "{type} at the {shorttitle},", 
      "{address}."
    ),
    Funding = "{FOT}, {organization}. <strong>{title}</strong>. PI: {PI}.",
    Synergy = paste0(
      "<strong>{position}</strong>",
      "{ifelse(is.na(authors), '', paste0('. ', authors))}",
      "{ifelse(is.na(url), paste0('. ', title), paste0('. <a href=\\'', url, '\\'>', title, '</a>'))}",
      "{ifelse(is.na(description), '', paste0('. ', description))}",
      ". Status: {status}."
    ),
    Fieldwork = paste0(
      "{project}, {county} Co., {state}, ({year}). ",
      "With {agency} as sponsoring agency for {organization}. ", 
      "PI(s): {pi}. Worked as {position} for {weeks} week(s).",
      "<br><br>"
    )
  )
  
  templates[[x]]
  
}

#' A set of functions for building bootstrap buttons for different
#' bibliographic collections
#' 
#' @param x a `data.frame` with the necessary bib information
#' 
#' @return a `list` with different types of buttons
#' 
manuscript_buttons <- function(x){
  
  list(
    github = purrr::map2(
      extract_url(x[["note"]], "github"), 
      "github", 
      make_button
    ),
    preprint = purrr::map2(
      extract_url(x[["note"]], "preprint"), 
      "preprint", 
      make_button
    )
  )
  
}

article_buttons <- function(x){
  
  list(
    article = purrr::map2(
      x[["url"]], 
      "article", 
      make_button
    ),
    github = purrr::map2(
      extract_url(x[["note"]], "github"), 
      "github", 
      make_button
    ),
    preprint = purrr::map2(
      extract_url(x[["note"]], "preprint"), 
      "preprint", 
      make_button
    )
  )
  
}

review_buttons <- function(x){
  
  list(
    review = purrr::map2(
      x[["url"]], 
      "article", 
      make_button
    )
  )
  
}

presentation_buttons <- function(x){
  
  list(
    presentation = purrr::map2(
      x[["url"]], 
      "slides", 
      make_button
    ),
    github = purrr::map2(
      extract_url(x[["note"]], "github"), 
      "github", 
      make_button
    )
  )
  
}

report_buttons <- function(x){ rep(NA, nrow(x)) }
funding_buttons <- function(x){ rep(NA, nrow(x)) }
synergy_buttons <- function(x){ rep(NA, nrow(x)) }
fieldwork_buttons <- function(x){ rep(NA, nrow(x)) }

#' Extract url from note column in zotero
#'
#' @param x the note column in the current table
#' @param i the url to extract (probably github or preprint)
#' 
#' @export
#'
extract_url <- function(x, i){
  
  x <- stringr::str_split(x, pattern = "\\n")
  
  purrr::map_chr(x, \(z){ 
    
    vv <- stringr::str_split_i(z, pattern = ": ", i = 2)
    
    names(vv) <- stringr::str_split_i(z, pattern = ": ", i = 1)
    
    unname(vv[i])
    
  })
  
}

#' Get status of submitted article from note column in zotero
#'
#' @param x the note column in the current table
#' 
get_status <- function(x){
  
  x <- stringr::str_split(x, pattern = "\\n")
  
  purrr::map_chr(x, \(z){ 
    
    vv <- stringr::str_split_i(z, pattern = ": ", i = 2)
    
    names(vv) <- stringr::str_split_i(z, pattern = ": ", i = 1)
    
    unname(vv["status"])
    
  })
  
}

#' Create a set of bootstrap hyperlink button
#'
#' @param x 
#' @param icon 
#' 
make_button <- function(x, icon){
  
  if (is.na(x)){ return(NULL) }
  
  icon_name <- switch(
    icon,
    "github" = "github",
    "preprint" = "filetype-pdf",
    "slides" = "file-easel",
    "article" = "journal-text"
  )
  
  txt <- stringr::str_to_title(icon)
  
  htmltools::withTags(
    a(
      class = "btn btn-outline-dark btn-sm",
      href = x,
      # target = "_blank",
      rel = "noopener noreferrer",
      i("", class = paste0("bi bi-", icon_name), `aria-label` = icon),
      span(txt)
    )
  )
  
}

#' Wrap multiple buttons in a `div` with `class="buttons"`
#'
#' @param ... dots to pass unknown set of buttons
#' 
sew_buttons <- function(...){
  
  buttons <- rlang::list2(...)
  
  if (all(lengths(buttons) == 0)) { return(NA) }
  
  htmltools::div(class = "buttons", buttons)
  
}

#' Standard formatting for generic cv blocks (date -- details)
#' 
#' Blocks are html divs wrapped around chunk content 
#' to make styling with css easier.
#'
#' @param x a data.frame or tibble with "year" and "item" columns
#' 
#' @return a giant block of HTML
#' 
#' @export
#'
make_blocks <- function(x){
  
  details <- ifelse(
    is.na(x[["buttons"]]),
    purrr::map(
      x[["entries"]], 
      \(j){ htmltools::div(class = "details", j) }
    ), 
    purrr::map2(
      x[["entries"]], 
      x[["buttons"]],
      \(j, k){ htmltools::div(class = "details", j, k) }
    )
  )
  
  block_rows <- purrr::map2(
    x[["dates"]], 
    details, 
    \(j, k){ htmltools::div(class = "block-row", j, k) }
  )
  
  years <- sort(unique(x[["year"]]), decreasing = TRUE)
  
  blocks <- purrr::map(years, \(y){
    
    i <- which(x[["year"]] == y)
    
    htmltools::div(class = "block-year", block_rows[i])
    
  })
  
  htmltools::div(class = "blocks", blocks)
  
}




