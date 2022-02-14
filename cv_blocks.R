
#' Collect Zotero bibliography with credentials 
#'
#' @param zotero data.frame with credentials for connecting to Zotero
#' @param collection data.frame with collection Zotero IDs
#'
cv_get_bibliography <- function(zotero, collection){
  
  bib_collections <- list()
  
  for(i in 1:nrow(collection)){
    
    bib_col <- RefManageR::ReadZotero(
      user = zotero$id,
      .params = list(key = zotero$key,
                     collection = collection[i, "id"])
    )
    
    bib_col <- as_tibble(bib_col)
    
    bib_col$groups <- collection[i, "collection"]
    
    bib_collections[[i]] <- bib_col
    
  }
  
  bib_collections %>%
    bind_rows() %>%
    mutate(
      author = stringr::str_replace_all(
        author,
        pattern = " and ",
        replacement = ", "
      ),
      author = stringr::str_remove_all(
        author, pattern = "[{}]"
      ),
      title = stringr::str_remove_all(
        title, pattern = "[{}]"
      ),
      title = if_else(
        is.na(url),
        title,
        paste0("[", title, "](", url, ")")
      ),
      shorttitle = stringr::str_remove_all(
        shorttitle, pattern = "[{}]"
      )
    ) %>%
    arrange(desc(year))
  
}

#' Wrapper to make div elements
#' 
#' a very, very crude version of htmltools::div()
#'
div <- function(..., class = NULL, sep = NULL) {
  
  open_fence <- paste0("<div class='", class, "'>\n")
  
  content <- if (!is.null(sep)) paste(..., sep = sep) else paste(...)
  
  close_fence <- "\n</div>"
  
  paste0(open_fence, content, close_fence)
  
} 


#' Standard formatting for generic cv blocks (date -- details)
#' 
#' Blocks are html divs wrapped around chunk content 
#' to make styling with css easier.
#'
#' @param x data.frame
#'
make_block <- function(x){
  
  x %>% 
    mutate(
      details = div(item, class = "details"),
      date = div(year, class = "date")
    ) %>% 
    rowwise() %>% 
    mutate(
      block_rows = div(date, details, class = "block-row", sep = "\n")
    ) %>%
    group_by(year) %>% 
    summarize(
      blocks = div(paste0(block_rows, collapse = "\n"), class = "block-year")
    ) %>% 
    arrange(desc(year)) %>% 
    pull(blocks) %>% 
    paste0(collapse = "\n") %>% 
    div(class = "blocks") %>% 
    cat()
  
}
