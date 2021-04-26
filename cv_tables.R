
#' Collect Zotero bibliography with credentials 
#'
#' @param zotero data.frame with credentials for connecting to Zotero
#' @param collection data.frame with collection Zotero IDs
#'
cv_get_bib <- function(zotero, collection){
  
  bib_collections <- list()
  
  for(i in 1:nrow(collection)){
    
    bib_col <- RefManageR::ReadZotero(user = zotero$id,
                                      .params = list(key = zotero$key,
                                                     collection = collection[i, "id"]))
    
    bib_col <- as_tibble(bib_col)
    
    bib_col$groups <- collection[i, "collection"]
    
    bib_collections[[i]] <- bib_col
    
  }
  
  bib_collections %>%
    bind_rows() %>% 
    mutate(author = stringr::str_replace_all(author, 
                                             pattern = " and ", 
                                             replacement = ", "),
           title = stringr::str_remove_all(title, pattern = "[{}]"),
           shorttitle = stringr::str_remove_all(shorttitle, pattern = "[{}]")) %>% 
    mutate(title = if_else(is.na(url), 
                           title,
                           paste0("<a href='", url, "' target = '_blank'>", title, "</a>"))) %>% 
    arrange(desc(year))
  
}


#' Standard formatting for tables
#' 
#' All tables have two columns and some standard formatting, but differ
#' depending on whether they are cv_bibble() or cv_kable().
#'
#' @param x data.frame
#' @param align vector of column alignments
#' @param col1_css character string with additional css for column one
#' @param col2_css character string with additional css for column two
#'
kibble <- function(x, align, col1_css, col2_css) {
  
  kable(x,
        # format = "html",
        escape = FALSE,
        col.names = NULL,
        align = align) %>%
    kable_styling(full_width = TRUE) %>%
    column_spec(1, extra_css = col1_css) %>%
    column_spec(2, extra_css = col2_css) %>%
    row_spec(nrow(x), extra_css = "border-bottom: 0;") %>%
    collapse_rows(columns = 1, valign = "top")
  
}

cv_bibble <- function(x) {
  
  align <- c("l", "l")
  
  col1_css <- paste0("border: 0; ",
                     "padding: 0; ",
                     "vertical-align: top; ")
  
  col2_css <- paste0("border: 0; ",
                     "padding: 0 0 0 12px; ",
                     "text-align: justify; ",
                     "text-justify: inter-word;")
  
  kibble(x = x, align = align, col1_css = col1_css, col2_css = col2_css)
  
}


cv_kable <- function(x, bib = FALSE){
  
  align <- c("l", "r")
  
  col1_css <- paste0("border: 0; ",
                     "padding: 8px 0; ")
  
  col2_css <- paste0("border: 0; ",
                     "padding: 8px 0; ")
  
  kibble(x = x, align = align, col1_css = col1_css, col2_css = col2_css)
  
}
