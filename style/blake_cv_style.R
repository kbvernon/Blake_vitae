
cv_bibble <- function(zotero, collection){
  
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



cv_kable <- function(x, bib = FALSE){
  
  if(bib){
    
    align <- c("l", "l")
    
    col1_css <- paste0("border: 0 solid transparent; ",
                       "padding: 0; ",
                       "vertical-align: top;")
    
    col2_css <- paste0("border: 0 solid transparent; ",
                       "padding: 0 0 0 12px; ",
                       "text-align: justify; ",
                       "text-justify: inter-word;")
    
  } else {
    
    align <- c("l", "r")
    
    col1_css <- paste0("border: 0 solid transparent; ",
                       "padding: 8px 0; ",
                       "text-align: left;")
    
    col2_css <- paste0("border: 0 solid transparent; ",
                       "padding: 8px 0; ",
                       "text-align: right;")
    
  }
  
  kb <- kable(x,
        format = "html", 
        escape = FALSE, 
        col.names = NULL, 
        align = align) %>%
    kable_styling(full_width = TRUE) %>% 
    column_spec(1, extra_css = col1_css) %>%
    column_spec(2, extra_css = col2_css)
  
  if(bib){ 
    
    kb %>% collapse_rows(columns = 1, valign = "top") 
    
  } else {
    
    kb
    
  }
  
}
