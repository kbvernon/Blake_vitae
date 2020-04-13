
cv_table <- function(x, format){
  
  names <- map_chr(x, function(x){ paste(x$author, collapse = ", ") })
  
  dplyr::as_tibble(x) %>%
    mutate(author = names,
           title = case_when(is.na(url) ~ title,
                             TRUE ~ paste0("<a href='", url, "' target = '_blank'>", title, "</a>"))) %>% 
    arrange(desc(date))

}

cv_kable <- function(x, bib = FALSE){
  
  align <- c("l", "r")
  
  col1_css <- paste0("border: 0 solid transparent; ",
                     "padding: 8px 0; ",
                     "text-align: left;")
  
  col2_css <- paste0("border: 0 solid transparent; ",
                     "padding: 8px 0; ",
                     "text-align: right;")
  
  if(bib){
    
    align <- c("l", "l")
    
    col1_css <- paste0("border: 0 solid transparent; ",
                       "padding: 0; ",
                       "vertical-align: top;")
    
    col2_css <- paste0("border: 0 solid transparent; ",
                       "padding: 0 0 0 12px; ",
                       "text-align: justify; ",
                       "text-justify: inter-word;")
    
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
