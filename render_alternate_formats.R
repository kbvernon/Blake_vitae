
# this is just so I can save elsewhere
# when I want to add another section
# without having to change everything in the Rmd
# or in the default format of the pdf

render_vitae <- function(input, output_dir, ...) {
  output <- rmarkdown::render(
    input = input,
    output_format = "pagedown::html_paged",
    output_file = NULL,
    output_dir = output_dir,
    output_options = list(
      self_contained = TRUE,
      css = "style.css",
      number_sections = FALSE
    ),
    ...
  )
  
  pagedown::chrome_print(output)
  
}

render_vitae(
  input = "blake_vitae.Rmd",
  output_dir = here::here()
)