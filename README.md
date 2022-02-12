
This is my attempt at a data-driven curriculum vitae using [pagedown](https://github.com/rstudio/pagedown). 

The __motivation__ for this is four-fold:

1. Separating data and formatting makes updating the CV much, much easier.
2. The default resume template in pagedown is really nice, but almost too nice. I wanted something a wee bit more austere.  
2. The data structures assumed in [datadrivencv](https://github.com/nstrayer/datadrivencv) are woefully inadequate for my field. 
2. I wanted the ability to turn sections of the CV on or off depending on the content requirements of various academic institutions and granting agencies.

So, I made this. It uses child Rmd documents for each section of the CV. These are conditionally evaluated (included or not included) based on yaml parameters. It draws data from a Google Sheet using the [googlesheets4](https://googlesheets4.tidyverse.org/) package. And, it works with a custom block environment (like a Frankenstein'd list) that handles page breaks better than tables.