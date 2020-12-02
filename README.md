# Network Robustness article repository  

## Code and text for article on network measure robustness

Code is divided into three scripts: 

* robustness_data.R, which generates the three standardised edge lists (with additional information) which form the basis of the results  

* robustness_code.R - code for the three functions, one to generate a table of correlations for a range of network metrics between a sample and full network , a second which generates a list of sample networks of progressively smaller sizes and applies the first function to the list, and a third which does the same as the second with some adjustments to sample by node rather than by an attribute of an edge.  The code makes use of the C++ backend with data.table and parallel computing with furrr to speed things up. 

* robustness_final - applies the function to the three datasets and does some data wrangling to knit the files together in final form to be used for figures in the manuscript.

The folder network_robustness_tool contains the source code for a Shiny app which generates robustness results from a user-uploaded edge list

article_draft_manuscript_latex.Rmd contains the R markdown for the final LaTeX copy, including the code to generate all figures using ggplot2