
#Prepare R for Network Analysis workshop, 9-11am on 4/23/2019, James 116
# Description of the workshop:
#     
#  This workshop is designed for folks with some R experience.(If R is completely new to you, you could still
#  find the workshop useful for considering  what you would like to do in the future.) The workshop will give
#  an overview of network applications for understanding ecological systems, including socioecological networks
#  that determine whether ecological management is likely to be successful. Examples  will include invasive management, 
#  and are relevant to many "intervention ecology" applications.
# 
# Please bring a laptop with the most recent versions of R (https://cran.r-project.org/),  R studio (https://www.rstudio.com/), 
# and the following packages installed: graph, ergm, ggplot2, network, sna, tidyverse



install.packages(c("igraph", "ergm", "ggplot2", "network", "sna", "tidyverse", "stringi"))

library(igraph)
library(ergm)
library(ggplot2)
library(network)
library(sna)
library(stringi)
library(tidyverse)