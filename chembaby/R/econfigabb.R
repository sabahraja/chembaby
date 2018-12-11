#'econfigabb
#'
#'This function allows you to generate the abbreviated electron configuration for any element (up til Atomic # 111, Rg) of the periodic table
#'@param ElementSymbol Element Symbol must be provided in order for the function to work. Reference: https://www.ptable.com/
#'@keywords electron configuration periodic
#'@export
#'@examples
#'econfig_abb(ElementSymbol='H')
#'econfig_abb(ElementSymbol='Fe')
#'econfig_abb(ElementSymbol='LR') Does not generate a lewis structure for a wrong element symbol (case sensitive)

#library(readr)
#elements <- read.csv("~/chembaby/inst/extdata/elements.csv",header=TRUE,fill=TRUE,quote ="\"")
#View(elements)
data(elements)
attach(elements)

# Electron Configuration (abbreviated) -------------------
econfig_abb<-function(ElementSymbol){
  rn<-which(ElementSymbol==elements$Sym)
  paste(elements[rn,19])
}


