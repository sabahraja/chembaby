#'econfig
#'
#'This function allows you to generate the electron configuration for any element (up til Atomic # 111, Rg) of the periodic table
#'@param ElementSymbol Element Symbol must be provided in order for the function to work. Reference: https://www.ptable.com/
#'@keywords electron configuration periodic
#'@export
#'@examples
#'econfig(ElementSymbol='H')
#'econfig(ElementSymbol='Fe')
#'econfig(ElementSymbol='LR') Does not generate a lewis structure for a wrong element symbol (case sensitive)

#library(readr)
#elements <- read.csv("~/chembaby/inst/extdata/elements.csv",header=TRUE,fill=TRUE,quote = "\"")
#View(elements)
data(elements)
attach(elements)

# Electron Configuration (long) ---------------------------
econfig<-function(ElementSymbol){
  rn<-which(ElementSymbol==elements$Sym)
  paste(elements[rn,20])
}



