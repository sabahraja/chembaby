#'lewisdot
#'
#'This function allows you to generate lewis dot structures for any element of the periodic table
#'@param ElementSymbol Element Symbol must be provided in order for the function to work. Reference: https://www.ptable.com/
#'@keywords lewis periodic
#'@export
#'@examples
#'lewisdot(ElementSymbol='H')
#'lewisdot(ElementSymbol='Fe')
#'lewisdot(ElementSymbol='LR') Does not generate a lewis structure for a wrong element symbol (case sensitive)


library(plotrix)
#Lewis Dot Structure:Group 1A ----------
ElementSymbol<-character()
lewisdot<-function(ElementSymbol){   #setting up the function
  library(plotrix)
plot.new();plot.window(xlim = c(-20,20),ylim = c(-20,20)) #Starting a new plot
plot(1,type='n',ann = FALSE,axes = FALSE);text(1,1,ElementSymbol,cex = 5) #Specifying a new plot
# Group 1A ---------------
if (ElementSymbol==('H')|ElementSymbol==('Li')|
      ElementSymbol==('Na')|ElementSymbol==('K')|
      ElementSymbol==('Rb')|ElementSymbol==('Cs')|
      ElementSymbol==('Fr')|ElementSymbol==('Cr')|
      ElementSymbol==('Cu')|ElementSymbol==('Nb')|
      ElementSymbol==('Mo')|ElementSymbol==('Ru')|
      ElementSymbol==('Rh')|ElementSymbol==('Ag')|
      ElementSymbol==('Pt')|ElementSymbol==('Au')){

  points(1,1.3,pch=1,col='red',lwd=10) #1 dot
}
if (ElementSymbol==('Cr')|
    ElementSymbol==('Cu')|ElementSymbol==('Nb')|
    ElementSymbol==('Mo')|ElementSymbol==('Ru')|
    ElementSymbol==('Rh')|ElementSymbol==('Ag')|
    ElementSymbol==('Pt')|ElementSymbol==('Au')){

  points(1,1.3,pch=1,col='red',lwd=10) #1 dot
}
if (ElementSymbol==('Cr')|ElementSymbol==('Cu')|
    ElementSymbol==('Nb')|ElementSymbol==('Mo')|
    ElementSymbol==('Ru')|ElementSymbol==('Rh')|
    ElementSymbol==('Ag')|ElementSymbol==('Pt')|
    ElementSymbol==('Au')){
  warning("Selected element is a transition metal. This Lewis dot diagram is based
            on the usual valence (1 electron) of the element.")
}
# Group 2A ----------------
 if (ElementSymbol==('Be')|ElementSymbol==('Mg')|
     ElementSymbol==('Ca')|ElementSymbol==('Sr')|
     ElementSymbol==('Ba')|ElementSymbol==('Ra')|
     ElementSymbol==('He')){
   points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
   points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
 }

if (ElementSymbol==('Sc')|ElementSymbol==('Ti')|
    ElementSymbol==('V')|ElementSymbol==('Mn')|
    ElementSymbol==('Co')|ElementSymbol==('Fe')|
    ElementSymbol==('Ni')|ElementSymbol==('Tc')|
    ElementSymbol==('Ta')|ElementSymbol==('W')|
    ElementSymbol==('Re')|ElementSymbol==('Os')|
    ElementSymbol==('Ir')|ElementSymbol==('Hf')|
    ElementSymbol==('Hg')|ElementSymbol==('Zn')|
    ElementSymbol==('Y')|ElementSymbol==('Zr')|
    ElementSymbol==('Cd')|ElementSymbol==('La')|
    ElementSymbol==('Ac')|ElementSymbol==('Rf')|
    ElementSymbol==('Ha')|ElementSymbol==('Sg')|
    ElementSymbol==('Ns')|ElementSymbol==('Hs')|
    ElementSymbol==('Mt')|ElementSymbol==('Rg')|
    ElementSymbol==('Cn')|
    ElementSymbol==('Ce')|
    ElementSymbol==('Pr')|ElementSymbol==('Nd')|
    ElementSymbol==('Pm')|ElementSymbol==('Sm')|
    ElementSymbol==('Eu')|ElementSymbol==('Gd')|
    ElementSymbol==('Tb')|ElementSymbol==('Dy')|
    ElementSymbol==('Ho')|ElementSymbol==('Er')|
    ElementSymbol==('Tm')|ElementSymbol==('Yb')|
    ElementSymbol==('Lu')|
    ElementSymbol==('Th')|ElementSymbol==('Pa')|
    ElementSymbol==('U')|ElementSymbol==('Np')|
    ElementSymbol==('Pu')|ElementSymbol==('Am')|
    ElementSymbol==('Cm')|ElementSymbol==('Bk')|
    ElementSymbol==('Cf')|ElementSymbol==('Es')|
    ElementSymbol==('Fm')|ElementSymbol==('Md')|
    ElementSymbol==('No')|ElementSymbol==('Lr')|
    ElementSymbol==('Ds')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  warning("Selected element is a transition or inner transition metal.
            This Lewis dot diagram is based
            on the usual valence (2 electrons) of the element.")
}

# Group 3A ----------------
if (ElementSymbol==('B')|ElementSymbol==('Al')|
    ElementSymbol==('Ga')|ElementSymbol==('In')|
    ElementSymbol==('Tl')|ElementSymbol==('Nh')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(1,0.7,pch=1,col='red',lwd=10) #3 dot
}
# Group 4A ----------------
if (ElementSymbol==('C')|ElementSymbol==('Si')|
    ElementSymbol==('Ge')|ElementSymbol==('Sn')|
    ElementSymbol==('Pb')|ElementSymbol==('Fl')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(0.95,0.7,pch=1,col='red',lwd=10) #3 dot
  points(1.05,0.7,pch=1,col='red',lwd=10) #4 dot
}
# Group 5A ----------------
if (ElementSymbol==('N')|ElementSymbol==('P')|
    ElementSymbol==('As')|ElementSymbol==('Sb')|
    ElementSymbol==('Bi')|ElementSymbol==('Mc')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(0.95,0.7,pch=1,col='red',lwd=10) #3 dot
  points(1.05,0.7,pch=1,col='red',lwd=10) #4 dot
  #--------------
  points(1.3,1,pch=1,col='red',lwd=10) #5 dot
}
# Group 6A ----------------
if (ElementSymbol==('O')|ElementSymbol==('S')|
    ElementSymbol==('Se')|ElementSymbol==('Te')|
    ElementSymbol==('Po')|ElementSymbol==('Lv')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(0.95,0.7,pch=1,col='red',lwd=10) #3 dot
  points(1.05,0.7,pch=1,col='red',lwd=10) #4 dot
  #--------------
  points(1.3,1,pch=1,col='red',lwd=10) #5 dot
  points(0.7,1,pch=1,col='red',lwd=10) #6 dot
}
# Group 7A ----------------
if (ElementSymbol==('F')|ElementSymbol==('Cl')|
    ElementSymbol==('Br')|ElementSymbol==('I')|
    ElementSymbol==('At')|ElementSymbol==('Ts')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(0.95,0.7,pch=1,col='red',lwd=10) #3 dot
  points(1.05,0.7,pch=1,col='red',lwd=10) #4 dot
  #--------------
  points(1.3,1,pch=1,col='red',lwd=10) #5 dot
  points(0.7,1.05,pch=1,col='red',lwd=10) #6 dot
  points(0.7,0.95,pch=1,col='red',lwd=10) #7 dot
}
# Group 7A ----------------
if (ElementSymbol==('Ne')|ElementSymbol==('Ar')|
    ElementSymbol==('Kr')|ElementSymbol==('Xe')|
    ElementSymbol==('Rn')|ElementSymbol==('Uuo')|
    ElementSymbol==('Pd')|ElementSymbol==('Og')){
  points(0.95,1.3,pch=1,col='red',lwd=10) #1 dot
  points(1.05,1.3,pch=1,col='red',lwd=10) #2 dot
  points(0.95,0.7,pch=1,col='red',lwd=10) #3 dot
  points(1.05,0.7,pch=1,col='red',lwd=10) #4 dot
  #--------------
  points(1.3,1.05,pch=1,col='red',lwd=10) #5 dot
  points(0.7,1.05,pch=1,col='red',lwd=10) #6 dot
  points(0.7,0.95,pch=1,col='red',lwd=10) #7 dot
  points(1.3,0.95,pch=1,col='red',lwd=10) #8 dot
}
if (ElementSymbol==('Pd')){
  warning("Selected element is a transition or inner transition metal.
            This Lewis dot diagram is based
          on the usual full valence (8+ electrons) of the element.")
}
if (ElementSymbol==('')){
  warning('Missing Element Symbol')}
title(main = paste('Lewis Dot Diagram of',ElementSymbol),col.main='Blue')

if (missing(ElementSymbol))(stop('Correct Element Symbol Required'))
}


