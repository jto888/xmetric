## PLpart.r
## This is the initial function for calculating the pipeline for parts demand at bases given various stock levels
## This function is presented as equation 3.2 in "Optimal Inventory Modeling of Systems", by Craig C. Sherbrooke.
## 
## (C) David J. Silkworth 2013
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
## GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, a copy is available at
##  http://www.r-project.org/Licenses/
##

PLpart<-function(Lam, t,r,O,TAT,ds,dLam)  {	
	pl<-Lam*(r*t+(1-r)*(O+EBO(ds,dLam,TAT)/dLam))
return(pl)	
}	
