## EBO.R
## This is a an initial function for calculating the Estimated Back Orders assuming a poisson distribution
## for demand given a an initial stock level (j), demand rate (Lam), and average time to restore (t).
## This function is presented as equation 2.7 in "Optimal Inventory Modeling of Systems", by Craig C. Sherbrooke.
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

EBO<-function(j,Lam,t)   {	
	ebo<-(Lam*t)*dpois(j,(Lam*t))+((Lam*t)-j)*(1-ppois(j,(Lam*t)))
return(ebo)	
}	
