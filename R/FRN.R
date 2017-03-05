## FRN.R
## This is a the function for calculating the Fill Rate Numerator using the poisson distribution
## for an initial stock level (j), demand rate (Lam), and average time to restore (t).
## This function is presented as equation 4.2 in "Statistical Theory of Reliability and Life Testing, 
## Probability Models" by Richard E. Barlow and Frank Proschan.
## 
## (C) David J. Silkworth 2014
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
FRN<-function(j,Lam,t)   {				
	if(j==0)  { 			
		thisNumerator<-0		
	}else{			
		thisNumerator<-Lam*ppois(j-1,Lam*t)		
	}			
	return(thisNumerator)			
}				
