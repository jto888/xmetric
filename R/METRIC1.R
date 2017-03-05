## METRIC1.R
## This function performs the basic METRIC algorithm as presented in Chapter 3 of
##  "Optimal Inventory Modeling of Systems", by Craig C. Sherbrooke.
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

METRIC1<-function(x, convergence_limit=0.015, show=FALSE)  {					
		## A number of error trapping steps would be required to assure that the input			
		## dataframe is of expected format.  Development of this code is defered at this time.			
					
		## Identify the name label and enumerate the individual parts in the data input			
		Parts<-levels(factor(x$LRU))			
		## This is referenced frequently enough that it should just be a lookup			
		numParts<-length(Parts)			
					
		## create objects that will describe process later			
		base_priorities<-NULL			
		AllSelections<-list(NULL)			
					
	for(ipart in 1:numParts)  {				
					
				## y will now be the sub-set of InputData that pertains to this part	
				y<-x[sapply(x$LRU, function(z)  z==Parts[ipart]),]	
				## Must test to confirm that all parts are listed with same Cost	
				if(min(y$C)!=max(y$C))  {	
					stop("METRIC error:  Unequal part prices entered for part  ",as.character(y$LRU[1]) )
				}	
					
					
				## calcualate the demand on the depot 	
				dLam=0	
				for(j in 1:length(y[,1]))  {	
					dLam<-dLam + (1-y$Pbr[j])*y$bLam[j]
				}	
					
				## depot stock and part stock initalized at zero	
				ds=0	
				s=0	
				numBases<-length(y[,1])	
				PLz<-NULL	
				for(j in 1:numBases)  {	
					PLz<-c(PLz,PLpart(y$bLam[j],y$brT[j],y$Pbr[j],y$transp[j],y$dTAT[j],ds,dLam))
				}	
					
				##append PLz values to the InputTable for this part	
				PLzDF<-data.frame(PLz=PLz)	
				y<-cbind(y,PLzDF)	
					
				## order the priority of bases for part allocation, only if there is a distinction	
				if(min(PLz)!=max(PLz))   {	
					NDX<-order(PLz,decreasing=TRUE)
					y<-y[NDX,]
				}	
					
			## this isthe place to record the priority order of bases for parts (based on the PLz marginal analysis)		
			## requires all bases to be represented for all parts because I am building a matrix		
				these_bases<-y[,1]	
				base_priorities<-rbind(base_priorities,these_bases)	
					
					
				PLn<-PLz	
				EBOz<-sum(PLz)	
					
			## initialize the options list here		
			Options<-data.frame(ds=ds,bs=s-ds,EBO=EBOz,EBO_reduction=0,Cost=s*y$C[1])		
			last_EBO=EBOz		
					
				diagList<-list(EBOz)	
			Converged<-FALSE		
					
					
		while(Converged!=TRUE)  {			
			s<-s+1		
					
					
				ds<-s	
				## get the PL at this s level with all parts at depot PLds (zero parts at bases)	
				PLdsz<-NULL	
				for(j in 1:numBases)  {	
					PLdsz<-c(PLdsz,PLpart(y$bLam[j],y$brT[j],y$Pbr[j],y$transp[j],y$dTAT[j],ds,dLam))
				}	
				EBOds<-sum(PLdsz)	
				this_diag<-EBOds	
					
				PLn<-rbind(PLn,PLdsz)	
					
					
					
					
			NegTest<-FALSE		
			for(bs in 1:s)  {		
					thisBase<-(bs-1)%%numBases+1
					ds<-s-bs
					PrevQatBase<-as.integer((bs)/(numBases+1))
					
					
					
					thisEBOb<-EBO(PrevQatBase+1,PLn[ds+1,thisBase],1)
					lastEBOb<-EBO(PrevQatBase,PLn[ds+1,thisBase],1)
					deltaEBOb<-lastEBOb-thisEBOb
					
					
					
					diag_entry<-diagList[[s]][bs]-deltaEBOb
					this_diag<-c(this_diag,diag_entry)
					
			}		
					
					
			if(any(this_diag<0))  {		
				NegTest=TRUE	
				Converged=TRUE	
			}		
					
					
					
			if(NegTest==FALSE)  {		
				this_diag<-list(this_diag)	
				diagList<-c(diagList,this_diag)	
					
				## build the options list for this part now	
					## need min and its location to determine ds and bs
					this_EBO<-min(this_diag[[1]])
					this_num_bs<-match(this_EBO,this_diag[[1]])-1
					this_option<-data.frame(ds=s-this_num_bs,bs=this_num_bs,
					    EBO=this_EBO,EBO_reduction=last_EBO-this_EBO,Cost=s*y$C[1])
					Options<-rbind(Options,this_option)
					
					if((last_EBO-this_EBO)<convergence_limit)  {Converged=TRUE}
					last_EBO<-this_EBO
			}		
		}			
					
		## now identify the non-convex options for removal			
		best_reduction<-Options$EBO_reduction[length(Options[,1])]			
		Convexity<-c(1,rep(0,length(Options[,1])-2),1)			
		for(r in length(Options[,1]-1):2)  {			
			if (Options$EBO_reduction[r]>best_reduction)  {		
				best_reduction<-Options$EBO_reduction[r]	
				Convexity[r]<-1	
			}		
		}			
					
		Selections<-Options[sapply(Convexity, function(z)  z==1),]			
					
		## recalculate EBO reduction and calculate incremental cost for each selection (after elimination of non-convex lines)			
		## to finally calculate a marginal value for each Selection (marginal value = EBO_reduction/incremental cost)			
					
					
		IncrCost<-0			
		MarginalValue<-0			
		for(r in 2:length(Selections[,1]))  {			
			Selections$EBO_reduction[r]<-Selections$EBO[r-1]-Selections$EBO[r]		
			IncrCost<-c(IncrCost,Selections$Cost[r]-Selections$Cost[r-1])		
			MarginalValue<-c(MarginalValue,Selections$EBO_reduction[r]/IncrCost[r])		
		}			
		Part<-rep(ipart,length(Selections[,1]) )			
		IncrCost<-data.frame(IncrCost)			
		MarginalValue<-data.frame(MarginalValue)			
		Selections<-cbind(Part,Selections,IncrCost,MarginalValue)			
					
		Selections<-list(Selections)			
		AllSelections<-c(AllSelections,Selections)			
	## get next part				
	}				
					
	FinalSelections<-AllSelections[[2]][1,]				
	for(i in 2:length(AllSelections))  {				
		if(i>2)  {			
		FinalSelections$EBO[1]<-FinalSelections$EBO[1]+AllSelections[[i]][1,4]
		}		
		RemainingSelections<-AllSelections[[i]][-1,]			
		FinalSelections<-rbind(FinalSelections,RemainingSelections)			
	}				
					
	FinalSelections[1,1]<-NA				
	FinalSelections$MarginalValue[1]<-sum(FinalSelections$MarginalValue)				
	NDX<-order(FinalSelections$MarginalValue,decreasing=TRUE)				
	FinalSelections<-FinalSelections[NDX,]				
					
	## Now recalculate EBO and Cost at each line below 1				
	for(j in 2:length(FinalSelections[,1]))  {				
		FinalSelections$EBO[j]<-FinalSelections$EBO[j-1]-FinalSelections$EBO_reduction[j]			
		FinalSelections$Cost[j]<-FinalSelections$Cost[j-1]+FinalSelections$IncrCost[j]			
	}				
					
	if(show==TRUE)  {				
		plot(FinalSelections$Cost,FinalSelections$EBO,type="b")			
	}				
					
FinalSelections					
}					
