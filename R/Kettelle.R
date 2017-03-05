## Kettelle.R
## This function performs the Generalized Kettelle Algorithm as presented in 
## Chapter 7 of "Statistical Theory of Reliability and Life Testing, Probability Models"
##  by Richard E. Barlow and Frank Proschan.  This function has been extended to handle
##  performance optimization of Sherbrooke's Estimated Back Order measure.
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

Kettelle<-function(x, limit=1e-4, data.name="", performance="FRN", show=FALSE)   {								
	numParts<-length(x[,1])								
		PerfFun=FRN							
	if(performance=="EBO")   {								
		PerfFun=EBO							
	}								
	UdomAll<-matrix(data=rep(0,numParts+2),								
		nrow=1, ncol=numParts+2)							
									
	UdomAll<-data.frame(UdomAll)								
		PartNames<-NULL							
	## build lists of individual part Perf and Cost values per Qty "s"								
	## this can be referenced rather than calculated each time								
	PerfList<-NULL								
	CostList<-NULL								
	## required to zero-for-other-parts, debug fix								
	z4op=0								
	for (n in 1:numParts)  {								
			s=0						
			withinBudget=TRUE						
			unconverged=TRUE						
			thisPerf<-NULL						
			thisCost<-NULL						
		while(unconverged)  {							
			newPerf<-PerfFun(s,x[n,2],x[n,3])						
			thisPerf<-c(thisPerf,newPerf)						
			thisCost<-c(thisCost,s*x$C[n])						
			withinBudget <-TRUE						
									
			if(s>2)  {						
				unconverged<-abs(newPerf-lastPerf)>limit					
			}else{						
				unconverged=TRUE					
			}						
			lastPerf<-newPerf						
									
			if (s==0) {						
				z4op<-z4op+newPerf					
			}						
			s=s+1						
		}							
		PerfList[[length(PerfList)+1]]<-thisPerf							
		CostList[[length(CostList)+1]]<-thisCost							
		PartNames<-c(PartNames,as.character(x[n,1]))							
									
	}								
	names(UdomAll)<-c(PartNames,"Perf","Cost")								
	ExAll<-UdomAll								
	thisRow<-UdomAll								
									
	for(j in 1:(numParts-1) )   {								
		z4op<-c(z4op,z4op[j]-PerfList[[j]][1])							
	}								
									
	j=1								
	UdomAll$Perf[j]<-z4op[j]								
									
	## Fill the UdomAll for the first part with no others								
	for(k in 2:length(CostList[[j]])-1)   {								
		thisRow[1,1]=k							
		for(f in 2:numParts)  {							
			thisRow[1,f]=0						
		}							
		thisRow$Perf<-PerfList[[j]][k+1]+z4op[j+1]							
		thisRow$Cost<-CostList[[j]][k+1]							
		UdomAll<-rbind(UdomAll,thisRow)							
									
	}								
	UdomLast<-UdomAll								
									
									
									
	if(UdomAll$Perf[1]<UdomAll$Perf[2])    {								
		##  This is the maximize algorithm							
		for(f in 2:(numParts))   {							
			## corrected to start from all zeros, else zrro part 1 cases are never evaluated						
			for(j in 1:length(UdomLast[,1]))  {						
									
				for(k in 1:length(CostList[[f]]))   {					
					thisRow<-UdomLast[j,]				
					thisRow[1,f]<-k-1				
					thisRow$Cost[1]<-thisRow$Cost[1]+CostList[[f]][k]				
			## Subtract the zero Perf for this part f, then add back the Perf for k-1 Qty of part f						
					thisRow$Perf[1]<-thisRow$Perf[1]-PerfList[[f]][1]+PerfList[[f]][k]				
					if(thisRow$Cost[1] %in% UdomAll$Cost)   {				
						pos<-match(thisRow$Cost[1],UdomAll$Cost)			
						if(UdomAll$Perf[pos]<thisRow$Perf[1])  {			
							ex<-UdomAll[pos,]		
							UdomAll[pos,]<-thisRow		
							ExAll<-rbind(ExAll,ex)		
			##  Does this undominated allocation dominate anything elseof higher cost?						
							if(max(UdomAll$Cost)>thisRow$Cost)  {		
							majorDom<-UdomAll[(pos+1):length(UdomAll[,1]),]		
								if(min(majorDom$Perf)<thisRow$Perf)  {	
									ex<-majorDom[sapply(majorDom$Perf, function(x) x<thisRow$Perf),]
									ExAll<-rbind(ExAll,ex)
									majorDom<-majorDom[sapply(majorDom$Perf, function(x) x>thisRow$Perf),]
									UdomAll<-rbind(UdomAll[1:pos,],majorDom)
								}	
							}		
						}else{			
			## this allocation is dominated by a previous allocation at this Cost						
							ExAll<-rbind(ExAll,thisRow)		
						}			
					}else{				
			## this is where additional activity must take place						
			## need to check if this allocation is dominated by a lower cost						
			## need to error trap here for case where this is a new high Cost						
						if(max(UdomAll$Cost)>thisRow$Cost)  {			
							pos<-min(which(UdomAll$Cost>thisRow$Cost))		
							minorDom<-UdomAll[1:(pos-1),]		
							if(max(minorDom$Perf)>thisRow$Perf)  {		
			## this is a new Cost allocation that is dominated by a lesser cost allocation						
								ExAll<-rbind(ExAll,thisRow)	
							}else{		
			## if not, it is dominant, but did it dominate something elseof higher cost?						
			##  Does this newly identified Udom alloccation dominate anything elseof higher cost?						
								majorDom<-UdomAll[pos:length(UdomAll[,1]),]	
								if(min(majorDom$Perf)<thisRow$Perf)  {	
									ex<-majorDom[sapply(majorDom$Perf, function(x) x<thisRow$Perf),]
									majorDom<-majorDom[sapply(majorDom$Perf, function(x) x>thisRow$Perf),]
									ExAll<-rbind(ExAll,ex)
								}	
								UdomAll<-rbind(UdomAll[1:(pos-1),],thisRow,majorDom)	
								## it is impossible to state a domination over anything at lower cost	
							}		
						}else{			
			## A new high cost allocation has been defined, so just add it to the end of table						
							UdomAll<-rbind(UdomAll,thisRow)		
						}			
					}				
				## close incremental new part					
				}					
			## close UdomLast						
			}						
			UdomLast<-UdomAll						
		## close all parts							
		}							
									
	}else{								
		##  This is the minimize algorithm							
		for(f in 2:(numParts))   {							
			## corrected to start from all zeros, else zrro part 1 cases are never evaluated						
			for(j in 1:length(UdomLast[,1]))  {						
									
				for(k in 1:length(CostList[[f]]))   {					
					thisRow<-UdomLast[j,]				
					thisRow[1,f]<-k-1				
					thisRow$Cost[1]<-thisRow$Cost[1]+CostList[[f]][k]				
			## Subtract the zero Perf for this part f, then add back the Perf for k-1 Qty of part f						
					thisRow$Perf[1]<-thisRow$Perf[1]-PerfList[[f]][1]+PerfList[[f]][k]				
					if(thisRow$Cost[1] %in% UdomAll$Cost)   {				
						pos<-match(thisRow$Cost[1],UdomAll$Cost)			
						if(UdomAll$Perf[pos]>thisRow$Perf[1])  {			
							ex<-UdomAll[pos,]		
							UdomAll[pos,]<-thisRow		
							ExAll<-rbind(ExAll,ex)		
			##  Does this undominated allocation dominate anything elseof higher cost?						
							if(max(UdomAll$Cost)>thisRow$Cost)  {		
							majorDom<-UdomAll[(pos+1):length(UdomAll[,1]),]		
								if(max(majorDom$Perf)>thisRow$Perf)  {	
									ex<-majorDom[sapply(majorDom$Perf, function(x) x>thisRow$Perf),]
									ExAll<-rbind(ExAll,ex)
									majorDom<-majorDom[sapply(majorDom$Perf, function(x) x<thisRow$Perf),]
									UdomAll<-rbind(UdomAll[1:pos,],majorDom)
								}	
							}		
						}else{			
			## this allocation is dominated by a previous allocation at this Cost						
							ExAll<-rbind(ExAll,thisRow)		
						}			
					}else{				
			## this is where additional activity must take place						
			## need to check if this allocation is dominated by a lower cost						
			## need to error trap here for case where this is a new high Cost						
						if(max(UdomAll$Cost)>thisRow$Cost)  {			
							pos<-min(which(UdomAll$Cost>thisRow$Cost))		
							minorDom<-UdomAll[1:(pos-1),]		
							if(min(minorDom$Perf)<thisRow$Perf)  {		
			## this is a new Cost allocation that is dominated by a lesser cost allocation						
								ExAll<-rbind(ExAll,thisRow)	
							}else{		
			## if not, it is dominant, but did it dominate something elseof higher cost?						
			##  Does this newly identified Udom alloccation dominate anything elseof higher cost?						
								majorDom<-UdomAll[pos:length(UdomAll[,1]),]	
								if(max(majorDom$Perf)>thisRow$Perf)  {	
									ex<-majorDom[sapply(majorDom$Perf, function(x) x>thisRow$Perf),]
									majorDom<-majorDom[sapply(majorDom$Perf, function(x) x<thisRow$Perf),]
									ExAll<-rbind(ExAll,ex)
								}	
								UdomAll<-rbind(UdomAll[1:(pos-1),],thisRow,majorDom)	
								## it is impossible to state a domination over anything at lower cost	
							}		
						}else{			
			## A new high cost allocation has been defined, so just add it to the end of table						
							UdomAll<-rbind(UdomAll,thisRow)		
						}			
					}				
				## close incremental new part					
				}					
			## close UdomLast						
			}						
			UdomLast<-UdomAll						
		## close all parts							
		}							
									
	}								
									
	title<-"Kettelle Part Allocation Optimization"								
									
	if(data.name!="") {								
		title<-c(title, data.name)							
	}								
									
	if(performance=="Fill Rate")   {								
		ExAll$Perf<-ExAll$Perf/sum(x[,2])							
		UdomAll$Perf<-UdomAll$Perf/sum(x[,2])							
	  }else{								
		if(performance!="EBO")  {							
	## ignore any stray argument names for performance								
	## "FRN" is default								
			performance="FRN"						
		}							
	}								
									
	outnames<-names(UdomAll)								
	outcols<-length(outnames)								
	outnames[outcols-1]<-performance								
	names(UdomAll)<-outnames								
									
	if(show==TRUE)  {								
		plot(ExAll$Cost,ExAll$Perf,							
		xlab="Cost",ylab=performance,							
		pch=19, col="blue", cex=0.5,							
		main=title							
									
		)							
		points(UdomAll$Cost,UdomAll[,outcols-1],							
		pch=21, bg="red")							
	}								
									
return(UdomAll)									
}	