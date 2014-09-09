col.bar <- function(breaks,horiz=TRUE,v=1,h=1,col=col,cex=2,type="r",verbose=FALSE,vl=0.5) {
     
  xleft <- par()$usr[1] 
  xright <- par()$usr[2]
  ybottom <- par()$usr[4] - 1 - h
  ytop <-  par()$usr[4] - 1 
  ## browser()
  steps <-   seq(0, (xright -xleft - v * (length(col))) , (xright - xleft - v * (length(col)))/(length(breaks))) # 
  nsteps <- length(steps) 

  if (verbose) print(steps)
  if (verbose) print(breaks)
  if (verbose) print(nsteps)

  if (max(abs(breaks))<=1) breaks <- round(breaks,digits=2)
  
  k <- 0
  for (i in 1 :(nsteps-2)) {  
      if (!is.null(v)) 
          if (i == 1) k <- v/2 else k <- k + v  
      if (type == "r") { ## "r" for rectangle
          rect(xleft= k  + xleft + steps[i] ,xright= k + xleft + steps[i+1],ybottom=ybottom,ytop=ytop,col=col[i])
          
          ## text(x = k + xleft + steps[i], y = ybottom - 1,labels=sprintf("%.1f",icn[i]),cex=cex)
      }
      else if (type == "p") { ## "p" points
          points(x= k + xleft + (steps[i]+ steps[i+1])/2, y=(ybottom + ytop)/2,pch=21, bg=col[i],cex=v)

      }
      
      text(x = k + xleft + (steps[i]+ steps[i+1])/2,  y = ybottom - vl, labels=levels(cut(breaks,breaks))[i],cex=cex)
  }
  
}
