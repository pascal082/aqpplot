#
#                       U s e f u l   f o r  M a n u a l  D e b u g g i n g
#
# NOTE: When running locally, make sure to turn off the setwd call at the start of each routine.
#       and make sure that the file name location is one that you want.
#
# Running full plot for splined texture
# generateSplinedPlotTexture(sandClayStones@horizons$id, sandClayStones@horizons$name, sandClayStones@horizons$top, sandClayStones@horizons$bottom, sandClayStones@horizons$value, 'B', 0)
#
# Running shallow plot for splined texture
# generateSplinedPlotTexture(smallerSandClayStones$id, smallerSandClayStones$name, smallerSandClayStones$top, smallerSandClayStones$bottom, smallerSandClayStones$value, 'F', 0)
#
# Running full plot for splined water
#
# Running shallow plot for splined water
# generateSplinedPlotWater(smallerWaterValues$id, smallerWaterValues$name, smallerWaterValues$horizonTop, smallerWaterValues$horizonBottom, smallerWaterValues$value, 'F', 0)
#
# Running full plot for splined water
# generateSplinedPlotWater(waterValues$id, waterValues$name, waterValues$horizonTop, waterValues$horizonBottom, waterValues$value, 'B', 0)

#' @title     g e n e r a t e   A q p   P l o t
#' @description        g e n e r a t e   A q p   P l o t using the aqp package
#' @export

generateAqpPlot <- function (id, name, top, bottom, value) {

  siblingParams <- data.frame(id, name, top, bottom, value, stringsAsFactors=FALSE)
  aqp::depths(siblingParams) <- id ~ top + bottom
  PuOrColours <- c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#d8daeb",
                   "#b2abd2", "#8073ac", "#542788", "#2d004b" )
  colourRamp11 <- colorRampPalette(PuOrColours)(10)
  fileName <- tempfile(fileext='.png',tmpdir=tmpD)
  png(fileName, width=480,height=480,units="px",pointsize=12)
  par(mar=c(3.0, 0, 8.0, 0))
  plotSPC(siblingParams, name='name', color='value', col.label='Percent',
          col.palette=colourRamp11, axis.line.offset=-6, x.idx.offset=0.5, max.depth=100)
  # addVolumeFraction(siblingParams, 'stones')
  title(name, line=3.5, cex.main=1.6)
  dev.flush()
  dev.off()
  return(fileName)
}

#' @export
testPlot <- function(n){
   graphics::plot(n)
}

#' @title     g e n e r a t e   B l o c k   P l o t   W a t e r
#' @description   g e n e r a t e   B l o c k   P l o t   W a t e r via the aqp package
#' @export

generateBlockPlotWater <- function(id, name, top, bottom, value,runner=T) {
  fhNames <- name[id == 'Field capacity']
  numFhs <- length(fhNames)

  wpValues <- value[id == 'Wilting point']
  fcValues <- value[id == 'Field capacity']
  tpValues <- value[id == 'Total porosity']
  stonesValues <- value[id == 'Stones']
  nonStoneValueAdjustment <- (100 - stonesValues) / 100
  wpValues <- wpValues * nonStoneValueAdjustment;
  fcValues <- fcValues * nonStoneValueAdjustment;
  tpValues <- tpValues * nonStoneValueAdjustment;
  awValues <- fcValues - wpValues

  airValues <- tpValues - fcValues
  unavailValues <- wpValues
  earthValues <- 100 - tpValues - stonesValues
  stonesXmin <- rep(0, numFhs)
  stonesXmax <- stonesValues / 100.0
  earthXmin <- stonesXmax
  earthXmax <- (stonesValues + earthValues) / 100.0
  unavailXmin <- earthXmax
  unavailXmax <- (stonesValues + earthValues + unavailValues) / 100.0
  availXmin <- unavailXmax
  availXmax <- (stonesValues + earthValues + unavailValues + awValues) / 100.0
  airXmin <- availXmax
  airXmax <- rep(1.0, numFhs)
  plot.new()
  par(mar=c(2,1,6,2), xpd=TRUE, las=1, usr=c(0,1,1,0))
  fhBottoms <- 1 - (bottom[1:numFhs] / 100.0)
  fhTops <- 1 - (top[1:numFhs] / 100.0)
  stonesColour <- rgb(84,84,84,maxColorValue=255)
  earthColour <- rgb(128,64,64,maxColorValue=255)
  unavailColour <- rgb(255,128,64,maxColorValue=255)
  availColour <- rgb(0,128,255,maxColorValue=255)
  airColour <- rgb(234,234,234,maxColorValue=255)
  text(x=0.5,y=-0.15, labels=c("Water Retention"), font=2, cex=1.2)
  legend(0.35,-0.16,title="",legend=c("Stones","Fine earth", "Air"),
         horiz=TRUE,fill=c(stonesColour,earthColour,airColour), cex=0.8, bty="n", x.intersp=0.4)

  legend(0.33,-0.12,title="", legend=c("Unavailable water","Available water"),horiz=TRUE,
         fill=c(unavailColour,availColour, airColour), cex=0.8, bty="n", x.intersp=0.4)

  rect(xleft=stonesXmin,ybottom=fhBottoms,xright=stonesXmax,ytop=fhTops, col=stonesColour)
  rect(xleft=earthXmin,ybottom=fhBottoms,xright=earthXmax,ytop=fhTops, col=earthColour)
  rect(xleft=unavailXmin,ybottom=fhBottoms,xright=unavailXmax,ytop=fhTops, col=unavailColour)
  rect(xleft=availXmin,ybottom=fhBottoms,xright=availXmax,ytop=fhTops, col=availColour)
  rect(xleft=airXmin,ybottom=fhBottoms,xright=airXmax,ytop=fhTops,col=airColour)
  axis(4,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),labels=c('0 cm','10 cm', '20 cm', '30 cm','40 cm','50 cm',
                                                       '60 cm','70 cm','80 cm','90 cm', '100 cm'), tcl=-.2, hadj=0.6, padj=0.2, cex.axis=0.5)
  axis(1, at=c(0,0.25,0.5,0.75,1.00), labels=c("0%","25%","50%","75%","100%"), tcl=-.2, mgp=c(3,.1,.5),
       hadj=0.3, cex.axis=0.5)

  invisible()
}



#' @title     g e n e r a t e   S p l i n e d   P l o t   W a t e r
#' @description   Generate a water plot using splining.
#' @export


generateSplinedPlotWater <- function(id, name, top, bottom, value, rootBarrier, rootBarrierFHNum) {
  fhNames <- name[id == 'Field capacity']
  fhTops <- top[id == 'Field capacity']
  fhBottoms <- bottom[id == 'Field capacity']
  numFhs <- length(fhNames)
  bottom <- fhBottoms[numFhs]
  sideLength <- bottom + 1
  wpValues <- value[id == 'Wilting point']
  fcValues <- value[id == 'Field capacity']
  tpValues <- value[id == 'Total porosity']
  stonesValues <- value[id == 'Stones']
  nonStoneValueAdjustment <- (100 - stonesValues) / 100
  wpValues <- wpValues * nonStoneValueAdjustment;
  fcValues <- fcValues * nonStoneValueAdjustment;
  tpValues <- tpValues * nonStoneValueAdjustment;
  awValues <- fcValues - wpValues

  airValues <- tpValues - fcValues
  unavailValues <- wpValues
  earthValues <- 100 - tpValues - stonesValues
  earthValues <- earthValues + stonesValues
  unavailValues <- unavailValues + earthValues
  availValues <- awValues + unavailValues
  airValues <- airValues + availValues
  #library(aqp)
  stonesProf <- data.frame(id=rep("Stones",times=numFhs), name=fhNames, top=fhTops,
                           bottom=fhBottoms, value=stonesValues, stringsAsFactors=FALSE)
  aqp::depths(stonesProf) <- id ~ top + bottom
  earthProf <- data.frame(id=rep("Earth",times=numFhs), name=fhNames, top=fhTops,
                          bottom=fhBottoms, value=earthValues, stringsAsFactors=FALSE)
  aqp::depths(earthProf) <- id ~ top + bottom
  unavailProf <- data.frame(id=rep("Unavail",times=numFhs), names=fhNames, top=fhTops,
                            bottom=fhBottoms, value=unavailValues, stringsAsFactors=FALSE)
  aqp::depths(unavailProf) <- id ~ top + bottom
  availProf <- data.frame(id=rep("Avail",times=numFhs), names=fhNames, top=fhTops,bottom=fhBottoms, value=availValues, stringsAsFactors=FALSE)
  aqp::depths(availProf) <- id ~ top + bottom
  airProf <- data.frame(id=rep("Air",times=numFhs), names=fhNames,top=fhTops,
                        bottom=fhBottoms, value=airValues, stringsAsFactors=FALSE)
  aqp::depths(airProf) <- id ~ top + bottom

  stonesSpline <- GSIF::mpspline(stonesProf, var.name="value", vlow=min(stonesProf@horizons$value),
                           vhigh=max(stonesProf@horizons$value),lam=0.1)
  xPlotStones <- stonesSpline$var.1cm[!is.na(stonesSpline$var.1cm)]
  xPlotStones <- c(xPlotStones[1], xPlotStones)

  earthSpline <- GSIF::mpspline(earthProf, var.name="value", vlow=min(earthProf@horizons$value),
                          vhigh=max(earthProf@horizons$value),lam=0.1)
  xPlotEarth <- earthSpline$var.1cm[!is.na(earthSpline$var.1cm)]
  xPlotEarth <- c(xPlotEarth[1], xPlotEarth)

  unavailSpline <- GSIF::mpspline(unavailProf, var.name="value", vlow=min(unavailProf@horizons$value),
                            vhigh=max(unavailProf@horizons$value),lam=0.1)
  xPlotUnavail <- unavailSpline$var.1cm[!is.na(unavailSpline$var.1cm )]
  xPlotUnavail <- c(xPlotUnavail[1], xPlotUnavail)

  availSpline <- GSIF::mpspline(availProf, var.name="value", vlow=min(availProf@horizons$value),
                          vhigh=max(availProf@horizons$value),lam=0.1)
  xPlotAvail <- availSpline$var.1cm[!is.na(availSpline$var.1cm)]
  xPlotAvail <- c(xPlotAvail[1], xPlotAvail)

  yPlotPoints <- c(seq(0,bottom,1),seq(bottom,0,-1)) / 100.0
  stonesPolyXs <- generatePolygonXValues(rep(0.0,times=sideLength), xPlotStones)
  earthPolyXs <- generatePolygonXValues(xPlotStones,xPlotEarth)
  unavailPolyXs <- generatePolygonXValues(xPlotEarth,xPlotUnavail)
  availPolyXs <- generatePolygonXValues(xPlotUnavail,xPlotAvail)
  airPolyXs <- generatePolygonXValues(xPlotAvail, rep(100.0,times=sideLength))
  stonesColour <- rgb(84,84,84,maxColorValue=255)
  earthColour <- rgb(128,64,64,maxColorValue=255)
  unavailColour <- rgb(255,128,64,maxColorValue=255)
  availColour <- rgb(0,128,255,maxColorValue=255)
  airColour <- rgb(234,234,234,maxColorValue=255)

  plot.new()
  par(mar=c(2,1,6,2), xpd=TRUE, las=1, usr=c(0,1,1,0))

  text(x=0.5,y=-0.15, labels=c("Water Retention"), font=2, cex=1.2)
  legend(0.35,-0.16,title="",legend=c("Stones","Fine earth", "Air"),
         horiz=TRUE,fill=c(stonesColour,earthColour,airColour), cex=0.8, bty="n", x.intersp=0.4)

  legend(0.33,-0.12,title="", legend=c("Unavailable water","Available water"),horiz=TRUE,
         fill=c(unavailColour,availColour, airColour), cex=0.8, bty="n", x.intersp=0.4)

  axis(4,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),labels=c('0 cm','10 cm', '20 cm', '30 cm','40 cm','50 cm',
                                                       '60 cm','70 cm','80 cm','90 cm', '100 cm'), tcl=-.2, hadj=0.6, padj=0.2, cex.axis=0.5)
  axis(1, at=c(0,0.25,0.5,0.75,1.00), labels=c("0%","25%","50%","75%","100%"), tcl=-.2, mgp=c(3,.1,.5),
       hadj=0.3, cex.axis=0.5)

  polygon(stonesPolyXs, yPlotPoints, col=stonesColour)
  polygon(earthPolyXs, yPlotPoints, col=earthColour)
  polygon(unavailPolyXs, yPlotPoints, col=unavailColour)
  polygon(availPolyXs, yPlotPoints, col=availColour)
  polygon(airPolyXs, yPlotPoints, col=airColour)

  if(rootBarrier == 'F' || rootBarrier == 'M') {
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, col=stonesColour)
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, density=20, col="black")
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, density=20, angle=135, col="black")
    shadowtext(0.5, (1 + (bottom/100)) / 2, "Rock", col="black", bg=stonesColour, cex=0.5)
  }

  invisible()
}



#' @title     g e n e r a t e   S p l i n e d   P l o t   T e x t u r e
#' @description   Generate a texture plot using splining.
#' @export


generateSplinedPlotTexture <- function(id, name, top, bottom, value, rootBarrier, rootBarrierFHNum) {
  fhNames <- name[id == 'Clay']
  fhTops <- top[id == 'Clay']
  fhBottoms <- bottom[id == 'Clay']
  numFhs <- length(fhNames)
  bottom <- fhBottoms[numFhs]
  sideLength <- bottom + 1
  clayValues <- value[id == 'Clay']
  sandValues <- value[id == 'Sand']
  siltValues <- 100 - sandValues - clayValues
  stonesValues <- value[id == 'Stones']
  nonStoneValueAdjustment <- (100 - stonesValues) / 100
  clayValues <- clayValues * nonStoneValueAdjustment
  siltValues <- siltValues * nonStoneValueAdjustment
  sandValues <- sandValues * nonStoneValueAdjustment

  siltValues <- siltValues + clayValues
  sandValues <- sandValues + siltValues

  clayProf <- data.frame(id=rep("Clay",times=numFhs), name=fhNames, top=fhTops,
                         bottom=fhBottoms, value=clayValues, stringsAsFactors=FALSE)
  aqp::depths(clayProf) <- id ~ top + bottom
  siltProf <- data.frame(id=rep("Silt",times=numFhs), name=fhNames, top=fhTops,
                         bottom=fhBottoms, value=siltValues, stringsAsFactors=FALSE)
  aqp::depths(siltProf) <- id ~ top + bottom
  sandProf <- data.frame(id=rep("Sand",times=numFhs), names=fhNames, top=fhTops,
                         bottom=fhBottoms, value=sandValues, stringsAsFactors=FALSE)
  aqp::depths(sandProf) <- id ~ top + bottom

  claySpline <- GSIF::mpspline(clayProf, var.name="value", vlow=min(clayProf@horizons$value),
                         vhigh=max(clayProf@horizons$value),lam=0.1)

  xPlotClay <- claySpline$var.1cm[!is.na(claySpline$var.1cm)]
  xPlotClay <- c(xPlotClay[1], xPlotClay)

  siltSpline <- GSIF::mpspline(siltProf, var.name="value", vlow=min(siltProf@horizons$value),
                         vhigh=max(siltProf@horizons$value),lam=0.1)
  xPlotSilt <- siltSpline$var.1cm[!is.na(siltSpline$var.1cm)]
  xPlotSilt <- c(xPlotSilt[1], xPlotSilt)

  sandSpline <- GSIF::mpspline(sandProf, var.name="value", vlow=min(sandProf@horizons$value),
                         vhigh=max(sandProf@horizons$value),lam=0.1)
  xPlotSand <- sandSpline$var.1cm[!is.na(sandSpline$var.1cm)]
  xPlotSand <- c(xPlotSand[1], xPlotSand)

  yPlotPoints <- c(seq(0,bottom,1),seq(bottom,0,-1)) / 100.0

  clayPolyXs <- generatePolygonXValues(rep(0.0,times=sideLength), xPlotClay)
  siltPolyXs <- generatePolygonXValues(xPlotClay,xPlotSilt)
  sandPolyXs <- generatePolygonXValues(xPlotSilt,xPlotSand)
  stonePolyXs <- generatePolygonXValues(xPlotSand, rep(100.0,times=sideLength))
  siltColour <- rgb(255,184,113,maxColorValue=255)
  clayColour <- rgb(182,73,82,maxColorValue=255)
  sandColour <- rgb(211,236,155,maxColorValue=255)
  stonesColour <- rgb(128,128,128,maxColorValue=255)
  plot.new()
  par(mar=c(2,1,6,2), xpd=TRUE, las=1, usr=c(0,1,1,0), lwd=0.8)
  text(x=0.5,y=-0.092, labels=c("Texture"), font=2, cex=1.2)

  legend(x="top",title="",inset=c(0,-.12),legend=c("Clay","Silt","Sand","Stones"),horiz=TRUE,
         fill=c(clayColour,siltColour,sandColour, stonesColour), bty="n", cex=0.8, x.intersp=0.4)

  axis(4,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),labels=c('0 cm','10 cm', '20 cm', '30 cm','40 cm','50 cm',
                                                       '60 cm','70 cm','80 cm','90 cm', '100 cm'), tcl=-.2, hadj=0.6, padj=0.2, cex.axis=0.5)
  axis(1, at=c(0,0.25,0.5,0.75,1.00), labels=c("0%","25%","50%","75%","100%"), tcl=-.2, mgp=c(3,.1,.5),
       hadj=0.3, cex.axis=0.5)

  polygon(clayPolyXs, yPlotPoints, col=clayColour)
  polygon(siltPolyXs, yPlotPoints, col=siltColour)
  polygon(sandPolyXs, yPlotPoints, col=sandColour)
  polygon(stonePolyXs, yPlotPoints, col=stonesColour)
  if(rootBarrier == 'F' || rootBarrier == 'M') {
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, col=stonesColour)
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, density=20, col="black")
    rect(xleft=0,ybottom=1.0,xright=1.0,ytop=bottom/100.0, density=20, angle=135, col="black")
    shadowtext(0.5, (1 + (bottom/100)) / 2, "Rock", col="black", bg=stonesColour, cex=0.5)
  }

  invisible()
}




#' @title     g e n e r a t e   B l o c k   P l o t   T e x t u r e
#' @description   Generate a texture block plot using aqp package.
#' @export

generateBlockPlotTexture <- function(id, name, top, bottom, value) {
  numFhs <- length(name[id == 'Clay'])
  clayValues <- value[id == 'Clay']
  sandValues <- value[id == 'Sand']
  siltValues <- 100 - sandValues - clayValues
  stonesValues <- value[id == 'Stones']
  nonStoneValueAdjustment <- (100 - stonesValues) / 100
  clayValues <- clayValues * nonStoneValueAdjustment
  sandValues <- sandValues * nonStoneValueAdjustment
  siltValues <- siltValues * nonStoneValueAdjustment
  clayXmin <- rep(0, numFhs)
  clayXmax <- clayValues / 100.0
  siltXmin <- clayXmax
  siltXmax <- (clayValues + siltValues) / 100.0
  sandXmin <- siltXmax
  sandXmax <- (sandValues + siltValues + clayValues) / 100.0
  stonesXmin <- sandXmax
  stonesXmax <- rep(1.0, numFhs)
  plot.new()
  par(mar=c(2,1,6,2), xpd=TRUE, las=1, usr=c(0,1,1,0))
  fhBottoms <- (bottom[1:numFhs] / 100.0)
  fhTops <- (top[1:numFhs] / 100.0)
  siltColour <- rgb(255,184,113,maxColorValue=255)
  clayColour <- rgb(182,73,82,maxColorValue=255)
  sandColour <- rgb(211,236,155,maxColorValue=255)
  stonesColour <- rgb(128,128,128,maxColorValue=255)
  text(x=0.5,y=-0.092, labels=c("Texture"), font=2, cex=1.2)

  legend(x="top",title="",inset=c(0,-.12),legend=c("Clay","Silt","Sand","Stones"),horiz=TRUE,
         fill=c(clayColour,siltColour,sandColour, stonesColour), bty="n", cex=0.8, x.intersp=0.4)

  rect(xleft=sandXmin,ybottom=fhBottoms,xright=sandXmax,ytop=fhTops, col=sandColour)
  rect(xleft=siltXmin,ybottom=fhBottoms,xright=siltXmax,ytop=fhTops, col=siltColour)
  rect(xleft=clayXmin,ybottom=fhBottoms,xright=clayXmax,ytop=fhTops, col=clayColour)
  rect(xleft=stonesXmin,ybottom=fhBottoms,xright=stonesXmax,ytop=fhTops,col=stonesColour)
  axis(4,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),labels=c('0 cm','10 cm', '20 cm', '30 cm','40 cm','50 cm',
                                                       '60 cm','70 cm','80 cm','90 cm', '100 cm'), tcl=-.2, hadj=0.6, padj=0.2, cex.axis=0.5)
  axis(4,at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),labels=c('0 cm','10 cm', '20 cm', '30 cm','40 cm','50 cm',
                                                       '60 cm','70 cm','80 cm','90 cm', '100 cm'), tcl=-.2, hadj=0.6, padj=0.2, cex.axis=0.4, lwd.ticks=0.8)
  axis(1, at=c(0,0.25,0.5,0.75,1.00), labels=c("0%","25%","50%","75%","100%"), tcl=-.2, mgp=c(3,.1,.5),
       hadj=0.3, cex.axis=0.5)

  invisible()
}




######################################## HELPER FUNCTIONS ##################################################


#
#                  g e n e r a t e   P o l y g o n   X   V a l u e s
#
# A helper function to combine a left hand side spline, with a right hand side spline, so that
# they are merged into a polygon.  Make sure to clip the polygon at 0 and 100.
#

generatePolygonXValues <- function(leftSideX, rightSideX, yValues) {
  # first do a sanity check.  No X points can be less than 0 or greater than 100
  leftSideX[leftSideX < 0] <- 0
  leftSideX[leftSideX > 100] <- 100.0
  rightSideX[rightSideX < 0] <- 0
  rightSideX[rightSideX > 100] <- 100.0
  # now join up the left and right side
  xValues <- c(leftSideX,rev(rightSideX)) / 100.0
  return(xValues)
}



#
#                        s h a d o w   t e x t
#
# A helper routine to provide a shadow around drawn text.
#
shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
                       theta= seq(0, 2*pi, length.out=50), r=0.1,  ... ) {

  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')

  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}


#
#                   r e t r i e v e   I m a g e
#
# Retrieve the named image with the specified file size.
#
retrieveImage <- function(filename) {
  fileSize <- file.info(filename)$size
  print(filename)
  print(fileSize)
  #  setwd("/home/users/cuthillt/SoilDiagramImages")
  blob = readBin(filename, 'raw', fileSize)
  unlink(filename)
  return(blob)
}

