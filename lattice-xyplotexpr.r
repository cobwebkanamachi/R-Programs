#This is how to use xyplot and update on lattice package. 
#output images and source are here
#http://rgm3.lab.nig.ac.jp/RGM/R_rdfile?f=lattice/man/xyplot.Rd&d=R_CC
#if you research how to use xyplot on lattice package, bellow.
#https://stat.ethz.ch/pipermail/r-help/attachments/20100404/5148512b/attachment.pl
#Take a look at
#http://CRAN.R-project.org/doc/FAQ/R-FAQ.html#Why-do-lattice_002ftrellis-graphics-not-work_003f
#https://stat.ethz.ch/R-manual/R-devel/library/lattice/html/xyplot.html
library(lattice)
require(stats)
old.options <- lattice.options(save.object = TRUE)
## Tonga Trench Earthquakes
Depth <- equal.count(quakes$depth, number=8, overlap=.1)
my0<-xyplot(lat ~ long | Depth, data = quakes)
print(my0)
print(.Last.value)
update(trellis.last.object(),
strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
                     par.strip.text = list(cex = 0.75),
                                         aspect = "iso")
#ex
trellis.last.object()
lattice.options(old.options)

## Constructing panel functions on the fly; prepane
EE <- equal.count(ethanol$E, number=9, overlap=1/4)
## Constructing panel functions on the fly; prepane
my1<-xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
	   xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
	   panel = function(x, y) {
	         panel.grid(h = -1, v = 2)
			 panel.xyplot(x, y)
			 panel.loess(x, y, span=1)
	   },
	   aspect = "xy")
print(my1)
## Extended formula interface
my2<-xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
   data = iris, scales = "free", layout = c(2, 2),
   auto.key = list(x = .6, y = .7, corner = c(0, 0)))
print(my2)
## user defined panel functions
states <- data.frame(state.x77,
     state.name = dimnames(state.x77)[[1]],
	 state.region = state.region)
my3<-xyplot(Murder ~ Population | state.region, data = states,
     groups = state.name,
	 panel = function(x, y, subscripts, groups) {
	     ltext(x = x, y = y, labels = groups[subscripts], cex=1,
		    fontfamily = "HersheySans")
	 })
print(my3)
## Stacked bar chart
my4<-barchart(yield ~ variety | site, data = barley,
	groups = year, layout = c(1,6), stack = TRUE,
	auto.key = list(space = "right"),
	ylab = "Barley Yield (bushels/acre)",
	scales = list(x = list(rot = 45)))
print(my4)
my5<-bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
print(my5)
my6<-dotplot(variety ~ yield | year * site, data=barley)
print(my6)
## Grouped dot plot showing anomaly at Morris
my7<-dotplot(variety ~ yield | site, data = barley, groups = year,
	key = simpleKey(levels(barley$year), space = "right"),
	xlab = "Barley Yield (bushels/acre) ",
	aspect=0.5, layout = c(1,6), ylab=NULL)
print(my7)
my8<-stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
	jitter.data = TRUE, xlab = "Height (inches)")
print(my8)
## Interaction Plot
#B_01_xyplot.ts
my9<-xyplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
	type = "a",
	auto.key =list(space = "right", points = FALSE, lines = TRUE))
print(my9)
## longer version with no x-ticks
## Not run:
my10<-bwplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
	panel = "panel.superpose",
	panel.groups = "panel.linejoin",
	xlab = "treatment",
	key = list(lines = Rows(trellis.par.get("superpose.line"),
			c(1:7, 1)),
			text = list(lab = as.character(unique(OrchardSprays$rowpos))),
			columns = 4, title = "Row position"))
print(my10)
## End(Not run)
