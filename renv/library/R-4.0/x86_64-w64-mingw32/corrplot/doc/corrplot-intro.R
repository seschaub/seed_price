## ----setup, include=FALSE-----------------------------------------------------
set.seed(0) # we need reproducible results
knitr::opts_chunk$set(
  out.extra = 'style="display:block; margin: auto"',
  fig.align = "center",
  fig.path = "webimg/",
  fig.width = 6,
  fig.height = 6,
  dev = "png")

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
if(get_os() =='windows' & capabilities('cairo')){
  knitr::opts_chunk$set(dev.args = list(type="cairo"))
}

## ----methods------------------------------------------------------------------
library(corrplot)
M <- cor(mtcars)
corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "ellipse")
corrplot(M, method = "number") # Display the correlation coefficient
corrplot(M, method = "shade")
corrplot(M, method = "color")
corrplot(M, method = "pie")

## ----layout-------------------------------------------------------------------
corrplot(M, type = "lower")

## ----mixed--------------------------------------------------------------------
corrplot.mixed(M)
corrplot.mixed(M, lower.col = "black", number.cex = .7)
corrplot.mixed(M, lower = "square", upper = "circle", tl.col = "black")

## ----order--------------------------------------------------------------------
corrplot(M, order = "AOE")
corrplot(M, order = "hclust")
corrplot(M, order = "FPC")

## ----rectangles---------------------------------------------------------------
corrplot(M, order = "hclust", addrect = 3)

## ----hclust-lightblue---------------------------------------------------------
# Change background color to lightblue
corrplot(M, type = "upper", order = "hclust", cl.pos = "n",
         col = c("black", "white"), bg = "lightblue")

## ----color--------------------------------------------------------------------
## color example, suitable for matrix in [-N, 0] or [0, N]
col0 <- colorRampPalette(c("white", "cyan", "#007FFF", "blue","#00007F"))

## diverging color example, suitable for matrix in [-N, N]
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))	
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")

## using these color spectra
corrplot(M, order = "hclust", addrect = 2, col = col1(100))
corrplot(M, order = "hclust", addrect = 2, col = col2(50))
corrplot(M, order = "hclust", addrect = 2, col = col3(20))
corrplot(M, order = "hclust", addrect = 2, col = col4(10))
corrplot(M, order = "hclust", addrect = 2, col = whiteblack, bg = "gold2")

## ----hclust-stdcolors---------------------------------------------------------
corrplot(M, order = "hclust", addrect = 2, col = cm.colors(100))

## ----hclust-rcolorbrewer------------------------------------------------------
library(RColorBrewer)

corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 8, name = "RdBu"))

## ----color-label--------------------------------------------------------------
## remove color legend, text legend and principal diagonal glyph
corrplot(M, order = "AOE", cl.pos = "n", tl.pos = "n", diag = FALSE)  

## bottom  color legend, diagonal text legend, rotate text label
corrplot(M, order = "AOE", cl.pos = "b", tl.pos = "d")

## a wider color legend with numbers right aligned
corrplot(M, order = "AOE", cl.ratio = 0.2, cl.align = "r")

## text labels rotated 45 degrees
corrplot(M, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

## ----cl_lim-------------------------------------------------------------------
# when is.corr=TRUE, cl.lim only affect the color legend
# If you change it, the color on matrix is still assigned on [-1, 1]
corrplot(M/2)
corrplot(M/2, cl.lim=c(-0.5,0.5))

# when is.corr=FALSE, cl.lim is also used to assign colors
# if the matrix have both positive and negative values
# the matrix transformation keep every values positive and negative
corrplot(M*2, is.corr = FALSE, cl.lim=c(-2, 2))

## ----non-corr-----------------------------------------------------------------
# for showing the usage of cl.lim, run with warning
corrplot(M*2, is.corr = FALSE, cl.lim=c(-2, 2) * 2)

## matrix in [50, 60]
corrplot(abs(M)*10+50, is.corr = FALSE, cl.lim=c(50, 60), col=col0(10))

## ----non-corr-asp-------------------------------------------------------------
ran <- matrix(rnorm(70), ncol = 7)
corrplot(ran, is.corr = FALSE, win.asp = .7, method = "circle")

## ----NAs----------------------------------------------------------------------
M2 <- M
diag(M2) = NA
corrplot(M2)
corrplot(M2, na.label = "o")
corrplot(M2, na.label = "NA")

## ----plotmath-----------------------------------------------------------------
M2 <- M[1:5,1:5]
colnames(M2) <- c("alpha", "beta", ":alpha+beta", ":alpha[0]", "=alpha[beta]")
rownames(M2) <- c("$alpha", "$beta", NA, "$alpha[0]", "$alpha[beta]")
corrplot(M2)

## ----test---------------------------------------------------------------------
res1 <- cor.mtest(mtcars, conf.level = .95)
res2 <- cor.mtest(mtcars, conf.level = .99)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .2)
corrplot(M, p.mat = res1$p, sig.level = .05)
corrplot(M, p.mat = res1$p, sig.level = .01)

## leave blank on no significant coefficient
corrplot(M, p.mat = res1$p, insig = "blank")

## add p-values on no significant coefficient
corrplot(M, p.mat = res1$p, insig = "p-value")

## add all p-values
corrplot(M, p.mat = res1$p, insig = "p-value", sig.level = -1)

## add cross on no significant coefficient 
corrplot(M, p.mat = res1$p, order = "hclust", insig = "pch", addrect = 3)

## ----ci-----------------------------------------------------------------------
corrplot(M, low = res1$lowCI, upp = res1$uppCI, order = "hclust",
         rect.col = "navy", plotC = "rect", cl.pos = "n")
corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         order = "hclust", pch.col = "red", sig.level = 0.01,
         addrect = 3, rect.col = "navy", plotC = "rect", cl.pos = "n")

## ----ci_with_label------------------------------------------------------------
res1 <- cor.mtest(mtcars, conf.level = .95)

corrplot(M, p.mat = res1$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white")
corrplot(M, p.mat = res1$p, method = "color",
         insig = "label_sig", pch.col = "white")
corrplot(M, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01, .05), pch.cex = .9,
         insig = "label_sig", pch.col = "white", order = "AOE")
corrplot(M, p.mat = res1$p, insig = "label_sig", pch.col = "white",
         pch = "p<.05", pch.cex = .5, order = "AOE")

