## Testing on MacBook Air M1. Quarter-screen size for Plot tab of RStudio.

library(MBCbook) ## Only for imshow()
base_dim <- 1000
world <- lapply(1:100, \(x) matrix(runif(base_dim*base_dim), nrow = base_dim))


for(i in 1:10) {
  imshow(world[[i]])
  Sys.sleep(.05) ## Too fast for rendering
}

## From past exercise with Schelling Seggregation Model, I had this:
tf <- function(m) t(m)[, nrow(m):1]
imageM <- function(m, grid = max(dim(m)) <= 25, asp = (nrow(m)-1)/(ncol(m)-1), ...) {
  image(tf(m), asp=asp, axes = FALSE, useRaster=TRUE, ...)
}

t_start <- Sys.time()
for(i in 1:10) {
  imageM(world[[i]])
  Sys.sleep(.1) ## Renders, but runtime goes to 2.22secs for 100*100px. Not awful.
}
Sys.time() - t_start


t_start <- Sys.time()
for(i in 1:10) { ## Direct image call, not faster that out imageM for some reason
  image(world[[i]], axes=FALSE, useRaster = TRUE, col = grey.colors(256))
  Sys.sleep(.2) ## Renders, but runtime goes to 2.26secs. Not awful.
}
Sys.time() - t_start


#### Using intermediate file save to PNG:
for(i in 1:10) {
  png(filename=paste0("pictures/image_mat_raster",i,".png"))
  image(world[[i]], axes=FALSE, useRaster = TRUE)
  dev.off()
}

dev.off()

library(png)
t_start <- Sys.time()

for(i in 1:10) { ## NOT AT ALL FASTER!
  img <- readPNG(paste0("pictures/image_mat_raster",i,".png"))
  plot(1:2, type='n', xlab='', ylab='', axes=FALSE, asp=1)
  rasterImage(img, 1, 1, 2, 2)
  Sys.sleep(0.08) ## Nah. Does render, but at 2.4secs
}
Sys.time() - t_start


#### OK, how about with Python Matplotlib? 
library(reticulate)

np <- import("numpy")
plt <- import("matplotlib.pyplot")
py_world <- r_to_py(world)
py_world[[2]]

t_start <- Sys.time()
for(i in 1:10) { ## Well for a 1000*1000 matrix, NOT at all faster, actually
  plt$imshow(py_world[[i]])
  plt$axis("off")
  plt$show()
}
Sys.time() - t_start ## 3-4x slower, but then again, maybe it makes sense...


## Not in RStudio/so post-processing visualization, but can be made fast:
library(animation)
# Create the animation
saveHTML({ ## This one at least offers speed controls...
  for (i in 1:100) {
    image(world[[i]], axes=FALSE, useRaster = TRUE, col = grey.colors(256))
    ani.options(interval = 0.02)
  }
},
htmlfile = "random_points.html",
ani.width = 500,
ani.height = 500)
