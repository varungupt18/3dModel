library(rgl)
library(magick)
baseball_df <- read.csv("mlb-player-stats-Batters.csv")


#Intializing RGL Device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) {
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

#Basic Graph
rgl_init()
x <- baseball_df$R
y <- baseball_df$RBI
z <- baseball_df$HR
rgl.points(x, y, z, color ="#20B2AA")
rgl.bbox(color = "#FFFAFA")



#Sphere Graph
rgl_init()
rgl.spheres(x, y, z, r = 0.75, color = "yellow")  
rgl.bbox(color = "#333377")

#Multi-color Spheres (Team)
colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col))
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

rgl_init()
rgl.spheres(x, y, z, r = 0.75,
            color = colors(baseball_df$Team))
rgl.bbox(color = "#333377")
aspect3d(1,1,1)
#Creating 3D Movie
movie3d(spin3d(axis = c(0, -1, 0)), duration = 5,
        dir = getwd())