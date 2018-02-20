
customHeatMap <- function (shapefile, targetVar,
                     location ="deutschland", zoom = 5, maptype = "hybrid",
                     difColors = 10,colLow, colMid, colHigh,
                     title
                     ) {


colLow = "#FFFFFF"
## --------------------------Define Settings--------------------------- ##
#set parameters for heatmap
#heatcolors from green to red (from cheap to expensive)
difColors = difColors
heatColors = colorRampPalette(c(as.character(colLow),as.character(colMid), as.character(colHigh)))(difColors) #
#get satellite map of moscow
customMap = get_map(location = location, zoom = zoom,
                    maptype = maptype,
                    color = "color")
#define a grid for the heatmap
grid = list("grid.raster", customMap,
            x = mean(position["x",]),
            y = mean(position["y",]),
            width = position["x",2]-position["x",1],
            height = position["y",2]-position["y",1],
            default.units = "native", first = TRUE)

## --------------------------Plot Heatmap--------------------------- ##
#plot the heatmap
spplot(shapefile, c(as.character(targetVar)), sp.layout = grid, main = as.character(title), col.regions = heatColors,
       sub = "", cuts = difColors-1, col = "black", lwd = 1)

print.warnings("Make sure that both layers of the displayed map show the same geographical location. If not edit the zoom argument for valid presentation. Hint: A change to maptype = 'terrain' can be helpful")


}

#custom error message, if data is not in an appropriate structure
strCheck = function(dataframe) {

  for(i in 1:length(colnames(dataframe))) {
    if (colnames(dataframe)[i] %in% colnames(dataframe)[-i]) {
      warning("The provided data shows redundant information. Please remove remove non-required information to prevent misunderstandings. Column number ", i , " is part of the problem.")
    } else {
      message("The structure of column number ", i , " is fine. Continue.")
    }
  }

}


