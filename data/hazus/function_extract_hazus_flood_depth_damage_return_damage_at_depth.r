
get_hazus_damage_function_return_damage_at_depth = function (damage_function_id, depth) {

# Extract and plot specific functions based on subset selection values below.
# fl_dept is loaded in server.R.
#fl_dept <- extract_hazus_functions()
gfx_data <- subset(fl_dept, grepl(damage_function_id, DmgFnId) & Cover_Class == "Bldg")
gfx_data$Description <- paste(gfx_data$DmgFnId, gfx_data$Description)

# Convert depth from feet to meters.
gfx_data$depth = 0.3048 * gfx_data$depth

# Fit linear model to the selected data.
#model <- lm(gfx_data$damage ~ poly(gfx_data$depth,3,raw=TRUE))

# Function coefficients are intercept and the cofficient of each power of t from 1 to 3.
#f4damage_flood_depth_lm = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t + summary(model)$coefficients[3,1]*t^2 + summary(model)$coefficients[4,1]*t^3 }

# Better methods than lm are approxfun and splinefun.
# approxfun returns function performing linear interpolation between each data point.
# splinefun does spline fit to the data, but can insert nonlinearities between points that may not be desired.
# Comparison plot.
f2 = approxfun(gfx_data$depth, gfx_data$damage)
#f3 = splinefun(gfx_data$depth, gfx_data$damage)
#curve(f2(x),-4,24 , col = "black")
#par(new=TRUE)
#curve(f3(x),-4,24 , col = "green")
#par(new=TRUE)
#points(gfx_data$depth, gfx_data$damage) 

# Select the approxfun version
f4damage_flood_depth = f2
return(f4damage_flood_depth(depth))

}
