
# Reference:  https://cran.r-project.org/web/packages/hazus/vignettes/

library(hazus)
library(reshape2)
library(ggplot2)

fl_dept <- extract_hazus_functions()
gfx_data <- subset(fl_dept, grepl("one floor", Description) & Cover_Class == "Bldg")
gfx_data$Description <- paste(gfx_data$DmgFnId, gfx_data$Description)
gfx_line <- ggplot(data = gfx_data, aes(x = depth, y = damage))
gfx_line <- gfx_line + geom_line(aes(colour = Description))
gfx_line <- gfx_line + ylab("Damage (%)") + xlab("Flood Depth (ft)")
print(gfx_line)


