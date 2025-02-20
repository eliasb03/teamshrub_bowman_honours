
install.packages("daymetr")
library("daymetr")

latitude <- 69.571161
longitude <- -138.902650
latitude <- 50
longitude <- -100

test <- download_daymet_tiles(location = c(latitude,longitude),
                      start = 1980,
                      end = 2024,
                      param = c("tmin", "tmax"))
