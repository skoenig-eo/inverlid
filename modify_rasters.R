library(raster)

# Cover
full <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Cover_full.tif")
plot(full)
full[full>1] <- NA
full[full<0] <- NA

back <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Cover_low_resampled.tif")
back[back>1] <- NA
back[back<0] <- NA

filled <- raster::merge(full, back)
writeRaster(filled,
            "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Cover_full_filled.tif",
            overwrite = TRUE)

# Shrub
full <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Shrub_full.tif")
plot(full)
full[full>1] <- NA
full[full<0] <- NA

back <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Shrub_low_resampled.tif")
back[back>1] <- NA
back[back<0] <- NA

filled <- raster::merge(full, back)
writeRaster(filled,
            "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Shrub_full_filled.tif",
            overwrite = TRUE)

# Under
full <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Under_full.tif")
plot(full)
full[full>1] <- NA
full[full<0] <- NA

back <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Under_low_resampled.tif")
back[back>1] <- NA
back[back<0] <- NA

filled <- raster::merge(full, back)
writeRaster(filled,
            "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Under_full_filled.tif",
            overwrite = TRUE)