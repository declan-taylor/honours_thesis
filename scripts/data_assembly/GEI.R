# RGB data extraction program, based on code from Alison Leslie Beamish (2011).
# FYI: MEAD_CTL_14 DOY 206 is missing: subbed in DOY 209.

# Loading rgdal generated "warnings of possible GDAL/OSR exportToProj4() 
# degradation". Note that by Dec 2023 rgdal will likely be retired and functions 
# in `terra` will need to be substituted.

# directory = here("data/greenness/cropped_images")
generate_GEI <- function(directory){
  # get the files in the directory and their creation dates and load them into a dataframe 
  files <- list.files(directory, pattern="\\.tif$", full.names=TRUE)
  photos <- data.frame(file=I(files), 
                   site=NA, plot=NA, treatment=NA, doy=NA,
                   Av_R=NA, Av_G=NA, Av_B=NA, 
                   Index=NA, Index2=NA, Index3=NA,
                   GEI=NA)
  
  for (i in 1:nrow(photos) ){
    # Labeling the row
    photos[i, 'site'] <- str_extract(photos[i, 'file'], "(?<=2022\\_)[A-Z]{4,5}(?=\\_)")
    photos[i, 'treatment'] <- str_extract(photos[i, 'file'], "(?<=[A-Z]{4,5}\\_)[A-Z]{3}(?=\\_)")    
    photos[i, 'plot'] <- str_extract(photos[i, 'file'], "(?<=[A-Z]{3}\\_)[0-9]{2}(?=\\_)")
    photos[i, 'doy'] <- str_extract(photos[i, 'file'], "(?<=[0-9]{2}\\_)[0-9]{3}(?=\\.tif$)")
    
    # Image processing
    f <- GDAL.open(photos[i, 'file'])
    
    r <- getRasterData(f, band=1) #extracts band 1 = Red 
    av_r <- mean(r)
      photos[i, 'Av_R'] <- av_r
   
    g <- getRasterData(f, band=2) #extracts band 2 = Green 
    av_g <- mean(g)
      photos[i, 'Av_G'] <- av_g
    b <- getRasterData(f, band=3) #extracts band 3 = Blue 
    av_b <- mean(b)
      photos[i, 'Av_B'] <- av_b
    
    index <- ((av_g)/(av_r + av_g + av_b)) #calculates a normalized green value = rG 
    av_index <- mean(index)
      photos[i, 'Index'] <- av_index
    
    index2 <- ((av_r)/(av_r + av_g + av_b)) #calculates a normalized red value = rR 
    av_index2 <- mean(index2)
      photos[i, 'Index2'] <- av_index2
    
    index3 <- ((av_b)/(av_r + av_g + av_b)) #calculates a normalized blue value = rB 
    av_index3 <- mean(index3)
      photos[i, 'Index3'] <- av_index3
    
    # Based on GEI = 2 * rG - (rR + rB)
    GEI <- (2*av_index - (av_index2 + av_index3))
      photos[i, 'GEI'] <- GEI
      
    GDAL.close(f) 
  }
  
  GEI <<- tibble(photos) %>%
    mutate(doy = as.numeric(doy),
           treatment = str_replace(treatment, "CTL", "C"),
           treatment = str_replace(treatment, "OTC", "T")) %>%
    select(site, plot, treatment, doy, GEI)
}

# write_csv(GEI, here("data/greenness/GEI.csv"))