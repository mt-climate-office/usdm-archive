if (!file.exists("conus.tif")) {
  !is.na(terra::rast("~/git/publications/usdm-climatology/data-derived/nclimgrid/prcp/1952-01-01_30.tif")) %>%
    magrittr::set_names(NULL) %>%
    terra::project("EPSG:5070") %>%
    terra::writeRaster(filename = "conus.tif",
                       overwrite = TRUE, 
                       gdal = c("COMPRESS=DEFLATE", "of=COG"),
                       memfrac = 0.9
    )
}

load_usdm_tif <-
  function(x){
    outfile <- file.path("tif", paste0(x,".tif"))
    
    if(file.exists(outfile))
      return(outfile)
    
    conus_grid <- 
      terra::rast("conus.tif")
    
    out <-
      terra::rasterize(x = get_usdm(x) %>%
                         terra::vect(),
                       y = conus_grid,
                       field = "usdm_class")
    
    m <- dplyr::left_join(levels(out)[[1]],
                          data.frame(value = 0:5, 
                                     usdm_class = c("None", paste0("D",0:4))))
    
    out %<>% 
      as.numeric() %>%
      terra::subst(from = m[,1], 
                   to = m[,3]) %>%
      terra::droplevels()
    
    out[is.na(out)] <- 0
    out %<>% 
      terra::mask(conus_grid, 
                  maskvalues = 1)
    levels(out) <- data.frame(value = 0:5, 
                              usdm_class = c("None", paste0("D",0:4)))
    out %>%
        magrittr::set_names(x) %>%
        terra::writeRaster(filename = outfile,
                           overwrite = TRUE, 
                           gdal = c("COMPRESS=DEFLATE", "of=COG"),
                           memfrac = 0.9
        )
    
    return(outfile)
  }
