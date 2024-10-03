load_usdm_tif <-
  function(x){
    outfile <- file.path("tif", paste0(x,".tif"))
    
    if(file.exists(outfile))
      return(outfile)
    
    oconus_grid <- 
      get_oconus_tif()
    
    out <-
      terra::rasterize(x = get_usdm(x) %>%
                         sf::st_transform(4326) %>%
                         sf::st_cast("POLYGON") %>%
                         tigris::shift_geometry() %>%
                         sf::st_make_valid() %>%
                         dplyr::group_by(usdm_class) %>%
                         dplyr::summarise() %>%
                         sf::st_cast("MULTIPOLYGON") %>%
                         terra::vect(),
                       y = oconus_grid,
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
      terra::mask(oconus_grid, 
                  maskvalues = NA)
    levels(out) <- data.frame(value = 0:5, 
                              usdm_class = c("None", paste0("D",0:4)))
    out %>%
      magrittr::set_names(x) %>%
      terra::writeRaster(filename = outfile,
                         overwrite = TRUE,
                         gdal = c("COMPRESS=DEFLATE"),
                         memfrac = 0.9
      )
    
    return(outfile)
  }
