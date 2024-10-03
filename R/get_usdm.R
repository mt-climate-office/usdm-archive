get_usdm <-
  function(x){
    
    outfile <- file.path("parquet", paste0(x,".parquet"))
    
    if(file.exists(outfile))
      return(sf::read_sf(outfile))
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y%m%d")
    
    sf::read_sf(
      paste0("/vsizip//vsicurl/https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_",x,"_M.zip")
    ) %>%
      sf::st_make_valid() %>%
      dplyr::transmute(usdm_class = factor(paste0("D", DM),
                                           levels = paste0("D", 0:4),
                                           ordered = TRUE)) %>%
      # Group by date and class, and generate multipolygons
      dplyr::group_by(usdm_class) %>%
      dplyr::summarise(.groups = "keep") %>%
      sf::st_cast("MULTIPOLYGON") %>%
      dplyr::arrange(usdm_class) %>%
      dplyr::mutate(date = as.character(x)) %>%
      dplyr::select(date, usdm_class) %>%
      sf::st_transform("EPSG:5070") %>%
      sf::st_intersection(
        get_states(rotate = FALSE) %>%
          sf::st_geometry() %>%
          sf::st_union()
      ) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::write_sf(outfile,
                   layer_options = c("COMPRESSION=BROTLI",
                                     "GEOMETRY_ENCODING=GEOARROW",
                                     "WRITE_COVERING_BBOX=NO"),
                   driver = "Parquet")
    
    return(sf::read_sf(outfile))
  }

