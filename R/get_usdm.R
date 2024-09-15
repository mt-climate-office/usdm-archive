if (!file.exists("conus.parquet")) {
  conus <-
    tigris::counties(cb = TRUE) %>%
    dplyr::filter(!(STATE_NAME %in% c(
      # "Puerto Rico",
      "American Samoa",
      # "Alaska",
      # "Hawaii",
      "Guam",
      "Commonwealth of the Northern Mariana Islands",
      "United States Virgin Islands"
    ))) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid() %>%
    dplyr::select(state = STATE_NAME,
                  state_fips = STATEFP,
                  county = NAME, 
                  county_fips = COUNTYFP
    ) %>%
    rmapshaper::ms_simplify() %>%
    sf::st_transform("EPSG:5070") %>%
    dplyr::group_by(state) %>%
    dplyr::summarise() %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::write_sf("conus.parquet",
                 layer_options = c("COMPRESSION=BROTLI"),
                 driver = "Parquet")
}

conus <- sf::read_sf("conus.parquet")

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
        sf::st_union(
          sf::st_geometry(conus)
        )
      ) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::write_sf(outfile,
                   layer_options = c("COMPRESSION=BROTLI",
                                     "GEOMETRY_ENCODING=GEOARROW",
                                     "WRITE_COVERING_BBOX=NO"),
                   driver = "Parquet")
    
    return(sf::read_sf(outfile))
  }

