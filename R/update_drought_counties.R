update_drought_counties <-
  function(){
    
    oconus_counties <- 
      get_oconus() |>
      dplyr::select(FSA_CODE)
    
    usdm_rasters <-
      file.path("tif") %>%
      list.files(pattern = "*.tif$",
                 full.names = TRUE) %>%
      magrittr::set_names(., tools::file_path_sans_ext(basename(.)))
    
    county_drought <-
      file.path("county") %>%
      list.files() %>%
      tools::file_path_sans_ext()
    
    usdm_rasters <-
      usdm_rasters[!(names(usdm_rasters) %in% county_drought)]
    
    if(length(usdm_rasters) > 0){
      usdm_rasters %<>%
        terra::rast(usdm_rasters)
      
      exactextractr::exact_extract(usdm_rasters, 
                                   oconus_counties, 
                                   fun = "max", 
                                   append_cols = TRUE,
                                   force_df = TRUE,
                                   full_colnames = TRUE) %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(-FSA_CODE, 
                            names_to = "Date", 
                            values_to = "USDM Class") %>%
        dplyr::mutate(Date = stringr::str_remove(Date, "max.") %>%
                        lubridate::as_date(),
                      `USDM Class` = as.integer(`USDM Class`) %>%
                        factor(levels = levels(usdm_rasters[[1]])[[1]][[1]],
                               labels = levels(usdm_rasters[[1]])[[1]][[2]],
                               ordered = TRUE)) %>%
        dplyr::arrange(FSA_CODE, Date, `USDM Class`) %>%
        dplyr::group_by(Date) %>%
        tidyr::nest() %$%
        {magrittr::set_names(data, Date)} %>%
        purrr::iwalk(\(x,y) arrow::write_parquet(x,
                                                 sink = file.path("county", paste0(y, ".parquet")),
                                                 version = "latest",
                                                 compression = "brotli"))
    }
    
    return(
      # list.files("county", 
      #            full.names = TRUE) %>%
      #   magrittr::set_names(., tools::file_path_sans_ext(basename(.))) %>%
      #   {tibble::tibble(Date = names(.), file = .)} %>%
      #   dplyr::rowwise() %>%
      #   dplyr::mutate(file = list(
      #     arrow::read_parquet(file)
      #   )) %>%
      #   tidyr::unnest(file)
    )
    
    
  }

