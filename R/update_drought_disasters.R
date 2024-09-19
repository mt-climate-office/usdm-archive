update_drought_disasters <-
  function(){
    format_date <-
      function(x){
        lubridate::stamp("Jan 4, 2000", orders = "%b %d, %Y")(x) %>%
          stringr::str_replace(" 0", " ")
      }
    
    outfile <- file.path("png", "latest-drought-disasters.png")
    
    conus <- 
      sf::read_sf("conus.parquet") %>%
      tigris::shift_geometry()
    
    counties <-
      # sf::read_sf("https://github.com/mt-climate-office/usda-climate-smart/raw/main/data-derived/fsa-counties.parquet") %>%

      tigris::counties(cb = TRUE) %>%
      tidyr::unite(col = "FSA_CODE", STATEFP, COUNTYFP, sep = "") %>%
      dplyr::select(FSA_CODE) %>%
      sf::st_cast("POLYGON") %>%
      tigris::shift_geometry() %>%
      dplyr::group_by(FSA_CODE) %>%
      dplyr::summarise() %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_crop(conus)
    
    ## Disasters
    disaster_xlsx <- tempfile(fileext = ".xlsx")
    
    curl::curl_download("https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/Disaster-Assist/Secretarials/2024-secretarial-disasters/METADATA_CY2024_SEC_YTD.xlsx",
                          destfile = disaster_xlsx)
    
    disasters <-
      openxlsx::read.xlsx(
        disaster_xlsx,
        check.names = FALSE,
        sep.names = " "
      ) %>% 
      tibble::as_tibble() %>%
      dplyr::filter(DROUGHT == 1,
                    `CROP DISASTER YEAR` == lubridate::year(lubridate::today())) %>%
      dplyr::transmute(FSA_CODE = FIPS, 
                       `Designation Code`= factor(`Designation Code`,
                                                  levels = 1:2,
                                                  labels = c("Primary",
                                                             "Contiguous"),
                                                  ordered = TRUE)) %>%
      dplyr::arrange(FSA_CODE, `Designation Code`) %>%
      dplyr::distinct(FSA_CODE, .keep_all = TRUE) %>%
      dplyr::left_join(counties, .)
    
      date <- 
        disaster_xlsx %>%
        unz("docProps/core.xml") %>%
        xml2::read_xml() %>%
        xml2::xml_find_all("dcterms:modified") %>%
        xml2::xml_text() %>%
        lubridate::as_date()
      
    p <-
      ggplot() + 
      geom_sf(data = dplyr::summarise(conus),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = disasters,
              aes(fill = `Designation Code`),
              color = "white",
              size = 0.05,
              show.legend = T) +
      geom_sf(data = conus,
              color = "white",
              alpha = 0,
              show.legend = FALSE,
              size = 0.2) +
      scale_fill_manual(
        values = 
          c(
            "Primary" = "#DC0005",
            "Contiguous" = "#FD9A09"
          ),
        na.value = NA,
        drop = FALSE,
        na.translate = FALSE,
        name = paste0(lubridate::year(date), " USDA Secretarial\nDisaster Designations\nfor Drought"),
        guide = guide_legend(direction = "vertical",
                             title.position = "top",
                             ncol = 1) ) +
      usdm_layout(attribution = "The Secretary of Agriculture is authorized to designate counties\nas disaster areas for emergency loan and assistance programs,\nsuch as Farm Service Agency (FSA) disaster assistance programs.\nMap data courtesy of the FSA. Map courtesy of the Montana Climate Office.",
                  footnote = paste0("Data updated ", format_date(date)))
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    grid::grid.draw(gt) %>%
      ggsave(plot = .,
             filename = outfile,
             device = ragg::agg_png,
             width = 10,
             height = 5.14,
             # height = 6.86,
             bg = "white",
             dpi = 600)
    
  }

