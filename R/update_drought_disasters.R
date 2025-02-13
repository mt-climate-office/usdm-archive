update_drought_disasters <-
  function(year = lubridate::today() |>
             lubridate::year()){
    format_date <-
      function(x){
        lubridate::stamp("Jan 4, 2000", orders = "%b %d, %Y")(x) %>%
          stringr::str_replace(" 0", " ")
      }
    
    year <- as.character(year)
    
    outfile <- file.path("png", "latest-drought-disasters.png")
    
    oconus <- 
      get_states()
    
    counties <-
      get_oconus()
    
    ## Disasters
    disaster_xlsx <- tempfile(fileext = ".xlsx")
    
    xml2::read_html("https://www.fsa.usda.gov/resources/disaster-assistance-program/disaster-designation-information") %>%
      xml2::xml_find_all(".//a") %>%
      as.character() %>%
      stringr::str_subset(paste0("cy",year)) %>%
      stringr::str_subset("xlsx") %>% 
      stringr::str_subset("sec") %>%
      xml2::read_html() %>%
      xml2::xml_find_all(".//a") %>%
      xml2::xml_attr("href") %>%
      paste0("https://www.fsa.usda.gov", .) %>%
      xml2::read_html() %>%
      xml2::xml_find_all(".//a") %>%
      as.character() %>%
      stringr::str_subset("xlsx") %>%
      stringr::str_subset(year) %>%
      xml2::read_html() %>%
      xml2::xml_find_all(".//a") %>%
      xml2::xml_attr("href") %>%
      curl::curl_download(destfile = disaster_xlsx)
    
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
      geom_sf(data = dplyr::summarise(oconus),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = disasters,
              aes(fill = `Designation Code`),
              color = "white",
              size = 0.05,
              show.legend = T) +
      geom_sf(data = oconus,
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

