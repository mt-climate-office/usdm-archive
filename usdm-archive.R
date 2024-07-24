# install.packages("pak")
# pak::pak(
#   c("magrittr",
#     "tidyverse",
#     "multidplyr",
#     # "arrow",
#     # "rmapshaper",
#     "tigris",
#     "sf",
#     "terra"
#     )
# )
install.packages("tigris", repos = "http://cran.us.r-project.org")

library(magrittr)
library(tidyverse)
library(multidplyr)
library(terra)
library(sf)


dir.create("parquet",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("png",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("tif",
           recursive = TRUE,
           showWarnings = FALSE)

source("R/get_usdm.R")
source("R/as_rast_usdm.R")


update_usdm_archive <-
  function(force = FALSE){
    
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
    
    if (!file.exists("conus.tif")) {
      !is.na(terra::rast("~/git/publications/usdm-climatology/data-derived/nclimgrid/prcp/1952-01-01_30.tif")) %>%
        magrittr::set_names(NULL) %>%
        terra::writeRaster(filename = "conus.tif",
                           overwrite = TRUE, 
                           gdal = c("COMPRESS=DEFLATE", "of=COG"),
                           memfrac = 0.9
        )
    }
    
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(lubridate::today() - usdm_dates) >= 2]
    
    cluster <- multidplyr::new_cluster(parallel::detectCores())
    multidplyr::cluster_library(cluster, c("magrittr", "sf", "terra"))
    multidplyr::cluster_copy(cluster, c("get_usdm", "load_usdm_tif", "conus"))
    multidplyr::cluster_send(cluster, sf::sf_use_s2(FALSE))
    
    usdm <-
      tibble::tibble(date = usdm_dates) %>%
      dplyr::rowwise() %>%
      multidplyr::partition(cluster) %>%
      dplyr::mutate(usdm = list(
        get_usdm(date)
      ),
      usdm_rast = list(
        load_usdm_tif(date)
      )) %>%
      dplyr::collect() %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(usdm)) %$%
      usdm %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(usdm_class = factor(usdm_class,
                                        levels = paste0("D", 0:4),
                                        ordered = TRUE),
                    date = lubridate::as_date(date))
    
    
    rm(cluster)
    gc()
    gc()
    
    ndmc <- 
      "https://droughtmonitor.unl.edu/webfiles/logos/NDMC/png/NDMC-logo-color.png" %>%
      httr::GET() %>%
      httr::content(type = "image/png") %>%
      grid::rasterGrob(interpolate=TRUE,
                       width = unit(0.75, "in"),
                       height = unit(0.75, "in"),
                       x = unit(-0.15, "npc"),
                       y = unit(0.28, "npc"),
                       just = "left")
    
    noaa <-
      "https://droughtmonitor.unl.edu/webfiles/logos/NOAA/png/NOAA-logo-color.png" %>%
      httr::GET() %>%
      httr::content(type = "image/png") %>%
      grid::rasterGrob(interpolate=TRUE,
                       width = unit(0.75, "in"),
                       height = unit(0.75, "in"),
                       x = unit(-0.03, "npc"),
                       y = unit(0.28, "npc"),
                       just = "left")
    
    mco <-
      "MCO_logo.png" %>%
      png::readPNG() %>%
      grid::rasterGrob(interpolate=TRUE,
                       width = unit(0.6 * 3600/1325, "in"),
                       height = unit(0.6, "in"),
                       x = unit(-0.15, "npc"),
                       y = unit(0.11, "npc"),
                       just = "left")
    
    attribution <-
      grid::textGrob(
        label = 
          "The U.S. Drought Monitor is jointly produced by the National Drought\nMitigation Center at the University of Nebraska-Lincoln, the United States\nDepartment of Agriculture, and the National Oceanic and Atmospheric Administration.\nMap data courtesy of NDMC. Map courtesy of the Montana Climate Office.",
        just = "left",
        x = unit(-0.21, "npc"),
        y = unit(0.43, "npc"),
        gp = grid::gpar(fontface = "italic",
                        fontsize = 6)
      )
    
    plot_usdm <-
      function(x, date){
        outfile <- file.path("png", paste0(date,".png"))
        
        if(file.exists(outfile))
          return(outfile)
        
        x %<>%
          sf::st_cast("POLYGON") %>%
          tigris::shift_geometry() %>%
          sf::st_make_valid() %>%
          dplyr::group_by(usdm_class) %>%
          dplyr::summarise() %>%
          sf::st_cast("MULTIPOLYGON")
        
        p <-
          ggplot(x) +
          geom_sf(data = dplyr::summarise(conus),
                  fill = "gray80",
                  color = NA,
                  show.legend = FALSE) +
          geom_sf(aes(fill = usdm_class),
                  color = "white",
                  size = 0.1,
                  show.legend = T) +
          geom_sf(data = conus,
                  color = "white",
                  alpha = 0,
                  show.legend = FALSE,
                  size = 0.2) +
          scale_fill_manual(values = c("#ffff00",
                                       "#fcd37f",
                                       "#ffaa00",
                                       "#e60000",
                                       "#730000"),
                            drop = FALSE,
                            name = paste0("US Drought Monitor\n", format(lubridate::as_date(date), "%B %e, %Y") %>% stringr::str_squish()),
                            guide = guide_legend(direction = "vertical",
                                                 title.position = "top") ) +
          theme_void(base_size = 24) +
          theme(legend.position = "inside",
                legend.position.inside = c(-0.05,0.7),
                plot.margin = unit(c(0,0,0,0.2), "npc"),
                # legend.justification = c(0.1,0.5),
                # legend.key.width = unit(0.1, "npc"),
                legend.text.position = "left",
                legend.title = element_text(size = 18, face = "bold", hjust = 1),
                legend.text = element_text(size = 14),
                strip.text.x = element_text(margin = margin(b = 5))) +
          annotation_custom(ndmc, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          annotation_custom(noaa, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          annotation_custom(mco, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          annotation_custom(attribution, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
        
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
        
        
        return(outfile)
      }
    
    
    usdm_dates <-
      usdm_dates[!(usdm_dates %in% stringr::str_remove(list.files("png/"),".png"))]

    usdm_tibble <-
      usdm %>%
      dplyr::mutate(usdm_class = factor(usdm_class,
                                        levels = paste0("D", 0:4),
                                        labels = c("Abnormally Dry: D0",
                                                   "Moderate Drought: D1",
                                                   "Severe Drought: D2",
                                                   "Extreme Drought: D3",
                                                   "Exceptional Drought: D4"),
                                        ordered = TRUE)) %>%
      # dplyr::mutate(usdm_class = forcats::fct_relabel(usdm_class, ~ paste(., " "))) %>%
      dplyr::arrange(date, usdm_class) %>%
      dplyr::group_by(date) %>%
      tidyr::nest(usdm = c(usdm_class, geometry)) %>%
      dplyr::filter(date %in% usdm_dates)

    conus %<>%
      tigris::shift_geometry()

    cluster <- multidplyr::new_cluster(parallel::detectCores())
    multidplyr::cluster_library(cluster, c("magrittr", "sf", "ggplot2"))
    multidplyr::cluster_copy(cluster, c("conus", "plot_usdm", "ndmc", "noaa", "mco", "attribution"))

    usdm_tibble %>%
      dplyr::rowwise() %>%
      multidplyr::partition(cluster) %>%
      dplyr::mutate(plot = plot_usdm(x = .data$usdm, date = .data$date)) %>%
      dplyr::collect()

    rm(cluster)
    gc()
    gc()

    if(
      !force &&
      !(length(usdm_dates) > 0)
    ) {
      return(invisible(NA))
    }

    system2(
      command = "ffmpeg",
      args = paste0(
        " -r 15",
        " -pattern_type glob -i 'png/*.png'",
        " -s:v 3000x2058",
        " -c:v libx265",
        " -crf 28",
        " -preset fast",
        " -tag:v hvc1",
        " -pix_fmt yuv420p10le",
        " -an",
        " usdm.mp4",
        " -y"),
      wait = TRUE
    )

    system2(
      command = "ffmpeg",
      args = paste0(
        "-i usdm.mp4",
        " -c:v libvpx-vp9",
        " -crf 30",
        " -b:v 0",
        " usdm.webm",
        " -y"),
      wait = TRUE
    )
    
    return(message("Finished!"))
  }

update_usdm_archive()


