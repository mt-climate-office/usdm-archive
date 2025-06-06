update.packages(repos = "https://cran.rstudio.com/",
                ask = FALSE)

install.packages("pak",
                 repos = "https://cran.rstudio.com/")

pak::pak(
  c("Rcpp",
    "proxy", 
    "e1071", 
    "wk", 
    "classInt", 
    "DBI", 
    "magrittr", 
    "s2", 
    "units",
    "arrow")
  )

install.packages('terra', 
                 repos = "https://cran.rstudio.com/",
                 type = "source")

install.packages('sf', 
                 repos = "https://cran.rstudio.com/",
                 type = "source")

# install.packages("arrow", 
#                 # type = "source", 
#                 repos = c(arrow = "https://nightlies.apache.org/arrow/r", 
#                           "https://cran.rstudio.com/"))
#
# install.packages("sf",
#                  type = "source",
#                  configure.args = "--with-proj-lib=$(brew --prefix)/lib/",
#                  repos = "https://cran.rstudio.com/")
# 
# install.packages("terra",
#                  type = "source",
#                  configure.args = "--with-proj-lib=$(brew --prefix)/lib/",
#                  repos = "https://cran.rstudio.com/")

pak::pak(
  c("magrittr",
    "tidyverse",
    "png",
    "ragg",
    "multidplyr",
    "rmapshaper",
    "tigris",
    "cols4all",
    "openxlsx",
    "curl",
    "exactextractr"
  )
)
# install.packages("tigris", repos = "http://cran.us.r-project.org")

library(magrittr)
library(tidyverse)
library(multidplyr)
library(terra)
library(sf)
library(arrow)

dir.create("parquet",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("png",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("tif",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("county",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("outlook/png",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("outlook/parquet",
           recursive = TRUE,
           showWarnings = FALSE)

source("R/get_usdm.R")
source("R/as_rast_usdm.R")
source("R/usdm_layout.R")
source("R/update_usdm_outlook.R")
source("R/update_usdm_change.R")
source("R/update_drought_disasters.R")
source("R/update_drought_counties.R")

get_oconus <-
  function(rotate = TRUE){
    if (!file.exists("oconus.parquet")) {
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
        tidyr::unite(col = "FSA_CODE", STATEFP, COUNTYFP, sep = "", remove = FALSE) %>%
        dplyr::select(FSA_CODE,
                      NAME, 
                      STATE_NAME,
                      STATEFP,
                      COUNTYFP
        ) %>%
        rmapshaper::ms_simplify() %>%
        sf::st_transform("EPSG:5070") %>%
        # dplyr::group_by(state) %>%
        # dplyr::summarise() %>%
        # sf::st_cast("MULTIPOLYGON") %>%
        sf::write_sf("oconus.parquet",
                     layer_options = c("COMPRESSION=BROTLI"),
                     driver = "Parquet")
    }
    
    oconus <- 
      sf::read_sf("oconus.parquet")
    
    if(rotate)
      oconus %<>%
      tigris::shift_geometry()
    
    return(oconus)
  }

get_states <-
  function(rotate = TRUE){
    states <-
      get_oconus(rotate = FALSE) %>%
      dplyr::group_by(STATE_NAME) %>%
      dplyr::summarise() %>%
      sf::st_cast("MULTIPOLYGON")
    
    if(rotate)
      states %<>%
      tigris::shift_geometry()
    
    return(states)
  }

get_oconus_tif <-
  function(){
    if (!file.exists("oconus.tif")) {
      get_states() %>%
        sf::st_buffer(40000) %>%
        sf::st_bbox() %>%
        as.list() %$%
        terra::rast(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax,
                    resolution = c(4000,4000),
                    crs = "ESRI:102003"
        ) %>%
        terra::rasterize(get_states(),
                         .)  %>%
        magrittr::set_names(NULL) %>%
        terra::writeRaster(filename = "oconus.tif",
                           overwrite = TRUE, 
                           gdal = c("COMPRESS=DEFLATE"),
                           memfrac = 0.9
        )
    }
    return(
      terra::rast("oconus.tif")
    )
  }

update_usdm_archive <-
  function(force = FALSE){
    
    oconus <- 
      get_states(rotate = FALSE)
    
    oconus_tif <- 
      get_oconus_tif()
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(lubridate::today() - usdm_dates) >= 2]
    
    cluster <- multidplyr::new_cluster(parallel::detectCores())
    multidplyr::cluster_library(cluster, c("magrittr", "sf", "terra"))
    multidplyr::cluster_copy(cluster, c("get_usdm", "load_usdm_tif", "oconus", "get_states", "get_oconus", "get_oconus_tif"))
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
          geom_sf(data = dplyr::summarise(oconus),
                  fill = "gray80",
                  color = NA,
                  show.legend = FALSE) +
          geom_sf(aes(fill = usdm_class),
                  color = "white",
                  size = 0.1,
                  show.legend = T) +
          geom_sf(data = oconus,
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
          usdm_layout()
        
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
    
    if(
      force |
      (length(usdm_dates) > 0)
    ) {
      
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
      
      oconus <- 
        get_states()
      
      cluster <- multidplyr::new_cluster(parallel::detectCores())
      multidplyr::cluster_library(cluster, c("magrittr", "sf", "ggplot2"))
      multidplyr::cluster_copy(cluster, c("oconus", "get_states", "get_oconus", "get_oconus_tif", "plot_usdm", "ndmc", "noaa", "nidis", "mco", "usdm_layout"))
      
      usdm_tibble %>%
        dplyr::rowwise() %>%
        multidplyr::partition(cluster) %>%
        dplyr::mutate(plot = plot_usdm(x = .data$usdm, date = .data$date)) %>%
        dplyr::collect()
      
      rm(cluster)
      gc()
      gc()
      
      
      invisible({
        list.files("png",
                   full.names = TRUE,
                   pattern = "\\d-") %>%
          sort() %>%
          dplyr::last() %>%
          file.copy(to = file.path("png", "latest.png"),
                    overwrite = TRUE)
      })
      
      system2(
        command = "ffmpeg",
        args = paste0(
          " -r 15",
          " -pattern_type glob -i 'png/[0-9]*.png'",
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
          " -row-mt 1",
          " usdm.webm",
          " -y"),
        wait = TRUE
      )
      
      
    }
    
    update_usdm_outlook()
    
    update_usdm_change()
    
    update_drought_disasters(year = 2024)
    
    update_drought_counties()
    
    return(message("Finished!"))
  }

update_usdm_archive()
