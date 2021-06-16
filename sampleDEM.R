

  library(tidyverse)
  library(fs)
  library(raster)
  library(sf)
  library(ggridges)
  library(furrr)
  
  commonFiles <- path("..","template","toCommon")
  
  if(file.exists(commonFiles)){
    
    files <- dir_ls(commonFiles)
    newFiles <- files %>% gsub(commonFiles,path("common"),.)
    dir_create("common")
    walk2(files,newFiles,file_copy,overwrite=TRUE)
    
  }
  
  source("common/functions.R") # these are generic functions (e.g. vec_to_sentence)
  
  
  
  # Cores to use for any parallel processing
  useCores <- if(parallel::detectCores() > 14) 14 else parallel::detectCores()-1
  
  # Plan for any furrr functions
  plan(multiprocess
       , workers = useCores
       )
  
  
#-------Rasters--------
  
  rasters <- tibble(path = dir_ls(path("..","..","Data","Raster","DEM"), regexp = "\\.tif$")) %>%
    dplyr::mutate(name = gsub(".tif","",basename(path))
                  , ras = map(path,raster)
                  )
  
  extent <- st_read(dir_ls("shp",regexp = "Bakara_smaller.shp$")) %>%
    st_transform(crs = 7854)

  # base raster
  r <- raster(extent
              , resolution = 10
              , crs = CRS("+init=epsg:7854")
              )
  
  samples <- rasters %>%
    dplyr::mutate(crop = map(ras
                            , projectRaster
                            , to = r
                            )
                  )
  
#------Terrain-------
  
  # Definitions
  windowXS <- 5
  windowXL <- 9
  flatnessThresh <- 0.5
  
  # Focal window (for geomorph)
  focalWindow <- matrix(1, nrow = windowXL, ncol = windowXL)
  
  terr <- samples %>%
    dplyr::mutate(terr = future_map(crop,terrain,opt = terrOptions,unit = "degrees")
                  , sixClass = future_map(crop, landfClass, n.classes = "six", scale = windowXS)
                  , tenClass = future_map(crop, landfClass, n.classes = "ten", sn = windowXS, ln = windowXL)
                  , geomorph = future_map(crop, geomorph_ras)
                  )
  
  
#------Analysis--------
  
  pts <- st_sample(extent,9999)
  
  res <- terr %>%
    dplyr::mutate(res = map(terr,raster::extract,y = as_Spatial(pts))) %>%
    dplyr::select(negate(is.list),res) %>%
    dplyr::mutate(res = map(res,~as_tibble(.) %>%
                              dplyr::bind_cols(st_coordinates(pts) %>%
                                                 as_tibble() %>%
                                                 dplyr::mutate(id = row_number())
                                               )
                            )
                  ) %>%    tidyr::unnest(cols = c(res)) %>%
    tidyr::pivot_longer(any_of(tolower(terrOptions))) %>%
    dplyr::filter(!is.na(value)
                  , value < quantile(value, probs = 0.999, na.rm = TRUE)
                  , value > quantile(value, probs = 0.001, na.rm = TRUE)
                  )
    
  
#----vis------
  
  ggplot(res,aes(value,colour = type)) +
    geom_density(size = 1) +
    facet_wrap(~name, scales = "free")
  
  
  
  
  