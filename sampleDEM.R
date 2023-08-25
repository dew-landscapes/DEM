

#--------Setup---------

  library(tidyverse)
  library(fs)
  library(raster)
  library(sf)
  library(ggridges)
  library(furrr)
  library(gridExtra)

  library(envGeomorph)
  

#-------Rasters--------
  
  types <- tribble(
    ~lookfor, ~short,
    "AW3D30", "AW3D_30",
    "SA_Sample_AW3D", "AW3D_2",
    "bak2_bp","Bakara",
    "TOPO.NASA", "SRTM",
    "WorldDEM_DTM", "WorldDEMFull",
    "WorldDEM_LIT", "WorldDEMLite"
  )
  
  rasters <- tibble(path = dir_ls(path("..","..","Data","Raster","DEM")
                                  , regexp = "\\.tif$"
                                  , recurse = TRUE
                                  )
                    ) %>%
    dplyr::mutate(lookfor = str_extract(path,paste0(types$lookfor,collapse = "|"))) %>%
    dplyr::left_join(types) %>%
    dplyr::mutate(ras = map(path,raster)
                  , ras = map(ras,reclassify,rcl = cbind(-Inf, 0, NA), right=FALSE)
                  )
  
  
  # Minimum extent of data across all rasters
  naPoly <- terra::as.polygons(terra::rast(rasters$ras[rasters$short == "AW3D_2"][[1]]) >= 0) %>%
    sf::st_as_sf()
  
  naPolyRas <- raster(resolution = 10
                      , ext = extent(naPoly)
                      , crs = crs(naPoly)
                      )
  
  samples <- rasters %>%
    dplyr::mutate(gda94 = map(ras
                              , raster::projectRaster
                              , crs =CRS("+init=epsg:28354")
                              )
                  , mask = map(gda94
                               , raster::mask
                               , mask = as_Spatial(naPoly)
                               )
                  , crop = map(mask
                               , raster::crop
                               , y = as_Spatial(naPoly)
                               )
                  , repr = map(mask
                               , raster::projectRaster
                               , to = naPolyRas
                               )
                  ) %>%
    tidyr::pivot_longer(cols = c(crop,repr), names_to = "type", values_to = "r")
  
#------Terrain-------
  
  # Definitions
  windowXS <- 3
  windowXL <- 33
  
  terrOptions <- c("slope", "aspect", "TPI", "TRI", "roughness", "flowdir")
  
  terr <- samples %>%
    dplyr::mutate(cellSize = map_dbl(r,function(x) res(x)[1]*res(x)[2])
                  , outFile = path("out",paste0(short,"_",type,".tif"))
                  , terr = map(r,terrain,opt = terrOptions,unit = "degrees")
                  , sixClass = map2(r, outFile, land_class, n.classes = "six", sn = windowXS, doNew = FALSE)
                  , tenClass = map2(r, outFile, land_class, n.classes = "ten", sn = windowXS, ln = windowXL, doNew = FALSE)
                  , geomorph = map2(r, outFile, geomorph_ras)
                  )
  
  make_levelplot <- function(objName,ras,rasName = NULL) {
    
    l <- levelplot(ras
                   , main = rasName
                   , colorkey = FALSE
                   )
    
    assign(objName,l,envir = globalenv())
    
  }
  
  pwalk(list(paste0("p",str_pad(1:nrow(terr),2,pad = "0"))
             , terr$tenClass
             , paste0(terr$short,"_",terr$type)
             )
        , make_levelplot
        )
  
  p00 <- levelplot(terr$geomorph[[2]]
                   , main = "legend"
                   )
  
  library(cowplot)
  
  plot_grid(plotlist = mget(ls(pattern = "leg|p\\d{2}")))
  
  
  
  
#------Analysis--------
  
  pts <- st_sample(naPoly,9999)
  
  cont <- terr %>%
    dplyr::mutate(rasVal = map(terr,raster::extract,y = as_Spatial(pts))) %>%
    dplyr::select(negate(is.list),rasVal) %>%
    dplyr::mutate(rasVal = map(rasVal,~as_tibble(.) %>%
                              dplyr::bind_cols(st_coordinates(pts) %>%
                                                 as_tibble() %>%
                                                 dplyr::mutate(id = row_number())
                                               )
                            )
                  ) %>% 
    tidyr::unnest(cols = c(rasVal)) %>%
    tidyr::pivot_longer(any_of(tolower(terrOptions))) %>%
    dplyr::filter(!is.na(value))
    
  categ <- terr %>%
    tidyr::pivot_longer(cols = c(sixClass,tenClass,geomorph)) %>%
    dplyr::mutate(rasVal = future_map(value,raster::extract,y = as_Spatial(pts))) %>%
    dplyr::select(negate(is.list),rasVal) %>%
    dplyr::mutate(rasVal = map(rasVal,~as_tibble(.) %>%
                              dplyr::bind_cols(st_coordinates(pts) %>%
                                                 as_tibble() %>%
                                                 dplyr::mutate(id = row_number())
                                               )
                            )
                  ) %>% 
    tidyr::unnest(cols = c(rasVal)) %>%
    dplyr::left_join(geomorph.def, by = c("value" = "num_lf"))
  
  
#----vis------
  
  ggplot(cont %>%
           dplyr::mutate(short = fct_reorder(short,cellSize,mean)) %>%
           # dplyr::group_by(short,name) %>%
           # dplyr::filter(value < quantile(value, probs = 0.99)
           #               , value > quantile(value, probs = 0.01)
           #               ) %>%
           # dplyr::ungroup() %>%
           {.}
         ,aes(value,short,fill = cellSize, height = ..density..)
         ) +
    geom_density_ridges(scale = 1, stat = "density") +
    facet_grid(type~name, scales = "free") +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  
  ggplot(categ %>%
           dplyr::mutate(short = fct_reorder(short,cellSize,mean))
         , aes(short,fill = cellSize)
         ) +
    geom_histogram(stat = "count"
                   , position = "dodge2"
                   ) +
    coord_flip() +
    facet_grid(name+type~name_en, scales = "free") +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  