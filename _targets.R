
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tar_source ------
tar_source()

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = floor(parallel::detectCores() * (2 / 3))
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = here::here("log"))
                                                    )
               )

list(
  # targets --------
  ## settings -------
  ### settings ------
  tar_target(set_file, "settings/setup.yaml", format = "file")
  , tar_target(settings, yaml::read_yaml(set_file))
  ### analysis --------
  , tar_target(set_file_analysis, "settings/analysis.yaml", format = "file")
  , tar_target(settings_analysis, yaml::read_yaml(set_file_analysis))
  ### aois ------
  , tar_target(set_file_aoi, "settings/aoi.yaml", format = "file")
  , tar_target(settings_aoi, yaml::read_yaml(set_file_aoi))
  ### dems --------
  , tar_target(set_file_dem, "settings/dem.yaml", format = "file")
  , tar_target(settings_dem, yaml::read_yaml(set_file_dem))
  ## base grid path ---------
  , tar_target(base_grid_path
               , fs::path(envFunc::name_env_out(list(context = settings$extent
                                                     , grain = settings$grain
                                                     )
                                                , base_dir = "I:"
                                                )$path
                          , "base.tif"
                          )
               )
  ## dem path -------
  , tar_target(dem_path
               , fs::path("V:/Samba_ImageServices_Admin/Digital_Elevation_Models/Statewide")
               )
  ## aois ---------
  , tar_target(aoi_df
               , settings_aoi |>
                 purrr::map(\(x) tibble::as_tibble(x)) |>
                 dplyr::bind_rows(.id = "aoi_name") |>
                 #dplyr::filter(grepl("Kanku", aoi_name)) |> # TESTING
                 dplyr::mutate(sf = purrr::map(polygons
                                               , \(x) sfarrow::st_read_parquet(fs::path("..", "..", ".."
                                                                                        , "data", "vector"
                                                                                        , paste0(x, ".parquet")
                                                                                        )
                                                                               )
                                               )
                               , aoi = purrr::pmap(list(sf, filt_col, filt_level, buffer)
                                                   , \(a, b, c, d) envFunc::make_aoi(a, b
                                                                                     , c, buffer = d
                                                                                     , out_crs = settings$crs$proj
                                                                                     )
                                                   )
                               )
               )
  ## dems ----------
  , tar_target(dem_df
               , settings_dem |>
                 purrr::map(\(x) tibble::as_tibble(x)) |>
                 dplyr::bind_rows(.id = "dem_name") |>
                 dplyr::inner_join(fs::dir_ls(dem_path
                                              , recurse = TRUE
                                              , regexp = "\\.tif$"
                                              , type = "file"
                                              ) |>
                                     tibble::enframe(name = NULL, value = "full_path") |>
                                     dplyr::mutate(file = basename(full_path))
                                   ) |>
                 dplyr::mutate(licence = dplyr::if_else(grepl("DEWOnly", full_path), "DEW only", "public"))
               )
  ## workflow df --------
  , tar_target(workflow_df
               , dem_df |>
                 dplyr::cross_join(aoi_df) |>
                 dplyr::mutate(overlap = purrr::map2(full_path, aoi, test_intersection)) |>
                 tidyr::unnest(cols = c(overlap)) |>
                 dplyr::filter(overlap > 0.9) |>
                 dplyr::mutate(r_temp = purrr::map2(full_path
                                                    , aoi
                                                    , \(x, y) apply_window(x
                                                                           , y
                                                                           )
                                                    )
                               , r_temp = purrr::map(r_temp
                                                     , terra::project
                                                     , y = paste0("epsg:", settings$crs$proj)
                                                     )
                               , original_cm = purrr::map_dbl(r_temp, \(x) round(terra::res(x)[[1]] * 100, 0))
                               ) |>
                 dplyr::select(-r_temp) |>
                 dplyr::group_by(aoi_name) |>
                 dplyr::mutate(is_ref = original_cm == min(original_cm)) |>
                 dplyr::ungroup() |>
                 dplyr::mutate(group = dplyr::row_number()) |>
                 dplyr::group_by(group) |>
                 tar_group()
               , deployment = "main"
               , iteration = "group"
               )
  ## mung rasters ---------
  , tar_terra_rast(m
                   , mung_ras(ras_path = workflow_df$full_path[[1]]
                              , aoi = workflow_df$aoi[[1]]
                              , base = base_grid_path
                              )
                   , pattern = map(workflow_df)
                   )
  ## qfit rasters --------
  , tar_terra_rast(q
                   , MultiscaleDTM::Qfit(m
                                         , na.rm = TRUE
                                         )
                   , pattern = map(m)
                   )
  ## terrain rasters ----------
  , tar_target(terrain_metrics
               , c("TPI", "TRI", "TRIriley", "TRIrmsd", "roughness"
                   #, "flowdir"
                   )
               )
  , tar_terra_rast(t
                   , terra::terrain(m
                                    , v = terrain_metrics
                                    )
                   , pattern = map(m)
                   )
  ## landform rasters -------
  , tar_terra_rast(l
                   , land_class(m
                                , n.classes = settings_analysis$classes
                                , sn = settings_analysis$sn
                                , ln = settings_analysis$ln
                                )
                   , pattern = map(m)
                   )
  ## results df -------
  , tar_target(results_df
               , workflow_df |>
                 dplyr::mutate(vals_qfit = purrr::map(q
                                                      , \(x) vals_to_tibble(x)
                                                      )
                               , vals_terr = purrr::map(t
                                                        , \(x) vals_to_tibble(x)
                                                        )
                               , vals_t = purrr::map2(vals_qfit, vals_terr
                                                      , \(x, y) dplyr::left_join(x, y)
                                                      )
                               , vals_l = purrr::map(l
                                                     , \(x) vals_to_tibble(x)
                                                     )
                               , aoi_id = gsub("\\s", "", aoi_name)
                               )
               )
  ## continuous terrain features -------
  , tar_target(terr_cont
               , c(grep("feature"
                        , formals(MultiscaleDTM::Qfit)$metrics |> as.character() |> head(-1) |> grep("c$", x = _, value = TRUE, invert = TRUE)
                        , value = TRUE
                        , invert = TRUE
                        )
                   , terrain_metrics
                   )
               )
  ## continuous df--------
  , tar_target(cont
               , results_df |>
                 dplyr::select(aoi_name
                               , aoi_id
                               , dem_name
                               , is_ref
                               , vals_t
                               , original_cm
                               ) |>
                 tidyr::unnest(cols = c(vals_t)) |>
                 dplyr::select(aoi_name, aoi_id, dem_name, x , y
                               , is_ref, original_cm, tidyselect::matches(terr_cont)
                               ) |>
                 tidyr::pivot_longer(matches(terr_cont)
                                     , names_to = "metric"
                                     ) %>%
                 dplyr::filter(!is.na(value)) %>%
                 # ensure only cells that have data from each DEM are included
                 dplyr::add_count(aoi_name, metric, x , y, name = "count") %>%
                 dplyr::group_by(aoi_name) %>%
                 dplyr::filter(count == max(count)) %>%
                 dplyr::ungroup() %>%
                 # find reference value
                 dplyr::group_by(aoi_name, metric, x , y) |>
                 dplyr::mutate(ref = value[is_ref]) |>
                 dplyr::ungroup() %>%
                 dplyr::mutate(diff = ref - value) %>%
                 tidyr::nest(data = -c(aoi_name, aoi_id, metric))
               )
  ## categorical df --------
  , tar_target(categ
               , results_df |>
                 dplyr::select(aoi_name, aoi_id, dem_name
                               , is_ref
                               , vals_l
                               , original_cm
                               ) |>
                 tidyr::unnest(cols = c(vals_l))  |>
                 dplyr::filter(!is.na(category)) |>
                 # ensure only cells that have data from each DEM are included
                 dplyr::add_count(aoi_name, x , y, name = "count") |>
                 dplyr::group_by(aoi_name) |>
                 dplyr::filter(count == max(count)) |>
                 dplyr::ungroup() |>
                 dplyr::group_by(aoi_name, aoi_id, dem_name, original_cm, category) |>
                 dplyr::summarise(category_cells = dplyr::n()) |>
                 dplyr::ungroup() |>
                 dplyr::group_by(aoi_name, dem_name, original_cm) |>
                 dplyr::mutate(total_cells = sum(category_cells, na.rm = TRUE)) |>
                 dplyr::ungroup() |>
                 dplyr::mutate(prop = category_cells / total_cells) |>
                 dplyr::arrange(aoi_name, dem_name, original_cm)
               )
  ## categorical compare ref -------
  , tar_target(categ_compare_ref
               , results_df |>
                 dplyr::select(aoi_name, aoi_id, dem_name
                               , is_ref
                               , vals_l
                               , original_cm
                               ) |>
                 tidyr::unnest(cols = c(vals_l)) %>%
                 dplyr::filter(!is.na(category)) %>%
                 # ensure only cells that have data from each DEM are included
                 dplyr::add_count(aoi_name, x , y, name = "count") %>%
                 dplyr::group_by(aoi_name) %>%
                 dplyr::filter(count == max(count)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(aoi_name, x , y) %>%
                 dplyr::mutate(has_ref = sum(is_ref) > 0) %>%
                 dplyr::filter(has_ref) %>%
                 dplyr::mutate(reference = category[is_ref]) %>%
                 dplyr::ungroup() %>%
                 dplyr::count(aoi_name, aoi_id, dem_name, original_cm, reference, category) %>%
                 dplyr::group_by(aoi_name, dem_name, original_cm, reference) %>%
                 dplyr::mutate(references = sum(n)
                               , prop = n / references
                               ) %>%
                 dplyr::ungroup() %>%
                 dplyr::arrange(aoi_name, dem_name, original_cm, reference)
               )
  )
