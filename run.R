
options(scipen = 999)

envFunc::check_packages(yaml::read_yaml("settings/packages.yaml")$packages
                        , lib = TRUE
                        )

source("dem.R")

purrr::map(fs::dir_ls(regexp = "_book|_main", recurse = 1)
           , \(x) if(file.exists(x)) fs::file_delete(x)
           )

xfun::in_dir("report"
             , bookdown::render_book(output_dir = "../docs")
             )

