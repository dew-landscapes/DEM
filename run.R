
options(scipen = 999)

envFunc::check_packages(yaml::read_yaml("settings/packages.yaml")$packages
                        , lib = TRUE
                        )

tar_visnetwork()

tar_make()

tar_prune()

purrr::map(fs::dir_ls(regexp = "_book|_main", recurse = 1)
           , \(x) if(file.exists(x)) fs::file_delete(x)
           )

xfun::in_dir("report"
             , bookdown::render_book(output_dir = "../docs")
             )


if(FALSE) {
  
  # clean up -------
  
  ## logs -------
  fs::dir_delete("log")
  
  ## docs --------
  # delete for clean start on report
  if(FALSE) fs::dir_delete("docs")
  
}
