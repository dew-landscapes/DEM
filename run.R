
  if(!exists("categ")) source("sampleDEM.R")
    
  # Clean up previous knits
  
  fs::file_delete(fs::dir_ls(regexp = "_book|_main"))
  
  bookdown::render_book()
  
  envFunc::git_commit_env(paste0("Successful render of report: "
                                 , format(Sys.Date(), "%Y-%m-%d")
                                 )
                          )
  