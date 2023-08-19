.onLoad <- function(libname, pkgname)
{
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  descriptionfile <- system.file("DESCRIPTION", package = "mu")
  descfile <- desc::desc(descriptionfile)
  
  packageStartupMessage(paste0("#### MU v",
                               installed.packages()['mu','Version'],
                               " | ",
                               descfile$get_field("Date"),
                               " ####"))
}
