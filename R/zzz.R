.onLoad <- function(libname, pkgname)
{
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage(paste0("#### MU v", 
                               installed.packages()['mu','Version'],
                               " 19 Aug 2023 ####"))
}
