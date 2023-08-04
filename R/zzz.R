.onLoad <- function(libname, pkgname)
{
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("#### MU v 3.0.0 - 4 Aug 2023 ####")
}
