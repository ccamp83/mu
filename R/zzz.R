.onLoad <- function(libname, pkgname)
{
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("#### MU v 3.0.2 - 19 Aug 2023 ####")
}
