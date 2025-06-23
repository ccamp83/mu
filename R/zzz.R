.onLoad <- function(libname, pkgname)
{
  libraries()
  theme_set(theme_cowplot())
}

.onAttach <- function(libname, pkgname)
{
  descriptionfile <- system.file("DESCRIPTION", package = "mu")
  descfile <- desc::desc(descriptionfile)
  muVersion <- as.character(packageVersion(pkgname))
  packageStartupMessage(paste0("#### MU v",
                               muVersion,
                               " | ",
                               descfile$get_field("Date"),
                               " ####"))
}
