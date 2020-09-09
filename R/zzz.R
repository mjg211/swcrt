.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    rep("-", 77), "\nswcrt: Draw and export stepped-wedge cluster randomised",
    " trial design diagrams\n",
    rep("-", 77), "\n\nv.0.1: For an overview of the package's functionality ",
    "enter: ?swcrt\n\nFor news on the latest updates enter: ",
    "news(package = \"swcrt\")")
}
