.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0("\033[38;5;208m",
    r"(                           
         _           _ ___ _         
     ___| |___ ___ _| |  _|_|___ ___ 
    |  _| | .'|   | . |  _| |  _| -_|
    |_| |_|__,|_|_|___|_| |_|_| |___|
                                 
    )", "\033[0m"), "version:", utils::packageVersion("rlandfire"),
"\033[38;5;160m\n\nWARNING:\033[0m\n",
"The LFPS API product names are changing!\nType 'viewProducts()' to view the current names.\n\n",
"New product names may require previous workflows to be updated")
}