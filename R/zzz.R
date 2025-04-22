.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0("\033[38;5;208m",
    r"(                           
         _           _ ___ _         
     ___| |___ ___ _| |  _|_|___ ___ 
    |  _| | .'|   | . |  _| |  _| -_|
    |_| |_|__,|_|_|___|_| |_|_| |___|
                                 
    )", "\033[0m"), "version:", utils::packageVersion("rlandfire"),
"\033[38;5;160m\n\nNOTICE:\033[0m\n",
"The LFPS API has been updated (LFPSv1 -> LFPSv2) and has new requirements.\n",
"To review the required parameters and syntax for LFPSv2 view `?rlandfire::landfireAPIv2`\n",
"Product names and availability may have changed, check `viewProducts()`\n\n",
"\033[38;5;160mWorkflows built before May 2025 or with `rlandfire` versions < 2.0.0 will need to be updated.\033[0m")
}