GTFSwizard.StartupMessage <- function(){
  msg <- paste0(crayon::blue$bold("
   _____ _______ ______ _____
  / ____|__   __|  ____/ ____|
 | |  __   | |  | |__ | (___
 | | |_ |  | |  |  __| \\___ \\
 | |__| |  | |  | |    ____) |
  \\_____|  |_|  |_|   |_____/ _
 __      ___ ______ _ _ __ __| |
 \\ \\ /\\ / | |_  / _` | '__/ _` |
  \\ V  V /| |/ | (_| | | | (_| |
   \\_/\\_/ |_/___\\__,_|_|  \\__,_|
"))
  msg <- paste0(msg, crayon::cyan$italic("####### "), crayon::green$italic("version "), crayon::green$italic(utils::packageVersion("GTFSwizard")), crayon::green$italic(" (dev)"), crayon::cyan$italic(' ########'),"\n")
  msg <- paste0(msg, crayon::cyan('Type \'citation("GTFSwizard")\' for\nciting this R package in publications. \n'))
  return(msg)
}
