GTFSwizard.StartupMessage <- function(){
  paste0(
    " _____ _______ ______ _____\n",
    "/ ____|__   __|  ____/ ____|\n",
    "| |  __   | |  | |__ | (___\n",
    "| | |_ |  | |  |  __| \\___ \\\n",
    "| |__| |  | |  | |    ____) |\n",
    " \\_____|  |_|  |_|   |_____/ _\n",
    "__      ___ ______ _ _ __ __| |\n",
    "\\ \\ /\\ / | |_  / _` | '__/ _` |\n",
    " \\ V  V /| |/ | (_| | | | (_| |\n",
    "  \\_/\\_/ |_/___\\__,_|_|  \\__,_|\n",
    "######## version ", utils::packageVersion("GTFSwizard"),
    " ##########\n",
    "Type 'citation(\"GTFSwizard\")' for\n",
    "citing this R package in publications."
  )
}
