.onAttach <- function(libname, pkgname){
  if(interactive()){
    packageStartupMessage(GTFSwizard.StartupMessage())
  }
}
