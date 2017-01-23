.onLoad <- function(libname = find.package("RandomThinning"), pkgname = "RandomThinning"){
    library.dynam("RandomThinning", "RandomThinning", .Library)
}

.onUnload <- function (libpath) { # nocov start
      library.dynam.unload("RandomThinning", libpath)
}
