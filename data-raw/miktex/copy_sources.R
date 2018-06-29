all(vapply(dir(path = "sources", full.names = TRUE),
           function(x) {
             file.copy(x, paste0("C:/temp/miktex/", x))
           },
           TRUE))


