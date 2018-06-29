all(vapply(dir(path = "sources"),
           function(x) {
             file.copy(file.path("sources", x), paste0("C:/temp/miktex/", x))
           },
           TRUE))


