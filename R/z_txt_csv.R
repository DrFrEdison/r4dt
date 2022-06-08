txt.file <- function(filename){

  txt <- list()
  txt$location <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                          , x = filename
                          , y = lapply(gregexpr("_", filename), function(x) x[[2]])
                          , z = lapply(gregexpr("_", filename), function(x) x[[3]]))

  txt$line <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                                         , x = filename
                                         , y = lapply(gregexpr("_", filename), function(x) x[[3]])
                                         , z = lapply(gregexpr("_", filename), function(x) x[[4]]))

  txt$type <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                                         , x = filename
                                         , y = lapply(gregexpr("_", filename), function(x) max(x))
                                         , z = lapply(gregexpr("\\.", filename), function(x) max(x)))

  txt$datefrom <- mapply( function( x, z) substring(x, 1, z - 1)
                                             , x = filename
                                             , z = lapply(gregexpr("_", filename), function(x) x[[1]]))

  txt$dateto <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                                           , x = filename
                                           , y = lapply(gregexpr("_", filename), function(x) x[[1]])
                                           , z = lapply(gregexpr("_", filename), function(x) x[[2]]))

  txt$loc.line <- paste(txt$location, txt$line, sep = "_")
  return(txt)
}

lambda <- expression(paste(lambda, " in nm"))
ylab_1st <- expression(paste(Delta, " AU / ", Delta, lambda))
ylab_2nd <- expression(paste(Delta, " AU / ", Delta, lambda^2))
