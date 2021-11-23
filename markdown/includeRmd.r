
includeRmd <- function(rmdfile, connection = "", level.shift = 0, strip.level = integer(0), hook = function(x) x) {
  infile <- readLines(rmdfile)
  for(x in infile) {
    x <- hook(x)
    x.1 <- sub("^#*", "", x, perl = TRUE)
    le <- nchar(x) - nchar(x.1) 
    if(le %in% strip.level) # skip that
      next
    if(le > 0) {
      if(le + level.shift > 0) { # on supprime le titre sinon ! (level.shift < 0...)
        cat(paste(rep("#", le + level.shift), collapse = ""), file = connection)
        cat(x.1, "\n", sep = "", file = connection)
      }
    } else 
    cat(x.1, "\n", sep = "", file = connection)
  }
}
