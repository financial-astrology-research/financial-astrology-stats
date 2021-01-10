# Title     : File sytem utilities.
# Created by: pablocc
# Created on: 10/01/2021

expandPath <- function(path) {
  normalizePath(path.expand(path))
}
