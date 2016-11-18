#' Make an elevational grid with water
#'
#' A light wrapper around \code{diamond.square.step}
#' @param n Size of grid will be a 2^n +1 grid (default: 7; a 129 x 129 grid)
#' diamond.square algorithm; inputs initially constructed terrain matrix and performs
#' diamond square steps on all cells
#' @return a terrain matrix; numeric elements indicate heights, and
#' NAs indicate cells filled with water
#' @return an image; a grid with colors corresponding to the values in the terrain matrix
#' @examples terrain <- make.terrain(4)
#' @export



## Terrain Wrapper
make.terrain <- function(n){
  if(is.null(n))
    n <- 4
  l <- 2^n + 1
  terrain <- matrix(nrow=l,ncol=l)
  terrain[c(1,l),c(1,l)] <- sample(abs(rnorm(4, 0, 1)), size = 4)

  # Diamond Square Step function
  diamond.square.step <- function(terrain){
    # Diamond step function
    diamond.step <- function(terrain){
      h <- (((ncol(terrain)-1)/2)+1)
      terrain[h,h] <- jitter(mean(terrain[c(1,(nrow(terrain))),c(1,(ncol(terrain)))]), factor = length(terrain), amount = 1)

      return(terrain)
    }
    # Square step function
    square.step <- function(terrain){
      h <- (((ncol(terrain)-1)/2)+1)

      terrain[h,1] <- jitter(mean(terrain[c(1,(nrow(terrain))),1]), factor = length(terrain), amount = 1)
      terrain[h,1] <- jitter(mean(terrain[h,c(h,1)]), factor = length(terrain), amount = 1)

      terrain[h,ncol(terrain)] <- jitter(mean(terrain[c(1,(nrow(terrain))),ncol(terrain)]), factor = length(terrain), amount = 1)
      terrain[h,ncol(terrain)] <- jitter(mean(terrain[h,c(h,ncol(terrain))]), factor = length(terrain), amount = 1)

      terrain[1,h] <- jitter(mean(terrain[1,c(1,ncol(terrain))]), factor = length(terrain), amount = 1)
      terrain[1,h] <- jitter(mean(terrain[c(1,h),h]), factor = length(terrain), amount = 1)

      terrain[nrow(terrain),h] <- jitter(mean(terrain[nrow(terrain),c(1,ncol(terrain))]), factor = length(terrain), amount = 1)
      terrain[nrow(terrain),h] <- jitter(mean(terrain[c(nrow(terrain),h),h]), factor = length(terrain), amount = 1)

      return(terrain)
    }
    # Running diamond square steps
    for (i in 2^(n:1)){
      for (c in seq(1, l-1, by=i)) {
        for (r in seq(1, l-1, by=i)) {
          terrain[r:(r+i),c:(c+i)] <- diamond.step(terrain[r:(r+i),c:(c+i)])
          terrain[r:(r+i),c:(c+i)] <- square.step(terrain[r:(r+i),c:(c+i)])
        }
      }
    }
    return(terrain)
  }

  terrain <- diamond.square.step(terrain)
  ## Add Water
  terrain[terrain < 0 ] <- NA
  ## Produce terrain image
  image(terrain)
  return(terrain)
}

