#' Make an time series plant ecosystem with water
#'
#' A light wrapper around \code{setup.plants} and \code{plant.timestep}
#' @param terrain; inputs terrain matrix for underlying the plant composition matrix
#' @param timesteps; accounts for time scale that plant species survive, reproduce and compete across terrain
#' @param survival; probability that a particular plant species will survive at each timestep
#' @param reproduction; probability that a particular plant species will reproduce at each timestep
#' @param competition; probability that a particular plant species will win in a competition
#' @return a time series plant/water composition array; character elements including single letters
#' indicate presence of a corresponding plant species while NA indicates water, "" indicates absolute absence
#' @usage run.plant.ecosystem(terrain, timesteps, survival, reproduciton, competition)
#' @examples rworld <- run.plant.ecosystem(terrain = make.terrain(4), timesteps=100, survival=c(.5,.5), reproduction=c(.5,.5), competition=matrix(c(.50,.50,.50,.50), 2))
#' @export


run.plant.ecosystem <- function(terrain, timesteps, survival, reproduction, competition){

  reproduction <- reproduction
  survival <- survival
  competition <- competition
  rownames(competition) <- c("a","b")
  colnames(competition) <- c("a","b")
  setup.plants <- function(reproduction, survival, competition, names=NULL){

    if(is.null(names))
      names <- letters[seq_along(reproduction)]
    if(length(reproduction) != length(survival))
      stop("Reproduction and survival parameters needed for all species")
    if(is.matrix(competition) == FALSE)
      stop("The Matrix Has You")
    reproduction <- setNames(reproduction, names)
    survival <- setNames(survival, names)

    return(list(reproduction=reproduction, survival=survival, competition=competition,
                species=names))
  }

  info <- setup.plants(reproduction, survival, competition)

  ## Creating species/water composition matrix

  composition <- as.matrix((expand.grid(info$species, "")))
  composition <- matrix(c(sample(composition[,1], nrow(terrain), replace=T),sample(composition[,2], nrow(terrain), replace=T)),nrow(terrain),ncol(terrain))
  composition <- matrix(sample(composition), nrow(terrain), ncol(terrain))
  composition[,][is.na(terrain)] <- NA

  ### Plant time step function ####

  plant.timestep <- function(composition, terrain, info, timesteps){
    plants <- array("", dim=c(dim(terrain),timesteps+1))
    #####
    pop.survival <- function(composition, info){
      plantmatrix <- composition
      survive <- function(cell, info){
        if(cell  == "" | is.na(cell) == TRUE){
        } else if (runif(1) > info$survival[cell]) {
          cell <- ""
        } else {
          cell <- cell
        }
        return(cell)
      }
      for (i in 1:length(composition)){
        plantmatrix[i] <- survive(composition[i], info)
      }
      return(plantmatrix)
    }
    ######
    pop.repro <- function(composition, info){
      reproduction <- function(cell, info, composition, position){
        new.location <- NA
        if(cell  == "" | is.na(cell) == TRUE){
          cell <- cell
        } else if (runif(1) <= info$reproduction[cell]) {
          possible.locations <- as.matrix(expand.grid(position+c((-1*nrow(composition)),-1,1,nrow(composition))))
          possible.locations <- as.matrix(possible.locations[possible.locations[, 1] <= length(composition), ])
          possible.locations <- as.matrix(possible.locations[possible.locations[, 1] >= 1, ])
          new.location <- possible.locations[sample(seq(1,nrow(possible.locations)),1),]
        }else if (runif(1) > info$reproduction[cell]){
          new.location <- position
        }
        if(cell  == "" | is.na(cell) == TRUE){
          cell <- cell
        }else if(identical(composition[new.location], cell) == TRUE){
          composition[new.location] <- cell
        }else if(composition[new.location] != "" && is.na(composition[new.location]) == FALSE){
          composition[new.location] <- sample(info$species, 1, prob=info$competition[cell,])
        } else if (is.na(cell)==FALSE){
          composition[new.location] <- cell
        }
        return(composition)
      }
      plantmatrix <- composition
      for (i in 1:length(plantmatrix)){
        plantmatrix <- reproduction(plantmatrix[i], info, plantmatrix, i)
      }
      return(plantmatrix)
    }
    #####
    plants[,,1] <- pop.survival(composition, info)
    plants[,,1][is.na(terrain)] <- NA
    plants[,,1] <- pop.repro(plants[,,1], info)
    plants[,,1][is.na(terrain)] <- NA
    for(i in (2:(timesteps+1))){
      plants[,,i] <- pop.survival(plants[,,i-1], info)
      plants[,,i][is.na(terrain)] <- NA
      plants[,,i] <- pop.repro(plants[,,i-1], info)
      plants[,,i][is.na(terrain)] <- NA
    }
    return(plants)
  }
  plant.timestep(composition, terrain, info, timesteps)
}


