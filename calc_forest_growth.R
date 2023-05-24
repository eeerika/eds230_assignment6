#' Calculate forest growth model
#' @param time time since start
#' @param C size of the forest (units of carbon)
#' @param threshold canopy closure threshold ie. size of the forest at which growth rates change from exponential to linear (units of carbon)
#' @param K carrying capacity (units of carbon)
#' @param r early exponential growth rate
#' @param g linear growth rate once canopy closure has been reached
#' @param parms list with four values: threshold, K, r, and g 
#' @return derivative of population with time
#' 
#' 
calc_forest_growth = function(time, C, parms) {
  
  # Run equation when C is below a threshold canopy closure
  if(C < parms$threshold){
    dC_dt = parms$r * C
  }
  # Run equation when C is at or above the threshold canopy closure
  else{
    dC_dt = parms$g * (1 - (C / parms$K))
  }
  
  return(list(dC_dt))
}