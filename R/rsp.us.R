#' @name rsp.us
#' @title Quick access to common SPECIATE subsets.
#' @aliases rsp_us rsp_us_gas rsp_us_other rsp_us_pm rsp_us_pm.ae6 rsp_us_pm.ae8
#' rsp_us_pm.cr1 rsp_us_pm.simplified

#' @description \code{rsp_us_} functions are quick access wrappers to commonly
#' requested SPECIATE subsets.

#' @return \code{rsp_us_} functions typically return a \code{respeciate}
#' \code{data.frame} of the requested profiles.
#'
#' For example:
#'
#' \code{rsp_us_gas()} returns all gaseous profiles in SPECIATE
#' (\code{PROFILE_TYPE == 'GAS'}).
#'
#' \code{rsp_us_pm} returns all particulate matter (PM) profiles in SPECIATE
#' not classified as a special PM type (\code{PROFILE_TYPE == 'PM'}).
#'
#' The special PM types are subsets profiles intended for special
#' applications, and these include \code{rsp_us_pm.ae6} (type \code{PM-AE6}),
#' \code{rsp_us_pm.ae8} (type \code{PM-AE8}), \code{rsp_us_pm.cr1} (type
#' \code{PM-CR1}), and \code{rsp_us_pm.simplified} (type \code{PM-Simplified}).
#'
#' \code{rsp_us_other} returns all profiles classified as other in SPECIATE
#' (\code{PROFILE_TYPE == 'OTHER'}).
#'


#############################
#NOTES
############################

# might not be keeping these

# SPECIATE profile types
# GAS, OTHER, PM, PM-AE6, PM-AE8, PM-CR1, PM-Simplified


##########################
# to do / think about ...
#########################

# any others worth doing???
#     others from SPECIATE (as rsp_us_... ???)
#     anything similar in SPECIEUROPE (as rsp_eu_... ???)

# feels like ther should be a quicker way of doing this...
#    maybe try going SPECIATE directly instead of using rsp_profile_info???
#       BUT might not be much a speed saving...

# rsp() or rsp_profile() ????


#' @rdname rsp.us
#' @export

rsp_us_gas <- function(){
  rsp_profile(rsp_profile_info("gas", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_other <- function(){
  rsp_profile(rsp_profile_info("other", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_pm <- function(){
  rsp_profile(rsp_profile_info("pm", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_pm.ae6 <- function(){
  rsp_profile(rsp_profile_info("pm-ae6", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_pm.ae8 <- function(){
  rsp_profile(rsp_profile_info("pm-ae8", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_pm.cr1 <- function(){
  rsp_profile(rsp_profile_info("pm-cr1", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.us
#' @export

rsp_us_pm.simplified <- function(){
  rsp_profile(rsp_profile_info("pm-simplified", by = "profile_type", partial=FALSE))
}




