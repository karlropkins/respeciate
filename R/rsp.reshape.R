#' @name rsp.reshape
#' @title Reshaping respeciate data sets
#' @aliases rsp_dcast rsp_dcast_profile rsp_dcast_profile_id rsp_dcast_species
#' rsp_dcast_species_id rsp_melt_wide

#' @description Functions for reshaping respeciate profiles

#' @description \code{rsp_dcast} and \code{rsp_melt_wide} reshape supplied
#' respeciate profile(s). \code{rsp_dcast} converts these from their supplied
#' long form to a widened form, \code{dcast}ing the data set by either species
#' or profiles depending on the \code{widen} setting applied.
#' \code{rsp_dcast_profile}, \code{rsp_dcast_profile_id},
#' \code{rsp_dcast_species} and \code{rsp_dcast_species_id} are wrappers for
#' these options. \code{rsp_melt_wide} attempts to return a previously widened data
#' set to the original long form.
#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles in standard long form or widened form using
#' \code{\link{rsp_dcast}} and \code{\link{rsp_melt_wide}}, respectively.
#' @param widen character, when widening \code{rsp} with
#' \code{\link{rsp_dcast}}, the data type to \code{dcast},
#' currently \code{'species'} (default), \code{'species.id'}, \code{'profile'}
#' or \code{'profile.id'}. See Note.
#' @param pad logical or character, when \code{melt}ing a previously widened
#' data set, should output be re-populated with species and/or profile
#' meta-data, discarded when widening. This is currently handled by
#' \code{\link{rsp_pad}}. The default \code{FALSE} does not pad, \code{TRUE}
#' pads, applies standard settings, so does not include profile sources
#' reference meta-data. (See \code{\link{rsp_pad}} for other options).
#' @param drop.nas logical, when \code{melt}ing a previously widened
#' data set, should output be stripped of any rows containing empty
#' weight/value columns. Because not all profile contains all species, the
#' \code{dcast}/\code{melt} process can generate empty rows, and this step
#' attempt account for that when working with standard \code{reSPECIATE}
#' profiles. It is, however, sometimes useful to check first, e.g. when
#' building profiles yourself.
#' @return \code{rsp_dcast} returns the wide form of the supplied
#' \code{respeciate} profile. \code{rsp_melt_wide}
#' returns the (standard) long form of a previously widened profile.

#' @note Conventional long-to-wide reshaping of data, or \code{dcast}ing, can
#' be slow and memory inefficient. So, \code{respeciate} uses the
#' \code{\link[data.table:dcast]{data.table::dcast}}
#' method. The \code{rsp_dcast_species} method,
#' applied using \code{widen='species'}, is effectively:
#'
#' \code{dcast(..., .profile.id+.profile~.species, value.var=".value")}
#'
#' And, the alternative \code{widen='profile'}:
#'
#' \code{dcast(..., .species.id+.species~.profile, value.var=".value")}
#'
#' \code{respeciate} uses a local version of the \code{SPECIATE} and
#' \code{SPECIEUROPE} weight measurements \code{.value}, so the EPA and
#' JCR source information can easily be recovered. See also
#' \code{\link{rsp_rescale_profile}}.
#'
#' @references
#'   Dowle M, Srinivasan A (2023). _data.table: Extension of `data.frame`_.
#'   R package version 1.14.8, <https://CRAN.R-project.org/package=data.table>.

#NOTE

#############################
# these use
#############################
# .rsp_tidy_profile
# data.table::as.data.table
# data.table::dcast
# data.table::melt
# data.table::merge.data.table
# rsp_pad

# need to set data.table specifically??
#      data.table::as.data.table, etc??

# in development
# extended handling species and profile to
#     species, species.id, profile and profile.id]

# might be an issue with rsp_dcast_profile
#     if profile names are not unique

#might be broken
#see
# a <- rsp(1:50)
# summary(a)
# summary(rsp_melt_wide(rsp_dcast_profile_id(a))) # nearly works!!!
# but next few don't
# summary(rsp_dcast_species(a))
# summary(rsp_melt_wide(rsp_dcast_species(a)))
# as.data.frame(head(rsp_melt_wide(rsp_dcast_species_id(a)),200)) #NANs in there...



#maybe think about padding rsp_dcast_profile

######################
#dcast
#long_to_wide reshape
######################

#' @rdname rsp.reshape
#' @export

rsp_dcast <- function(rsp, widen = "species"){

  ####################
  #see ?data.table::dcast for examples
  ####################

  # currently running with any object type as rsp
  # but likely errors out with anything but a respeciate object...

  #note: should this handle non-respeciate objects?
  #      maybe not but with a force option to (at own risk) override??

  #note: thinking about adding formal to set the wide term
  #      so user can set the dcast term, e.g. ~ species or profile
  #      name key? wide? variable.name?

  #note: think there bit be an issue is anything is not unique
  #

  ######################
  # SPECIEUROPE data
  ######################
  if("rsp_eu" %in% class(rsp)){
    rsp <- .rsp_eu2us(rsp)
  }
  #######################

  #adds .value if missing
  ## using .value rather the WEIGHT_PERCENT in case rescaled
  x <- .rsp_tidy_profile(rsp)


  #save class
  cls <- class(x)

  xx <- data.table::as.data.table(x)

  #stop if widen option not known.
  if(!widen %in% c("species", "species.id", "profile", "profile.id")){
    stop("unknown widen option")
  }
  if(widen=="species"){
    cmt <- paste(xx$.species, xx$.species.id, sep="--rsp--")
    cmt <- c(".species--rsp--.species.id", cmt)
    ncl <-  "rsp_sw"
    out <- data.table::dcast(xx,
                 .profile.id + .profile ~ .species,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }
  if(widen=="species.id"){
    cmt <- paste(xx$.species.id, xx$.species, sep="--rsp--")
    cmt <- c(".species.id--rsp--.species", cmt)
    ncl <-  "rsp_siw"
    out <- data.table::dcast(xx,
                             .profile.id + .profile ~ .species.id,
                             mean,
                             na.rm=TRUE,
                             value.var = ".value")
  }
  if(widen=="profile"){
    cmt <- paste(xx$.profile, xx$.profile.id, sep="--rsp--")
    cmt <- c(".profile--rsp--.profile.id", cmt)
    ncl <- "rsp_pw"
    out <- data.table::dcast(xx,
                             .species.id + .species ~ .profile,
                             mean,
                             na.rm=TRUE,
                             value.var = ".value")
  }
  if(widen=="profile.id"){
    cmt <- paste(xx$.profile.id, xx$.profile, sep="--rsp--")
    cmt <- c(".profile.id--rsp--.profile", cmt)
    ncl <- "rsp_piw"
    out <- data.table::dcast(xx,
                 .species.id + .species ~ .profile.id,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }

  ################################################
  # testing
  # using comments to track the dropped rsp term
  ################################################

  #output
  # convert to data frame, add cmt as source for dropped info
  # class what came in plus the rsp_.w identifier
  # we might not need to keep the rsp_.w identifier or use a general rsp_wide???
  out <- as.data.frame(out)
  comment(out) <- unique(cmt)
  class(out) <- c(ncl, cls)
  out
}

######################################
# rsp_dcast(..., widen) shortcuts
######################################

#' @rdname rsp.reshape
#' @export

rsp_dcast_profile <- function(rsp, widen = "profile"){
  rsp_dcast(rsp=rsp, widen=widen)
}

#' @rdname rsp.reshape
#' @export

rsp_dcast_profile_id <- function(rsp, widen = "profile.id"){
  rsp_dcast(rsp=rsp, widen=widen)
}

#' @rdname rsp.reshape
#' @export

rsp_dcast_species <- function(rsp=rsp, widen = "species"){
  rsp_dcast(rsp=rsp, widen=widen)
}

#' @rdname rsp.reshape
#' @export

rsp_dcast_species_id <- function(rsp=rsp, widen = "species.id"){
  rsp_dcast(rsp=rsp, widen=widen)
}




##   now imports from xxx.r
##   #' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

# wanted melt to go with dcast
#    curently just to reverse the dcast action
#    see e.g.
#    https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html

#padding needs work
#

######################
#melt
#wide_to_long reshape
######################

#' @rdname rsp.reshape
#' @export

rsp_melt_wide <- function(rsp, pad = FALSE, drop.nas = FALSE){

  ####################
  #see ?data.table::melt for examples
  ####################

  #setup
  x <- .rsp_tidy_profile(rsp)
  cls <- class(x)
  cmt <- comment(x)
  xx <- data.table::as.data.table(x)

  #################
  #test log/wide
  ####################
  #should be able to simplify this a lot
  .test <- c(".profile", ".profile.id", ".species.id", ".species",
             ".pc.weight", ".value")
  .test <- .test[.test %in% names(xx)]
  if(length(.test)>2){
    stop("RSP> melt halted; rsp already looks like a long profile.", call.=FALSE)
  }
  .test.sp <- length(grep("profile", .test))
  .test.pr <- length(grep("species", .test))
  if(.test.pr>0 & .test.sp>0){
    stop("RSP> melt halted; rsp looks looks suspect.", call.=FALSE)
  }
  .long <- "bad"
  if(.test.pr>0 & length(.test)==.test.pr){
    .id.vars <- .test
    .long <- ".profile.id"
  }
  if(.test.sp>0 & length(.test)==.test.sp){
    .id.vars <- .test
    .long <- ".species"
  }
  if(.long=="bad"){
    stop("RSP> melt halted; rsp looks suspect.", call.=FALSE)
  }

  #should only be species.wide or profile.wide
  #   if we get to here

  if(!is.null(cmt)){
    # we have a tracked record of the dropped term
    # use this as source of .long
    cmt <- strsplit(cmt, "--rsp--")
    .nms <- cmt[[1]]
    .long <- .nms[1]
    cmt <- as.data.frame(t(as.data.frame(cmt)))
    names(cmt) <- .nms
  }

  out <- data.table::melt(xx, id.vars = .id.vars)
  names(out)[names(out)=="variable"] <- .long
  names(out)[names(out)=="value"] <- ".value"
  if(!is.null(cmt)){
    out <- data.table::merge.data.table(out, cmt, all.x=TRUE)
  }
  if(".species.id" %in% names(out)){
    out$.species.id <- as.character(out$.species.id)
  }
  if(".species" %in% names(out)){
    out$.species <- as.character(out$.species)
  }
  if(".profile.id" %in% names(out)){
    out$.profile.id <- as.character(out$.profile.id)
  }
  if(".profile" %in% names(out)){
    out$.profile <- as.character(out$.profile)
  }


  #out$WEIGHT_PERCENT <- out$.value

  #merge if padding
  #####################
  #might not be best way of doing it
  #   could pass other args to pad
  #   might need to think about the .value/WEIGHT_PERCENT handling

  if(is.logical(pad) && pad){
    pad <- "standard"
  }
  if(is.character(pad)){
    out <- rsp_pad(out, pad, drop.nas)
    #tidy bad profile_name
    if(all(is.na(out$.profile)) && ".profile.id" %in% names(out)){
      out$.profile <- out$.profile.id
    }
    #tidy bad species_id
    if(all(is.na(out$.species.id)) && ".species" %in% names(out)){
      out$.species.id <- as.character(-as.numeric(factor(out$.species)))
    }

  }

  ################################
  # could tidy structure here??
  ################################

  # if weight_percent but not .value add .value
  # if.value but not weight_percent add .value
  # similar for profile_name/code and species_name/id

  # is that done in rsp_build_x ??


  #drop.nas...
  if(drop.nas){
    if(".value" %in% names(out)){
      out <- out[!is.na(out$.value),]
    } else {
      if(".pc.weight" %in% names(out)){
        out <- out[!is.na(out$.pc.weight),]
      }
        #do we want to warn if nothing to strip
        #if so, in else here??
    }
  }

  #output
  #need to rationalise outputs!!!
  #.rsp_build_respeciate(out)
  out <- as.data.frame(out)
  class(out) <- cls[!cls %in% c("rsp_pw", "rsp_piw", "rsp_sw", "rsp_siw")]
  out
}


########################
#unexported, old code and text notes
#######################

#test data 219 records
#aa <- rsp_profile(rsp_find_profile("ae6", by="profile_type"))

#idiot-test reference.

#I don't think I am using it anywhere...

#########################
#testing getting rid of this
#########################

#rsp_build_wide_profile <- function(x){
#  .usp <- unique(x$SPECIES_NAME)
#  .upr <- unique(x$PROFILE_CODE)
#  ref <- data.frame(t(rep(NA, length(.usp))))
#  names(ref) <- .usp
#  ans <- lapply(.upr, function(.pr){
#    temp <- x[x$PROFILE_CODE==.pr,]
#    out <- data.frame(t(temp$WEIGHT_PERCENT))
#    names(out) <- temp$SPECIES_NAME
#    out<- modifyList(ref, out)
#    out$PROFILE_CODE <- .pr
#    out$PROFILE_NAME <- temp$PROFILE_NAME[1]
#    out
#    #ref
#  })
#  do.call(rbind, ans)
#}

#require(dplyr)

#aa %>% group_by(PROFILE_CODE, SPECIES_ID) %>%
#   summarise(PROFILE_NAME = PROFILE_NAME[1],
#             SPECIES_NAME = SPECIES_NAME[1],
#             SPEC_MW = SPEC_MW[1],
#             total = sum(WEIGHT_PERCENT, na.rm=T),
#             mean = mean(WEIGHT_PERCENT, na.rm=T),
#             sd = sd(WEIGHT_PERCENT, na.rm=T),
#             n = length(WEIGHT_PERCENT[!is.na(WEIGHT_PERCENT)]))

#require(data.table)

#test_wide <- function(x){
  ##as above
  #####################
  ##see ?data.table::dcast for examples
  #####################
  #xx <- as.data.table(x)
  #out <- dcast(xx,
  #             PROFILE_CODE + PROFILE_NAME ~SPECIES_NAME,
  #             mean,
  #             na.rm=TRUE,
  #             value.var = "WEIGHT_PERCENT")
  ##maybe use species id in dcast
  ##then add matched species names, making unique??
  ##you can keep profile but not species info...
  #as.data.frame(out)
#}

#plot(test_wide(aa)$Iron, rsp_build_wide_profile(aa)$Iron)
## ~ 2 seconds

#aa <- aa <- sp_profile(find_sp_profile("gas", by="profile_type"))
## 2641 profiles
## (full archive 6855)
#plot(test_wide(aa)$Formaldehyde)
## much faster than
#plot(rsp_build_wide_profile(aa)$Formaldehyde)
#plot(test_wide(aa)$Formaldehyde, rsp_build_wide_profile(aa)$Formaldehyde)
##straight y=x
#
