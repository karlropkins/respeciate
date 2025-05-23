#' @name rsp.plot
#' @title plotting respeciate source profiles
#' @aliases rsp_plot rsp_plot_profile rsp_plot_species rsp_plot_match

#' @description General plots for \code{respeciate} objects.
#' @description \code{rsp_plot} functions generate plots for supplied
#' respeciate data sets.

#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles.
#' @param id numeric, the indices of profiles or species to use when
#' plotting with \code{rsp_plot_profile} or \code{rsp_plot_species},
#' respectively. For example, \code{rsp_plot_profile(rsp, id=1:6)} plots
#' first 6 profiles in \code{respeciate} object \code{rsp}.
#' @param multi.profile character, how \code{rsp_plot_profile} should
#' handle multiple profiles, e.g. 'group' or 'panel' (default
#' group).
#' @param order logical, order the species in the
#' profile(s) by relative abundance before plotting.
#' @param log logical, log y scale when plotting.
#' @param ... any additional arguments, typically passed on the lattice
#' plotting functions.
#' @param silent logical, hide warnings when generating plots (default
#' \code{FALSE})
#' @param output character, output method, one of: 'plot' to return just the
#' requested plot; 'data' to return just the data; and, c('plot', 'data') to
#' plot then return the data invisibly (default).
#' @param multi.species, character, like \code{multi.profile} in
#' \code{rsp_plot_profile} but for species in \code{rsp_plot_species}.
#' @param ref \code{respeciate} or similar data set of profiles, used by
#' \code{rsp_match_plot} as a reference when comparing with \code{rsp}. See
#' \code{\link{rsp_match_profile}} for further details and other matching
#' arguments.
#' @param plot.type numeric, option if the \code{rsp_plot...} function includes
#' different plot reports.
#' @return \code{rsp_plot} graph, plot, etc usually as a trellis object.

#' @note These functions are currently in development, so may change.

#' @references Most \code{respeciate} plots make extensive use of
#' \code{lattice} and \code{latticeExtra} code:
#'
#' Sarkar D (2008). \emph{Lattice: Multivariate Data Visualization with R}.
#' Springer, New York. ISBN 978-0-387-75968-5, \url{http://lmdvr.r-forge.r-project.org}.
#'
#' Sarkar D, Andrews F (2022). \emph{latticeExtra: Extra Graphical Utilities Based
#' on Lattice}. R package version 0.6-30,
#' \url{https://CRAN.R-project.org/package=latticeExtra}.
#'
#' They also incorporate ideas from \code{loa}:
#'
#' Ropkins K (2023). \emph{loa: various plots, options and add-ins for use with lattice}.
#' R package version 0.2.48.3, \url{https://CRAN.R-project.org/package=loa}.


#functions
# rsp_plot_profile
# rsp_plot_species
# rsp_plot_match

# plot.respeciate is wrapper for rsp_plot_profile

#uses unexported code
#   (now any unexported code (.rsp...) should be in xxx.r)

#  .rsp_plot_fix
#  .rsp_yscale.component.log10 (currently in rsp.pls.r)
#  .rsp_plot_output
#     (so need to update them all if .rsp function formals change)
#  .rsp_panelPal
#     (this is a very crude loa::panelPal/listHandler)

#JOBS
#######################

#  common approaches needed for ordering, subsetting and renaming
#       for both species and profiles when plotting.
#       also like to make this same for these and pls_plots
#            so more consistent

#references may need formatting tidying
#   currently these are lattice, latticeEXtra and loa...
#   check roxygen2 guidance ???

#all functions need work
#see function fix, tidy, etc job notes in code
#     ALL need better colour handling for large numbers of cases
#         typically group handling...
#            maybe a variation on col=rainbow ???

#examples
# maybe
# rsp_plot_profile(spq_pm.ae8())
#   (allows most lattice style plot control, etc key=list(...))
#   (but includes some short cuts to common handling, e.g. log=T to
#      log y scales and reformat y axes)
# rsp_plot_profile(rsp_q_pm.ae8(), key=list(space="top", columns=2), log=T)

#color defaults...
#issue current default wraps if you exceed number of cols in default set.
#from: https://stackoverflow.com/questions/26314701/r-reducing-colour-saturation-of-a-colour-palette
#function(x) colorRampPalette(rainbow(12, s = 0.5, v = 1)[2:11],interpolate = "spline")(x)
##   ?? could extrapolate the default colors using something like above ???


##############################
# to think about
##############################

# box plot as rsp_plot_species option maybe plot.type=1, default.

# common approaches for ordering subsetting and renaming
#       for both species and profiles when plotting.
#       also like to make this and pls__plot consistent


# dennis asked for data as part of return
#    that is do-able but may need an object class
#         (maybe like the openair code...)

#thinking about an rsp_plot_compare(x, y)
#  to compare profile x and profile(s) y
#  started project (in own-notes)
#    currently doing as rsp_match_profile plot output...

# think about tidying out from code...
#     plot(rsp_q_pm.ae8(), log=T, key=list(space=NULL, x=0.45,y=0.95))
#         key argument needs to be a list... could be key. arguments...
#         and key(space) needs to be null because default space value overrides...
#         also looks like too much white space below bottom of plot
#         also not sure how many of the plot functions do same....

#possible issue
#     check order = TRUE/FALSE behavior...
#          rsp_plot_profile seems OK but...
#          rsp_plot_species seem to be ignoring this, but be careful...
#               need to check it works in all combinations





###################################
#rsp_plot_profile
###################################


##########################
#notes
##########################
#moved this to lattice for paneling option

############################
#using .rsp_plot_fix for warning/handling for
#  duplicate species in profiles (handling merge/mean)
#  duplicated profile names (handling make unique)

#############################
#using .rsp_test_profile
#   when ordering...

#see in code notes about jobs

##############################
#testing
#   reset.x as option to change
#        x access handing
#        wondering about a general fix
#             upfront so applied to x (rsp data.frame)
#               what is x, how is it formatted, etc
#               then same for y, groups and cond...

#' @rdname rsp.plot
#' @export

rsp_plot_profile <-   function(rsp, id, multi.profile = "group",
                              order=TRUE, log=FALSE, ...,
                              silent=FALSE, output="default"){

  #setup
  x <- rsp ## this needs sorting...
  ######################
  # SPECIEUROPE data
  ######################
  if("rsp_eu" %in% class(x)){
    x <- .rsp_eu2us(x)
  }
  #######################
  x <- .rsp_plot_fix(x, silent=silent, ...)
  .x.args <- list(...)

  #currently not even trying to stack logs...
  if("stack" %in% names(.x.args) && .x.args$stack){
    if(log){
      ######################
      #to do
      #document issues
      stop("RSP> Sorry, currently not stacking log plots",
           call. = FALSE)
    }
  }
  #others refs
  #was profile_code; changed to profile_name
  #    might be an issue; some names not unique...
  .sp.pro <- if(is.factor(x$.profile)) {
    levels(x$.profile)
  } else {
    unique(x$.profile)
  }

  #n/profile handling
  profile <- if (missing(id)) {
    .sp.pro
  } else {
    id
  }
  if (is.numeric(profile)) {
    profile <- if (all(profile == -1)) {
      .sp.pro
    }
    else {
      .sp.pro[profile]
    }
  }
  if (!any(profile %in% .sp.pro) | any(is.na(profile))) {
    stop("RSP> unknown profile(s) or missing ids, please check",
         call. = FALSE)
  }
  if(length(profile)>6 & missing(id)){
    if(!silent){
      warning("RSP> ", length(profile), " profiles... ",
              "just showing first 6 to reduce plot clutter",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
    }
    profile <- profile[1:6]
  }
  x <- x[x$.profile %in% profile,]

  #check for duplicates, etc...
  #tidy naming etc...
  x <- .rsp_plot_fix(x, silent=silent, ...)

  ##test something to plot
  if(nrow(x)==0){
    ######################
    #think about this
    ######################
    #maybe stop() instead???
    #stop("empty respeciate object?")
    #maybe warning() aw well??
    return(invisible(NULL))
  }



  ####################################
  #switching profile from profile_code to profile_name...
  #    for plot labeling
  ####################################
  #profile <- unique(x$PROFILE_NAME)
  #should think about other naming options???
  #(now using profile_name from start)

  #order largest to smallest
  #############################
  #like to also be able to order by molecular weight
  ##############################
  if(order){
    ################################
    #bit of a cheat...
    ################################
    test <- x
    test$.profile.id <- ".default"
    test <- .rsp_test_profile(test)
    #previous barplot had bedside
    if("stack" %in% names(.x.args) && .x.args$stack){
      test <- test[order(test$.total, decreasing = TRUE),]
      xx <- unique(test$.species)
    } else {
      test <- x[order(x$.pc.weight, decreasing = TRUE),]
      xx <- unique(test$.species)
    }
  } else {
    xx <- unique(x$.species)
  }
  #x <- x[c(".value", ".species", ".species")]

  #print(x$.profile)

  x$.species <- factor(x$.species,
                           levels = xx)
  if(!is.factor(x$.profile)){
    x$.profile <- factor(x$.profile, levels=unique(x$.profile))
  }
  #should profile handling be like species_name?
  #    maybe following profile above??

#print(as.data.frame(x))
  ##################
  #profile bar chart
  ##################
  p1.ls <- list(x= .value~.species,
                data=x, ylab="Profile Loading", xlab="",
                #NB: prepanel seemed to break ylim when stacking
                panel = function(x, y, origin, ...){
                  .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                               panel.grid, ...)
                  if(missing(origin)){
                    ######################################
                    #we are getting -Inf from somewhere...
                    ######################################
                    .y <- y[is.finite(y)]
                    origin <- if(min(.y, na.rm=TRUE) < 0 ) {
                      min(.y, na.rm=TRUE) - 0.02
                    } else {
                      0
                    }
                  }
                  panel.barchart(x=x, y=y, origin=origin, ...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=90,
                                   cex=0.7,
                                   alternating=1),
                            y=list(rot=c(0,90),
                                   cex=0.7,
                                   alternating=3,
                                   relation="free"))
  )

  #1.if multiple profiles, panel or group
  if(length(profile)>1){
    if(tolower(multi.profile) %in% c("panel", "panels")){
      #paneling multiple panels
      p1.ls$x <- .value~.species | .profile
    } else {
      #grouping multiple panels
      p1.ls$x <- .value~.species
      p1.ls$groups <- x$.profile
    }
  }

  #2. if log=TRUE
  #shortcut to scales(y(log)) and yscale.component
  if(log){
    p1.ls$scales$y$log <- 10
    p1.ls$yscale.components <- .rsp_yscale.component.log10
  }

  #3. extra user settings
  p1.ls <- modifyList(p1.ls, .x.args)

  #4. tidy loose ends

  #set cols
  if("col" %in% names(p1.ls)){
    if(is.function(p1.ls$col)){
      p1.ls$col <- if("groups" %in% names(p1.ls)){
        p1.ls$col(length(levels(x$.profile)))
      } else {
        p1.ls$col(1)
      }
    }
  } else {
    p1.ls$col <- if("groups" %in% names(p1.ls)){
      rep(trellis.par.get("superpose.polygon")$col,
          length.out=length(levels(x$.profile)))
    } else {
      trellis.par.get("superpose.polygon")$col[1]
    }
  }
  #here we must have col
  #################################
  #issue here if user sets par.settings
  .tmp <- list(superpose.polygon = list(col = p1.ls$col),
               superpose.symbol = list(fill = p1.ls$col))
  p1.ls$par.settings <- if("par.settings" %in% names(p1.ls)){
    modifyList(.tmp, p1.ls$par.settings)
  } else {
    .tmp
  }
  #add key if needed
  #################################
  #issue if user uses key = FALSE: should remove..
  #could be issue here if user uses auto.key???
  #issue if user wants to use x and y to put key in plot: also needs space=NULL
  #like to track border as well as col...
  #want to add box behind legend when key in plot...
  if("groups" %in% names(p1.ls)){
    .tmp <- list(space="top",
                 #title="Legends",
                 rectangles=list(col=rep(p1.ls$col,
                                         length.out=length(levels(x$.profile)))),
                 text = list(profile, cex=0.7))
    p1.ls$key <- if("key" %in% names(p1.ls)){
      modifyList(.tmp, p1.ls$key)
    } else {
      .tmp
    }
  }

  #output
  ##################
  p <- do.call(barchart, p1.ls)
  .rsp_plot_output(as.data.frame(p1.ls$data), p1.ls, p, output)
}



###################################
#rsp_plot_species
###################################

#' @rdname rsp.plot
#' @export

#in development

#lot taken straight from sp_plot_profile
#so lots of redundancy

rsp_plot_species <- function(rsp, id, multi.species = "group",
                            order = FALSE, log = FALSE,
                            ..., silent = FALSE, output = "default"){

  #setup
  x <- rsp ## this needs sorting...
  ######################
  # SPECIEUROPE data
  ######################
  if("rsp_eu" %in% class(x)){
    x <- .rsp_eu2us(x)
  }

  x <- .rsp_plot_fix(x, silent=silent, ...)
  .x.args <- list(...)

  ######################################
  #not sure we are using stack for this...
  ######################################
  #currently not even trying to stack logs...
  if("stack" %in% names(.x.args) && .x.args$stack){
    if(log){
      ######################
      #to do
      #document issues
      stop("RSP> Sorry, currently not stacking logs",
           call. = FALSE)
    }
  }

  #need to get species as character
  ##############################
  #if already factor ???
  #   user could be forcing order
  ##############################
  .sp.ord <- if(is.factor(x$.species)){
    levels(x$.species)
  } else {
    as.character(unique(x$.species))
  }
  species <- if (missing(id)) {
    .sp.ord
  } else {
    id
  }

  if (is.numeric(species)) {
    species <- if (all(species == -1)) {
      .sp.ord
    }
    else {
      .sp.ord[species]
    }
  }

  #########################
  # split previous warning
  if (!any(species %in% .sp.ord)){
    print(.sp.ord)
    stop("RSP> unknown species(s) requested, please check",
         call. = FALSE)
  }
  if(any(is.na(species))){
    species <- species[!is.na(species)]
    if(length(species)<1){
      stop("RSP> Too may suspect species(s) found, halting plot...",
           "\n\t (maybe bad or corrupt'rsp'?)",
           call. = FALSE)
    }
    warning("RSP> Suspect species(s) found, discarding for plot...",
         "\n\t (maybe check 'rsp' if a concern?)",
         call. = FALSE)
  }


  if(length(species)>20 & missing(id)){
    if(!silent){
      warning("RSP> ", length(species), " species... ",
              "just showing first 20 to reduce plot clutter",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
    }
    species <- species[1:20]
  }
  x <- x[x$.species %in% species,]

  #check for duplicates, etc...
  #tidy naming etc...
  x <- .rsp_plot_fix(x, silent=silent, ...)

  ##test something to plot
  if(nrow(x)==0){
    ######################
    #think about this
    ######################
    #maybe stop() instead???
    #stop("empty respeciate object?")
    #maybe warning() aw well??
    return(invisible(NULL))
  }

  ####################################
  #current species ordering by name arg...
  #(see below about reordering)
  ####################################

  species <- species[species %in% unique(x$.species)]
  x$.species <- factor(x$.species, levels=species)
  x <- x[order(x$.species),]
  #sp.ord <- as.numeric(factor(species, levels=sort(species)))
  #sp.ord <- 1:length(levels(x$.species))


  ##################################
  #should think about other naming options???
  ##################################
  #print(species)
  #print(sp.ord)

#ignoring option to re-order at moment

  #order largest to smallest
  #############################
  #like to enable this
  #like to also be able to order by molecular weight
  #need to decide handling if species is already a factor... ???
  #need to decide if this should work off species_id or species_name... ???
  ##############################
  if(order){
    ################################
    #taken from _profile plots
    ################################
    test <- x
    test$.profile.id <- ".default"
    test <- .rsp_test_profile(test)
    #previous barplot had bedside
    if("stack" %in% names(.x.args) && .x.args$stack){
      test <- test[order(test$.total, decreasing = TRUE),]
      xx <- unique(as.character(test$.species))
    } else {
      test <- x[order(x$.pc.weight, decreasing = TRUE),]
      xx <- unique(as.character(test$.species))
    }
  } else {
    xx <- unique(as.character(x$.species))
  }
  x <- x[c(".value",".profile.id", ".profile", ".species")]


  #print(xx)

  ##################
  #species trend line plot
  ##################

  #dcast and melt to add in any missed entries as NAs
  #(to force trend line gaps)
  #not padding, obviously not dropping nas...
  x <- rsp_melt_wide(rsp_dcast_species(x), pad=FALSE, drop.nas = FALSE)

  if(!is.factor(x$.profile)){
    x$.profile <- factor(x$.profile, levels=unique(x$.profile))
  }
  ################
  #testing tracking
  ##if(!is.factor(x$.species)){
  ##  x$.species <- factor(x$.species, levels=unique(x$.species))
  ##}
  x$.species <- factor(as.character(x$.species),
                           levels=xx)
  #################

  ###############################
  #species handling
  ##############################

  #dropped for now...
  #x$.species <- factor(x$.species,
  #                         levels = xx)

  #############################
  # x axis handling
  #############################

  #currently calculating and using sample index
  #could add alternatives
  #  use profile_code column as is
  #     x$x.id <- x$PROFILE_CODE
  #        would also want an xlab change....
  #  use a different column?
  #  convert to factor
  #       but then by default lattice shows all factors labels...
  #  format using a supplied function???

  ############################
  #could move this top and apply
  #before plotting??

  if("reset.x" %in% names(.x.args)){
    #initial test reset.x
    #  this is a function and it is applied to profile_code
    #      to build the x axis...
    x$.x <- .x.args$reset.x(x$.profile.id)
    .xlab <- ""
  } else {
    x$.x <- as.numeric(factor(x$.profile.id))
    .xlab <- "Sample [index]"
  }

  ##############################
  #species alignment
  p1.ls <- list(x= .value~.x,
                data=x, ylab="Measurement", xlab=.xlab,
                type="l",
                #NB: prepanel seemed to break ylim when stacking
                panel = function(x, y, ...){
                  at.x <- pretty(x)
                  at.y <- pretty(y)
                  .rsp_panelPal("grid", list(h=at.y,v=at.x, col="grey", lty=3),
                               panel.abline, ...)
                  panel.xyplot(x=x, y=y, ...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=0,
                                   cex=0.7,
                                   alternating=1),
                            y=list(rot=c(0,90),
                                   cex=0.7,
                                   alternating=3,
                                   relation="free"))
  )

  #1.if multiple profiles, panel or group
  if(length(species)>1){
    if(tolower(multi.species) %in% c("panel", "panels")){
      #paneling multiple panels
      p1.ls$x <- .value~.x | .species
    } else {
      #grouping multiple panels
      p1.ls$x <- .value~.x
      p1.ls$groups <- x$.species
    }
  }

  #2. if log=TRUE
  #shortcut to scales(y(log)) and yscale.component
  if(log){
    p1.ls$scales$y$log <- 10
    p1.ls$yscale.components <- .rsp_yscale.component.log10
  }

  #3. extra user settings
  p1.ls <- modifyList(p1.ls, .x.args)

  #4. tidy loose ends

  #set cols
  if("col" %in% names(p1.ls)){
    if(is.function(p1.ls$col)){
      p1.ls$col <- if("groups" %in% names(p1.ls)){
        ##testing tracking
        ##p1.ls$col(length(species))
        p1.ls$col(length(xx))
      } else {
        p1.ls$col(1)
      }
    }
  } else {
    p1.ls$col <- if("groups" %in% names(p1.ls)){
      colorRampPalette(rainbow(12, s = 0.5, v = 1)[1:11],
                               ##testing tracking
                               ##interpolate = "spline")(length(species))
                               interpolate = "spline")(length(xx))
      #or:
      #colorRampPalette(rainbow(12, s = 0.5, v = 1),interpolate = "spline")(x)
      #was:
      #colorRampPalette(trellis.par.get("superpose.line")$col,
      #    interpolate = "spline")(length(species))
      #rep(trellis.par.get("superpose.line")$col,
      #    length.out=length(species))
    } else {
      trellis.par.get("superpose.line")$col[1]
    }
  }
  #here we must have col
  #################################
  #issue here if user sets these????
  p1.ls$par.settings = list(superpose.polygon = list(col = p1.ls$col),
                            superpose.symbol = list(fill = p1.ls$col))
  #add key if needed
  #################################
  #issue if user uses key = FALSE
  #could be issue here if user uses auto.key???
  #like to track border as well as col...
  if("groups" %in% names(p1.ls)){
    #print(x$.species)
    .tmp <- list(space="right",
                 #title="Legends",
                 lines=list(col=rep(p1.ls$col,
                                    ##testing tracking
                                    ##length.out=length(species))),
                                    length.out=length(xx))),
                 ##########################
                 #text = list(levels(x$.species), cex=0.7))
                 ##text = list(species, cex=0.7))
                 text = list(xx, cex=0.7))
                 #changed from above because x$.species
    p1.ls$key <- if("key" %in% names(p1.ls)){
      modifyList(.tmp, p1.ls$key)
    } else {
      .tmp
    }
  }
  p <- do.call(xyplot, p1.ls)

  #output
  ##################
  .rsp_plot_output(as.data.frame(p1.ls$data), p1.ls, p, output)

}





###################################
#rsp_plot_match
###################################

#' @rdname rsp.plot
#' @export

# TO DO
#########################

# very early; lot of do

# think about standardizing this and other rsp_plot... functions
# also standardise these ...
#    lines(.) might not be best names for the 'red' lines?
# think about how to document grid., lines., etc...

#like to improve the legend/key output
#    key modification is messy
#       for example key=list(text=list(c("hic", "hoc")))
#    changing the rect values without the text, reorders it...
#    key rectangles are a bit wide


# general is OK but
#    think about zero cases
#        currently removing these before plotting
#             but they might be worth including ???
#             maybe different colour
# plot.type 1 (bellis-like) is OK but could be better
#       but wondering if original fishbone plot idea was better???
# plot.type 2 (normalised type 1) is OK but could be better
# plot.type 3 (percentage scores) is OK but could be better
# plot.type 4 (rsp/ref barplot) is work in progress
#        might be better plotting .test.z[whatever]
#        rather than .value.ref and .value.x?
#             need to look that up
#        maybe turn off bar border?
#             border=NA does the bars but not the key...
#        maybe reduce the rectangle width in key ???

rsp_plot_match <- function(rsp, ref = NULL, plot.type = 2,
                           log=FALSE, ..., output="plot"){

  #option to pass previous rsp_match_profile(..., output="list")
  #    but maybe need to check structure more in case user just passes a list...
  if(class(rsp)[1] !="list"){
    rsp <- rsp_match_profile(rsp, ref=ref, plot.type=plot.type, ...,
                             output="plot.data")
  }

  #################
  # set up
  #################
  .xargs <- list(...)

  #error catcher instead of NULL???
  .test <- 1:4
  if(!as.character(plot.type) %in% as.character(.test)){
    stop("RSP> Sorry, ", plot.type, " is not a know 'plot.type'",
         "\n\t    (maybe one of: ", paste(.test, collapse=","), ")",
         call. = FALSE)
  }
  plt <- NULL
  .dat <- rsp$data

  ########################
  # remove zero cases not used in calculations
  #   might be useful to include them?
  #   maybe in different colour??
  #########################
  .dat <- subset(.dat, .test.z1!=0)
  .dat <- subset(.dat, .value.ref!=0)

  if(plot.type==1){
    #based on bellis
    if(log){
      stop("RSP> Sorry, currently not logging match type 1 plots",
           call. = FALSE)
    }
    plt.ls <- list(x = .value.ref~.value.x | .profile.id.ref,
                   data=.dat, xlab="rescaled(rsp)", ylab="rescaled(reference)",
                   xlim = range(pretty(c(.dat$.value.ref, .dat$.value.x))),
                   ylim = range(pretty(c(.dat$.value.ref, .dat$.value.x))),
                   aspect=1, pch=16, col=trellis.par.get()$superpose.symbol$col[1],
                   panel=function(x, y, subscripts, ...){
                     #grid
                     .rsp_panelPal("grid", list(h=-1, v=-1, col="grey", lty=3),
                                   lattice::panel.grid, ...)
                     #lines
                     # bit messy but need to run panel.abline for each b value...
                     .tmp <- .rsp_panelPal("lines", list(a=c(0,0,0), b=c(0.5, 1, 2),
                                                         col="red", lty=c(2,1,2)),
                                           function(...){list(...)},
                                           ...)
                     for(i in 1:length(.tmp$b)){
                       .plt <-.tmp
                       for(j in 1:length(.plt)){
                         if(length(.plt[[j]])==length(.tmp$b)){
                           .plt[[j]] <- .plt[[j]][i]
                         }
                       }
                       do.call(lattice::panel.abline, .plt)
                     }
                     lattice::panel.xyplot(x = .dat$.value.x[subscripts],
                                           y = .dat$.value.ref[subscripts],
                                           subscripts = subscripts,...)
                     .rsp_panelPal("arrows", list(x1=.dat$.test.z[subscripts],
                                                 y1=.dat$.test.z[subscripts],
                                                 x0=.dat$.value.x[subscripts],
                                                 y0=.dat$.value.ref[subscripts],
                                                 col=list(...)$col,
                                                 length=0.05,
                                                 units="native"),
                                   lattice::panel.arrows, ...)
                   },
                   strip=lattice::strip.custom(bg="transparent",
                                               par.strip.text=list(cex=0.7))
    )
    plt.ls <- modifyList(plt.ls, .xargs)
    plt <- do.call(lattice::xyplot, plt.ls)
  }
  if(plot.type==2){
    #normalised version of plot.type=1
    if(log){
      stop("RSP> Sorry, currently not logging match type 2 plots",
           call. = FALSE)
    }
    #.n.pr <- length(unique(.dat$.profile.id.ref))
    plt.ls <- list(x = (.test.z1n-.test.zn)~factor(.species) | .profile.id.ref,
                   data=.dat, xlab="", ylab="Standardized Difference",
                   pch=16,
                   strip=lattice::strip.custom(bg="transparent",
                                               par.strip.text=list(cex=0.7)),
                   type=c("h", "p"),
                   scales=list(x=list(rot=90,
                                      alternating=TRUE),
                               cex=0.7),
                   panel=function(...){
                     .rsp_panelPal("grid", list(h=-1, v=0.1, col="grey", lty=3),
                                  lattice::panel.grid, ...)
                     .rsp_panelPal("lines", list(h=c(-0.5, 0, 0.5),
                                                col="red", lty=c(2,1,2)),
                                   lattice::panel.abline, ...)
                     lattice::panel.xyplot(...)
                   }
    )
    plt.ls <- modifyList(plt.ls, .xargs)
    plt <- do.call(lattice::xyplot, plt.ls)
  }
  if(plot.type==3){
    #percentage fit
    if(log){
      stop("RSP> Sorry, currently not logging match type 2 plots",
           call. = FALSE)
    }
    #.n.pr <- length(unique(.dat$.profile.id.ref))
    plt.ls <- list(x =100*(1-abs(1-.test.z1n))~ factor(.species) | .profile.id.ref,
                   data=.dat, xlab="", ylab="Percentage Fit",
                   pch=16, origin=0,
                   strip=lattice::strip.custom(bg="transparent",
                                               par.strip.text=list(cex=0.7)),
                   type=c("h", "p"),
                   scales=list(x=list(rot=90,
                                      alternating=TRUE),
                               cex=0.7),
                   panel=function(...){
                     .rsp_panelPal("grid", list(h=-1, v=0.1, col="grey", lty=3),
                                   lattice::panel.grid, ...)
                     .rsp_panelPal("lines", list(h=c(100, 50),
                                                 col="red", lty=c(1,2)),
                                   lattice::panel.abline, ...)
                     lattice::panel.barchart(...)
                   }
    )
    plt.ls <- modifyList(plt.ls, .xargs)
    plt <- do.call(lattice::barchart, plt.ls)
  }
  if(plot.type==4){
    #rsp/ref bars
    # .test.z to .test.z2 might be better than .value.x and .value.ref
    plt.ls <- list(x =.value.x + .value.ref ~ factor(.species) | .profile.id.ref,
                   data=.dat, xlab="", ylab="Profile Loading",
                   col=trellis.par.get()$superpose.symbol$col[1:2],
                   strip=lattice::strip.custom(bg="transparent",
                                               par.strip.text=list(cex=0.7)),
                   scales=list(x=list(rot=90,
                                      alternating=TRUE),
                               cex=0.7),
                   panel=function(...){
                     .rsp_panelPal("grid", list(h=-1, v=0.1, col="grey", lty=3),
                                   lattice::panel.grid, ...)
                     lattice::panel.barchart(...)
                   }
    )
    if(log){
      plt.ls$scales$y$log <- 10
      plt.ls$yscale.components <- .rsp_yscale.component.log10
    } else {
      plt.ls$origin <- 0
    }
    plt.ls <- modifyList(plt.ls, .xargs)
    .tmp <- list(space="right",
                   #title="Legends",
                   text = list(c("rsp", "ref")),
                   rectangles=list(col=rep(plt.ls$col,
                                           length.out=2)),
                   cex=0.7)
    plt.ls$key <- if("key" %in% names(plt.ls)){
      if(is.logical(plt.ls$key)){
        if(!plt.ls$key){
          NULL
        } else {
          .tmp
        }
      } else {
        modifyList(plt.ls$key, .tmp)
      }
    } else {
       .tmp
    }
    plt <- do.call(lattice::barchart, plt.ls)
  }


  #outputs like other plots
  .rsp_plot_output(as.data.frame(plt.ls$data), plt.ls, plt, output)
}

