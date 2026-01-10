#' @name rsp.export
#' @title Exporting respeciate objects
#' @aliases rsp_export_esat


#' @description rsp function(s) to export respeciate (and respeciate-like)
#' objects to other software

#' @param rsp (\code{respeciate} or similar, e.g. a data-frame set up for use
#' with \code{respeciate}), the data-set to export.
#' @param file.name (\code{character}), the file name of the exported file or
#' files. See also \code{output} and Details below.
#' @param index (\code{character}), the name of \code{rsp} column to use as the
#' output file(s) index or (default) \code{'row.count'} a row number counter.
#' @param unc (various), if \code{numeric}, the scaling factor to apply to
#' concentration values when hole filling uncertainties, else \code{'eu.rsp'},
#' in which case it tries to recover values from the \code{SPECIEUROPE} meta
#' information.
#' @param bad.values (\code{character}), handling method to use if bad values
#' are found in the supplied data.
#' @param output (\code{character}), the file types to export. See also
#' Details below.
#' @param overwrite (\code{character}), overwrite file if it already exists.
#' @param ... other arguments, currently ignored.
#' @return \code{rsp_export}s attempt to build and save files suitable for use
#' outside \code{r}.
#'
#' @details
#' \code{rsp_build_esat} makes files that can be used as inputs with ESAT.
#' \code{output} options: 'con.csv' and 'unc.csv' (both required by ESAT).
#'

# TO DO: document and reference this

#NOTES
#######################

#to think about
#######################

## think this needs more/better documentation

#     but only me using it at moment


##############################
# rsp_export_esat
##############################

# notes
##############################

# 0.1. notes 2025-02-26

# this makes .csv files for use with ESAT
#

#' @rdname rsp.export
#' @export

rsp_export_esat <-
  function(rsp, file.name="file",
           index = "row.count",
           unc = 0.15, bad.values = "fill.1",
           output=c("con.csv", "unc.csv"),
           overwrite=FALSE,
           ...){

    # concentrations
    .x <- as.respeciate(rsp, test.rsp=TRUE)
    .con <- as.data.frame(rsp_dcast_species(.x))

    # uncertainties
    .unc <- NULL
    if(is.numeric(unc)){
      #use numeric as a scaling factor
      if(length(unc)==1){
        unc <- rep(unc, nrow(.x))
      }
      if(length(unc)!=nrow(.x)){
        stop("rsp_export> 'unc'/rsp length mismatch, unc can be 1 or ", nrow(.con),
             call. = FALSE)
      }
      .unc <- .x
      .unc$.value <- .unc$.value * unc
      .unc <- as.data.frame(rsp_dcast_species(.unc))
    } else {
      #column name or eu.rsp
      if(is.character(unc)){
        if(unc  %in% names(.x)){
          .unc <- .x
          .unc$.value <- .unc[unc[1]]
          .unc <- as.data.frame(rsp_dcast_species(.unc))
        }
        if(unc == "eu.rsp"){
          .temp <- ..rsp_uncertainties_meta()
          .unc <- data.table::merge.data.table(data.table::as.data.table(.x),
                                               data.table::as.data.table(.temp),
                                               all.x=TRUE, all.y=FALSE)
          .unc <- as.data.frame(.unc)
          .unc$.value <- .unc$.pc.unc
          .unc <- as.data.frame(rsp_dcast_species(.unc))
        }
      }
    }
    if(is.null(.unc)){
      stop("rsp_export> not a known 'unc' methods...",
           call. = FALSE)
    }

    # check with derron (sp?) about options...
    # to do
    # .unc hole filling

    # hole filing
    ###################################
    #don't like this
    #needs rethinking...
    ###################################
    if(bad.values=="fill.1"){
      #zero/na fill method one
      #if conc missing or <= 0 use min(what.is.larger)*3
      for(i in 2:ncol(.con)){
        .test <- is.na(.con[i]) | .con[i] <=0
        if(!all(.test) & any(.test)){
          .in <- min(.con[i][!.test], na.rm=TRUE)
          .con[.test, i] <- .in/10
        }
        .test <- is.na(.unc[i]) | .unc[i] <=0
        if(!all(.test) & any(.test)){
          .in <- min(.unc[i][!.test], na.rm=TRUE)
          .unc[.test, i] <- .in/5
        }
      }
    }
    if(any(is.na(.con[names(.con)!=index])) | any(.con[names(.con)!=index] <=0)){
      warning("rsp_export_esta> suspect CONs in output (maybe reset bad values)",
              call. = FALSE)
    }
    if(any(is.na(.con[names(.unc)!=index])) | any(.con[names(.unc)!=index] <=0)){
      warning("rsp_export_esta> suspect UNCs in output (maybe reset bad values)",
              call. = FALSE)
    }


    #index tidy
    if(length(index)!=1){
      stop("rsp_export_esat> unexpected 'index'",
           call. = FALSE)
    }
    if(index=="row.count"){
      .con <- data.frame(row.count = 1:nrow(.con), .con[!names(.con) %in% c(".profile", ".profile.id")])
      .unc <- data.frame(row.count = 1:nrow(.unc), .unc[!names(.unc) %in% c(".profile", ".profile.id")])
    }

    #col name tidies
    #names(.con) <- make.names(names(.con))
    #names(.unc) <- make.names(names(.unc))
    # to do if needed...
    # do for both .con and .unc

    #save outputs
    # to do...
    # stop over-saving??
    # do for .unc as well as .con...
    .con.fn <- paste(file.name, "con.csv", sep="_")
    if(.con.fn %in% dir()){
      if(!overwrite){
        stop("rsp_export_esat> file exists; overwrite?")
      }
    }
    .unc.fn <- paste(file.name, "con.csv", sep="_")
    if(.unc.fn %in% dir()){
      if(!overwrite){
        stop("rsp_export_esat> file exists; overwrite?")
      }
    }

    write.csv(.con, paste(file.name, "con.csv", sep="_"), row.names=FALSE)
    write.csv(.unc, paste(file.name, "unc.csv", sep="_"), row.names=FALSE)

    #remember to delete the files when running this via package project...
  }






