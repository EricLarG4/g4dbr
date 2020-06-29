#' Erases the data of all of selected oligonucleotides from a g4dbr database while maintaining its structure for reuse
#'
#' @param db.to.erase An .Rda file path to erase
#' @param remove.oligos A vector containg strings of the \code{oligo} names
#' @return Return a list of partially of fully emptied dataframes with the correct structure for g4db use
#' @examples
#' database.eraser("this is a filepath")
#'

database.eraser <- function(db.to.erase = NULL, remove.oligos = NULL, erase.CD, erase.NMR, erase.MS, erase.UV){

  #operator definition
  '%notin%' <- Negate('%in%')

  #data to remove
  remove.oligos <- remove.oligos

  #if all exp data is removed, remove the oligo info as well
  if (erase.CD == TRUE & erase.NMR == TRUE & erase.MS == TRUE & erase.UV == TRUE) {
    erase.info <- TRUE
  } else {
    erase.info <- FALSE
  }

  #file loading
  load(file = db.to.erase)

  #erasing function
  erase.db <- function(dataset = NULL, remove.oligos){

    dataset <- dataset %>%
      filter(oligo %notin% remove.oligos)

    return(dataset)
  }

  #Data removal (per method, if selected for removal)
  if (erase.CD == TRUE) {
    db.CD <- as.data.frame(erase.db(dataset = db.CD, remove.oligos))
  }

  if (erase.info == TRUE) {
    db.info <- as.data.frame(erase.db(db.info, remove.oligos))
  }

  if (erase.MS == TRUE) {
    db.MS <- as.data.frame(erase.db(db.MS, remove.oligos))
  }

  if (erase.UV == TRUE) {
    db.UV <- as.data.frame(erase.db(db.UV, remove.oligos))
  }

  if (erase.NMR == TRUE) {
    db.NMR <- as.data.frame(erase.db(db.NMR, remove.oligos))
  }


  #Rest of data collected back in a list
  db.collection <- list('db.info' = db.info,
                        'db.CD' = db.CD,
                        'db.NMR' = db.NMR,
                        'db.MS' = db.MS,
                        'db.UV' = db.UV)

  return(db.collection)

}

