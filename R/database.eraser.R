#' Erases the data of all of selected oligonucleotides from a g4dbr database while maintaining its structure for reuse
#'
#' @param db.to.erase An .Rda file path to erase
#' @param remove.oligos A vector containg strings of the \code{oligo} names
#' @return Return a list of partially of fully emptied dataframes with the correct structure for g4db use
#' @examples
#' database.eraser("this is a filepath")
#'

database.eraser <- function(db.to.erase = NULL, remove.oligos = NULL){

  '%notin%' <- Negate('%in%')

  remove.oligos <- remove.oligos

  load(file = db.to.erase)

  #erasing function
  erase.db <- function(dataset = NULL, remove.oligos){

    dataset <- dataset %>%
      filter(oligo %notin% remove.oligos)

    return(dataset)
  }

  db.CD <- as.data.frame(erase.db(dataset = db.CD, remove.oligos))
  db.info <- as.data.frame(erase.db(db.info, remove.oligos))
  db.MS <- as.data.frame(erase.db(db.MS, remove.oligos))
  db.UV <- as.data.frame(erase.db(db.UV, remove.oligos))
  db.NMR <- as.data.frame(erase.db(db.NMR, remove.oligos))

  db.collection <- list('db.info' = db.info,
                        'db.CD' = db.CD,
                        'db.NMR' = db.NMR,
                        'db.MS' = db.MS,
                        'db.UV' = db.UV)

  return(db.collection)

}

