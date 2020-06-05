#' @name getDomain
#' @aliases getDomain getPhylum getClass getOrder getFamily getGenus getTag getTaxonomy
#' @title Extractor functions for ConTax data
#' 
#' @description Extracting taxonomic information from ConTax data sets.
#' 
#' @usage getDomain(header)
#' getPhylum(header)
#' getClass(header)
#' getOrder(header)
#' getFamily(header)
#' getGenus(header)
#' getTag(header)
#' getTaxonomy(header)
#' 
#' @param header A vector of texts, typically the \code{Header} from a \code{data.frame} object,
#' containing taxonomy information in the proper format.
#' 
#' @details The ConTax data sets are \code{Fasta} objects, where the \code{Header} line follows
#' a strict format.
#' 
#' The Header always starts with a short text, a Tag, which is a unique identifier for every sequence.
#' The function \code{getTag} will extract this from the \code{header}.
#' 
#' After the Tag follows one or more tokens. One of these tokens must be a string with the
#' following format:
#' 
#' "k__<...>;p__<...>;c__<...>;o__<...>;f__<...>;g__<...>;"
#' 
#' where <...> is some proper text. Here is an example of a proper string:
#' 
#' "k__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales;f__Staphylococcaceae;g__Staphylococcus;"
#' 
#' The functions \code{getDomain}, ..., \code{getGenus} extract the
#' corresponding information from the \code{header}. \code{getTaxonomy}
#' combines all taxonomy extractors, combines these in a data.frame
#' and imputes missing taxa with parent taxa.
#' 
#' @return A vector containing the sub-texts extracted from each \code{header} text.
#' 
#' @author Lars Snipen.
#' 
#' @seealso \code{\link{contax.trim}}, \code{\link{medoids}}.
#' 
#' @examples 
#' data(contax.trim)
#' getTag(contax.trim$Header)
#' getGenus(contax.trim$Header)
#' getPhylum(contax.trim$Header)
#' 
#' @export getDomain
#' @export getPhylum
#' @export getClass
#' @export getOrder
#' @export getFamily
#' @export getGenus
#' @export getTag
#' @export getTaxonomy
#' 
getDomain <- function( header ){
  txt <- sub( ";", "", sub( "k__", "", unlist( microseq::gregexpr( "k__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getPhylum <- function( header ){
  txt <- sub( ";", "", sub( "p__", "", unlist( microseq::gregexpr( "p__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getClass <- function( header ){
  txt <- sub( ";", "", sub( "c__", "", unlist( microseq::gregexpr( "c__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getOrder <- function( header ){
  txt <- sub( ";", "", sub( "o__", "", unlist( microseq::gregexpr( "o__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getFamily <- function( header ){
  txt <- sub( ";", "", sub( "f__", "", unlist( microseq::gregexpr( "f__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getGenus <- function( header ){
  txt <- sub( ";", "", sub( "g__", "", unlist( microseq::gregexpr( "g__[^;]+;", header, extract=T ) ) ) )
  return( txt )
}

getTag <- function( header ){
  tag <- sapply( strsplit( header, split=" " ), function( x ){ x[1] } )
  return( tag )
}

getTaxonomy <- function( header ){
  domain <- getDomain( header )
  phylum <- getPhylum( header )
  class  <- getClass( header)
  order  <- getOrder( header)
  family <- getFamily(header)
  genus  <- getGenus( header)
  class [class  == "unknown"] <- paste(phylum[class  == "unknown"], '.class',  sep="")
  order [order  == "unknown"] <- paste(class [order  == "unknown"], '.order',  sep="")
  family[family == "unknown"] <- paste(order [family == "unknown"], '.family', sep="")
  genus [genus  == "unknown"] <- paste(family[genus  == "unknown"], '.genus',  sep="")
  data.frame(domain = domain, phylum = phylum, class = class, order = order, family = family, genus = genus, stringsAsFactors = FALSE)
}

