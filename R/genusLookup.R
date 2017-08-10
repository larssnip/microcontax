#' @name genusLookup
#' @aliases genusLookup
#' @title Taxonomy lookup
#' 
#' @description Extracting taxonomic information from the \code{\link{taxonomy.table}}.
#' 
#' @usage genusLookup(genera, level="Phylum")
#' 
#' @param genera A vector of texts, the genera names to look up.
#' @param level A single text, the level of the taxonomy to look up.
#' 
#' @details Function for looking up higher-level taxonomy of specified genera.
#' 
#' The argument \code{genera} must consist of names in the \code{Genus} column of the data
#' set \code{\link{taxonomy.table}}.
#' 
#  The \code{level} text must be either "Domain", "Phylum", "Class", "Order" or "Family".
#' 
#' @return A character vector containing the taxonomy information. Names in \code{genera} not recognized will 
#' return \code{NA}. Please note that there are some cases of un-assigned taxonomy at some levels 
#' (Class, Order or Family), this is returned as "unknown".
#' 
#' @author Hilde Vinje, Lars Snipen.
#' 
#' @seealso \code{\link{taxonomy.table}}.
#' 
#' @examples 
#' genus <-  c("Acidilobus","Nitrosopumilus","Hyphomonas") 
#' genusLookup(genus,level="Phylum")
#' genusLookup(genus,level="Class")
#' 
#' 
#' @export genusLookup
#' 
genusLookup <- function( genera, level="Phylum" ){
  lev <- c( "DOMAIN", "PHYLUM", "CLASS", "ORDER", "FAMILY" )
  col <- match( toupper( level ), lev )
  if( is.na( col ) ) stop( "Unknown level, must be: Domain, Phylum, Class, Order or Family" )
  taxonomy.table <- NULL
  load( paste( path.package( "microcontax"), "/data/taxonomy.table.rda", sep="" ) )
  vec <- match( genera, taxonomy.table$Genus )
  ret <- taxonomy.table[vec,col]
  return( ret )
}

#' @name fullTaxonomy
#' @title The full taxonomy of a genus
#' 
#' @description Converts a genus to a string containing the full taxonomy.
#' 
#' @usage fullTaxonomy(genera)
#' 
#' @param genera A vector of texts, the genera names to look up.
#' 
#' @details The argument \code{genera} must consist of names in the \code{Genus} column of the data
#' set \code{\link{taxonomy.table}}.
#' 
#  Each genus is converted to a string with the following format:
#'
#' "k__<...>;p__<...>;c__<...>;o__<...>;f__<...>;g__<...>;"
#' 
#' where <...> is some proper text.
#' 
#' @return A character vector containing the taxonomy information. 
#' 
#' @author Lars Snipen.
#' 
#' @seealso \code{\link{taxonomy.table}}, \code{\link{genusLookup}}.
#' 
#' @examples 
#' genera <-  c("Bacillus","Clostridium","Hyphomonas") 
#' fullTaxonomy(genera)
#' 
#' 
#' @export fullTaxonomy
#' 
fullTaxonomy <- function( genera ){
  taxonomy.table <- NULL
  load( paste( path.package( "microcontax"), "/data/taxonomy.table.rda", sep="" ) )
  qiime.heads <- paste( "k__", taxonomy.table$Domain,
                        ";p__", taxonomy.table$Phylum,
                        ";c__", taxonomy.table$Class,
                        ";o__", taxonomy.table$Order,
                        ";f__", taxonomy.table$Family,
                        ";g__", taxonomy.table$Genus, sep="" )
  vec <- match( genera, taxonomy.table$Genus )
  return( qiime.heads[vec] )
}