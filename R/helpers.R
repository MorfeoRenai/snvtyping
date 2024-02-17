#' Get SNV from a Row in the Main Data Frame
#'
#' This function returns the context with the Single Nucleotide Variant in the
#' form `UP[REF>ALT]DOWN` from a row-vector with the following information:
#' upstream context, reference nucleotide, alternative nucleotide and downstream
#' context.
#' The SNV information is redundant, given their reverse complement, so they are
#' compressed to only T and C references.
#'
#' @param row row in the main data frame with columns `up` `ref` `alt` `down`
#'
#' @return character vector of SNVs in the form of `UP[REF>ALT]DOWN`
#'
#' @importFrom Biostrings DNAString
#' @importFrom Biostrings reverseComplement
#'
#' @examples
#' # not exported
#'
#' @noRd
#' @keywords internal
.addressRedundancy <- function(row){
    up <- row[["up"]]
    ref <- row[["ref"]]
    alt <- row[["alt"]]
    down <- row[["down"]]

    if (ref == "A" | ref == "G"){
        # reverse complement in order to compress redundant information
        up <- Biostrings::reverseComplement(Biostrings::DNAString(up))
        down <- Biostrings::reverseComplement(Biostrings::DNAString(down))
        ref <- Biostrings::reverseComplement(Biostrings::DNAString(ref))
        alt <- Biostrings::reverseComplement(Biostrings::DNAString(alt))

        # coerced as character vector for final result
        up <- as.character(up)
        down <- as.character(down)
        ref <- as.character(ref)
        alt <- as.character(alt)

        # result
        # example : CAC[C>G]ATC
        res <- paste(down, "[", ref, ">", alt, "]", up, sep = "")
    }
    else {
        # all mutation types should be reported such that they have C or T as
        # the mutated REF base
        # coerced as character for final result
        res <- paste(up, "[", ref, ">", alt, "]", down, sep = "")
    }
    return(res)
}


#' Get SNV from a Row in the Main Data Frame
#'
#' This function returns only the SNV as the Reference-Alternative pair
#' `REF>ALT` from a row-vector with the following information: reference and
#' alternative nucleotide.
#' The SNV information is redundant, given their reverse complement, so they are
#' compressed to only T and C references.
#'
#' @param row row in the main data frame with columns `ref` and `alt`
#'
#' @return character `REF>ALT` SNV
#'
#' @importFrom Biostrings DNAString
#' @importFrom Biostrings reverseComplement
#'
#' @examples
#' # not exported
#'
#' @noRd
#' @keywords internal
.getSnvRowWise <- function(row){
    ref <- row[["ref"]]
    alt <- row[["alt"]]

    if (ref == "A" | ref == "G"){
        # reverse complement in order to compress redundant information
        ref <- Biostrings::reverseComplement(Biostrings::DNAString(ref))
        alt <- Biostrings::reverseComplement(Biostrings::DNAString(alt))

        # coerced as character for final result
        ref <- as.character(ref)
        alt <- as.character(alt)

        # result
        res <- paste(ref, ">", alt, sep = "")
    }
    else {
        # result
        res <- paste(ref, ">", alt, sep = "")
    }
    return(res)
}

