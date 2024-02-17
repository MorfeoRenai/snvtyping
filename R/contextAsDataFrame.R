#' Coerce Context DNAStringSet as Data Frame
#'
#' This function converts the context DNAStringSet of SNVs to a data frame.
#' The SNV information is redundant, given their reverse complement, so
#' they are compressed to only T and C references.
#'
#' @param vcf ExpandedVcf object
#' @param context `DNAStringSet` returned by `snvtyping::getSnvContext` function
#'
#' @return Data frame with columns `up` `ref` `alt` `down` `context`
#' `snv.context` `snv`
#' @export
#'
#' @importFrom S4Vectors width
#' @importFrom VariantAnnotation alt
#'
#' @example R/examples/contextAsDataFrame-example.R
#'
contextAsDataFrame <- function(vcf, context){
    # check parameters
    if (!inherits(vcf, "ExpandedVCF")) {
        stop("VCF must be an object of class ExpandedVCF.")}
    else if (!inherits(context, "DNAStringSet")) {
        stop("Context must be an object of class DNAStringSet.")}
    else if (length(unique(S4Vectors::width(context))) > 1){
        # there must be only one unique width (= context.len) in the
        # DNAStringSet context, if not the DNAStringSet could have been
        # manipulated or generated in an incorrect way
        stop("Context DNAStringSet has non-homogenous DNAString widths. Meaning
there is more than one unique width value. It's impossible to correctly
separate upstream and downstream contexts."
             )
    }
    # deduce context length
    context.len <- S4Vectors::width(context)[1]

    # coerce as character
    context <- as.character(context)

    # upstream and downstream contexts
    up <- substr(context, 1, context.len%/%2)
    ref <- substr(context, context.len%/%2 + 1, context.len%/%2 + 1)
    down <- substr(context, context.len%/%2 + 2, context.len)

    # alt from VariantAnnotation
    alt <- VariantAnnotation::alt(vcf)

    # main data frame
    df <- data.frame(up, ref, alt, down, context)

    # add a column for the SNV context
    # example : CAC[C>G]ATC
    df$snv.context <- apply(df, MARGIN = 1, FUN = .addressRedundancy)

    # add a column for the SNV
    df$snv <- apply(df, MARGIN = 1, FUN = .getSnvRowWise)

    return(df)
}
