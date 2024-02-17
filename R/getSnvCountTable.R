#' Get Count Table of SNVs
#'
#' @param vcf ExpandedVcf object
#' @param context `DNAStringSet` returned by `snvtyping::getSnvContext` function
#' @param type Flag for getting a different
#' count table: `"context"` return a count table for every SNV context,
#' `"context.by.snv"` returns a count table for every reference context grouped
#' by their SNV type
#'
#' @return Count table of SNVs
#' @export
#'
#' @example man/examples/getSnvCountTable-example.R
#'
getSnvCountTable <- function(vcf, context, type = "context"){
    # build main data frame
    df <- contextAsDataFrame(vcf, context)

    # return two different tables based on flag
    if (type == "context"){
        table(df$snv.context)
    } else if (type == "context.by.snv") {
        table(df$snv, df$context)
    } else {
        stop(
        "Please, enter a valid type parameter. 'context' or 'context.by.snv'")
    }

}
