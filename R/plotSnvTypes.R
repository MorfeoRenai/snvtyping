#' Barplot of all reference contexts grouped by SNV type
#'
#' This functions plots a bar plot using `ggplot2` of all reference contexts
#' grouping them by SNV type.
#'
#' @param vcf ExpandedVcf object
#' @param context `DNAStringSet` returned by `snvtyping::getSnvContext` function
#'
#' @return Barplot of all reference contexts grouped by SNV type
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @example man/examples/plotSnvTypes-example.R
plotSnvTypes <- function(vcf, context){
    # build main data frame
    df <- contextAsDataFrame(vcf, context)

    # plotting reference contexts, grouping by SNV
    df %>%
        group_by(snv, context) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = context, y = count, fill=snv)) +
        geom_bar(position = "dodge", stat = 'identity', width = 0.5) +
        labs(x = "Contexts", y = "Counts", fill = "SNVs") +
        theme(axis.text.x = element_text(angle = 90))
}

