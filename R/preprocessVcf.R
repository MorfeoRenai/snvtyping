#' Preprocess Genome According to VCF Information
#'
#' This function will filter the genome by taking into account only relevant
#' chromosomes used in the variant calling experiment that generated the VCF
#' file in question.
#'
#' @param vcf `ExpandededVCF` object
#'
#' @return Set of relevant sequences on which variant calling was made.
#' Usually subset of the original genome.
#' @export
#'
#' @importFrom S4Vectors subset.Vector
#' @importFrom S4Vectors nchar
#' @importFrom VariantAnnotation ref
#' @importFrom VariantAnnotation alt
#'
#' @example R/examples/preprocessVcf-example.R
#'
preprocessVcf <- function(vcf) {
    if (!(inherits(vcf, "ExpandedVcf"))) {
        simpleError(
            "Genome must be an object of class BSGenome or DNAStringSet"
            )
    }
    vcf <- S4Vectors::subset.Vector(vcf,
        S4Vectors::nchar(VariantAnnotation::ref(vcf)) ==
            S4Vectors::nchar(VariantAnnotation::alt(vcf)))
    vcf <- S4Vectors::subset.Vector(vcf,
            S4Vectors::nchar(VariantAnnotation::ref(vcf)) == 1)
    return(vcf)
}

