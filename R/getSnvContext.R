#' Get Genomic Context
#'
#' This function returns the genomic context of a SNV, of a defined base-pair
#' length, from parameters: chromosome, position, reference nucleotide and
#' alternative nucleotide.
#' The SNV information is redundant, given their reverse complement, so
#' they are compressed to only T and C references.
#'
#' @param vcf ExpandedVcf object
#' @param genome Set of sequences on which query is made
#' @param context.len Length of the genomic context to query. Odd integer number
#' equal or greater than 3. (Default: 3)
#'
#' @return Genomic contexts as DNAStringSet object of all the SNVs in the
#' ExpandedVCF
#' @export
#'
#' @importFrom GenomeInfoDb seqlevels
#' @importFrom IRanges IRanges
#' @importFrom BiocGenerics start
#' @importFrom IRanges ranges
#' @importFrom GenomicRanges GRanges
#' @importFrom Biostrings getSeq
#'
#' @example man/examples/getSnvContext-example.R
#'
getSnvContext <- function(vcf, genome, context.len = 3){
    if (!inherits(vcf, "ExpandedVCF")) {
        stop("VCF must be an object of class ExpandedVCF.")}
    else if (!(inherits(genome, "BSgenome") ||
               inherits(genome, "DNAStringSet"))) {
        stop("Genome must be an object of class BSGenome or DNAStringSet.")}
    else if (!inherits(context.len, "numeric")) {
        # not a number
        stop("Context length must be an odd integer >=3.")}
    else if (context.len %% 1 != 0) {
        # not an integer
        stop("Context length must be an odd integer >=3.")}
    else if (context.len %% 2 == 0 || context.len < 3) {
        # even or smaller than 3
        stop("Context length must be an odd integer >=3.")}

    # names of the chromosomes/sequences in the VCF
    seqnames <- ifelse(grepl("chr", GenomeInfoDb::seqlevels(vcf)),
                       GenomeInfoDb::seqlevels(vcf),
                       paste("chr", GenomeInfoDb::seqlevels(vcf), sep=""))

    # genomic range of context
    ir <- IRanges::IRanges(
        start = BiocGenerics::start(
            IRanges::ranges(vcf)) - context.len%/%2,
        end = BiocGenerics::start(
            IRanges::ranges(vcf)) + context.len%/%2)
    gr <- GenomicRanges::GRanges(seqnames = seqnames,
                                 ranges = ir)

    if (!(seqnames %in% names(genome))) {
        stop("Sequence names in the VCF are not a subset of the sequence names
of the genome. You might be using an incorrect genome, please double-check.")}

    # contexts fetched and then coerced into character vector
    context <- Biostrings::getSeq(x = genome, gr)
    return(context)
}

