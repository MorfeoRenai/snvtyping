library(snvtyping)
library(S4Vectors)
context.len <- 3
genome <- BSgenome.Hsapiens.UCSC.hg19::Hsapiens
fl <- system.file("extdata", "chr7-sub.vcf.gz", package="VariantAnnotation",
                  mustWork=TRUE)
vcf <- VariantAnnotation::readVcf(fl)
vcf <- VariantAnnotation::expand(vcf)
vcf <- preprocessVcf(vcf)
vcf <- vcf[1:50]



#### tests ####

test_that("no parameters", {
    expect_error(
        getSnvContext()
    )
})

test_that("missing parameter", {
    expect_error(
        getSnvContext(vcf = vcf)
    )
})

test_that("missing parameter", {
    expect_error(
        getSnvContext(genome = genome)
    )
})

test_that("invert parameters", {
    expect_error(
        getSnvContext(vcf, context.len, genome)
    )
})

test_that("incorrect parameter type vcf", {
    expect_error(
        getSnvContext(666, context.len, genome)
    )
})

test_that("incorrect context length parameter", {
    expect_error(
        getSnvContext(vcf, genome, context.len = 6)
    )
})

test_that("incorrect context length parameter", {
    expect_error(
        getSnvContext(vcf, genome, context.len = -1)
    )
})

test_that("incorrect context length parameter", {
    expect_error(
        getSnvContext(vcf, genome, context.len = "qwerty")
    )
})

test_that("incorrect context length parameter", {
    expect_error(
        getSnvContext(vcf, genome, context.len = 3.1)
    )
})

wrong.genome <- Biostrings::getSeq(genome, names = c("chr22", "chrY"))
test_that("incorrect context length parameter", {
    expect_error(
        getSnvContext(vcf, wrong.genome, context.len)
    )
})

test_that("correct usage", {
    expect_s4_class(
        getSnvContext(vcf, genome, context.len),
        'DNAStringSet'
        )
})









