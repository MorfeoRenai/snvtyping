library(snvtyping)
library(S4Vectors)
context.len <- 3
genome <- BSgenome.Hsapiens.UCSC.hg19::Hsapiens
fl <- system.file("extdata", "chr7-sub.vcf.gz", package="VariantAnnotation",
                  mustWork=TRUE)
vcf <- VariantAnnotation::readVcf(fl)
vcf <- VariantAnnotation::expand(vcf)



#### tests ####

test_that("no parameters", {
    expect_error(
        preprocessVcf()
    )
})

test_that("invalid vcf parameter", {
    expect_error(
        preprocessVcf(vcf = "something")
    )
})

test_that("invalid parameter", {
    expect_error(
        preprocessVcf(vcf, type = "something")
    )
})

test_that("correct usage", {
    expect_s4_class(
        preprocessVcf(vcf),
        "ExpandedVCF"
    )
})

