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
context <- getSnvContext(vcf, genome, context.len)



#### tests ####

test_that("no parameters", {
    expect_error(
        plotSnvTypes()
    )
})

test_that("missing parameter", {
    expect_error(
        plotSnvTypes(vcf = vcf)
    )
})

test_that("missing parameter", {
    expect_error(
        plotSnvTypes(context = context)
    )
})

test_that("invert parameters", {
    expect_error(
        plotSnvTypes(context, vcf)
    )
})

test_that("invalid type parameter", {
    expect_error(
        plotSnvTypes(vcf, context, type = "something")
    )
})

test_that("correct usage", {
    expect_s3_class(
        plotSnvTypes(vcf, context),
        'ggplot'
    )
})

