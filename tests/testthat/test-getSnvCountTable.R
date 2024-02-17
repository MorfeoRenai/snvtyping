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
        getSnvCountTable()
    )
})

test_that("missing parameter", {
    expect_error(
        getSnvCountTable(vcf = vcf)
    )
})

test_that("missing parameter", {
    expect_error(
        getSnvCountTable(context = context)
    )
})

test_that("invert parameters", {
    expect_error(
        getSnvCountTable(context, vcf)
    )
})

test_that("invalid type parameter", {
    expect_error(
        getSnvCountTable(vcf, context, type = "something")
    )
})

test_that("invalid type parameter", {
    expect_error(
        getSnvCountTable(vcf, context, type = 1)
    )
})

test_that("correct usage", {
    expect_s3_class(
        getSnvCountTable(vcf, context),
        'table'
    )
})

test_that("correct usage", {
    expect_s3_class(
        getSnvCountTable(vcf, context, type = "context.by.snv"),
        'table'
    )
})
