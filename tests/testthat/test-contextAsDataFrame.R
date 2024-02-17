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
        contextAsDataFrame()
        )
})

test_that("invert parameters", {
    expect_error(
        contextAsDataFrame(context, vcf)
        )
})

test_that("using chracter", {
    expect_error(
        contextAsDataFrame(vcf, "AATTA")
        )
})

wrong.context <- Biostrings::DNAStringSet(c(context[1:49], "AAAAAA"))
test_that("using non-unique context widths", {
    expect_error(
        contextAsDataFrame(vcf, wrong.context)
        )
})

wrong.context <- Biostrings::DNAStringSet(c(context[1:50], "ATT"))
test_that("using different context length from vcf", {
    expect_error(
        contextAsDataFrame(vcf, wrong.context)
    )
})

test_that("correct usage", {
    expect_s3_class(
        contextAsDataFrame(vcf, context),
        "data.frame"
    )
})


