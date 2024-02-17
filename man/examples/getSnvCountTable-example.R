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
getSnvCountTable(vcf, context, type = "context")
getSnvCountTable(vcf, context, type = "context.by.snv")
