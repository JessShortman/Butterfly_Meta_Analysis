# read in data
meta<-read.csv('Meta-Analysis Effect Sizes and Paper Screening.csv.csv')

# Install metafor package 
library(metafor)

# Run basic meta-analysis
meta_analysis<-rma(yi=Slope,sei=SE,data=meta)
meta_analysis

# Add SE^2 column 
meta$se2<-meta$SE^2

# Run mixed-effects model
model1<-rma.mv(yi=Slope,V=se2, mods=~NumMonth+I(NumMonth^2)+Binary_Sex+NumStudyType+Duration, random = list(~ 1 | Taxon/ID, ~ 1 | RefID, ~1|Country), data=meta)
summary(model1)

# Make quadratic plot 
xs <- seq(1, 10, length=10)
modelq<-rma.mv(yi=Slope,V=se2, mods=~NumMonth+I(NumMonth^2), random = list(~ 1 | Taxon/ID, ~ 1 | RefID), data=meta)
sav <- predict(modelq, newmods=unname(poly(xs, degree=2, raw=TRUE)))
regplot(modelq, mod=2, pred=sav, xvals=xs, psize=1, bg='lightblue2', xlab='Month', ylab='Effect Size', xlim=c(1,10))

# Make aggregated forest plots 

dat <- meta[!is.na(meta$Month) & meta$Month != "", ]

dat2 <- escalc(measure="SMD", yi=Slope, vi=se2, data=dat)

agg <- aggregate(dat2, cluster=Numeric_Month, V=vcov(meta_analysis2, type="obs"), addk=TRUE)
res <- rma(yi, vi, method="EE", data=agg, digits=3)
forest(res, mlab="Pooled Estimate", header='Month', ilab=ki, slab=agg$Month, cex=0.8, xlab='Effect Size', order=Numeric_Month)

agg2 <- aggregate(dat, cluster=Reference, V=vcov(meta_analysis, type="obs"), addk=TRUE)
res2 <- rma(yi, vi, method="EE", data=agg2, digits=3)
forest(res2, mlab="Pooled Estimate", header='Study', ilab=ki, slab=agg2$Reference, cex=0.8, xlab='Effect Size', order='obs')

# Test of assymetry
regtest(meta_analysis)


