library(DoE.base)

factorize(12)
factorize(c(2,2,3,3,6))
factorize(fac.design(nlevels=c(2,2,3,3,6)))
unlist(factorize(c(2,2,3,3,6)))
factorize(undesign(fac.design(nlevels=c(2,2,3,3,6))))

exp.design <- oa.design(nlevels=c(2,2,2,2,2,2,2,2,2,5))

library(AlgDesign)
cand.list = expand.grid(Storage = c("8 GB", "16 GB"),
                        Brand = c("Samsung", "Apple", "Nokia"),
                        RAM = c("1 GB", "2 GB"),
                        BrowseTime = c("24 hour", "36 hour"),
                        Weight = c("3.95 oz OR 111 gram", "5.04 oz OR 142gram"),
                        ScreenSize = c("4.7", "5.5", "5.7"))

###same as SPSS orthogonal design 'seed'. Can put any number. Does not matter.
set.seed(69)

###Generate 16 alternatives in an optimal orthogonal design
optFederov( ~ ., data = cand.list, nTrials = 16)

###End of code


des.control <- oa.design(ID=L18)
des.noise <- oa.design(ID=L4.2.3,nlevels=2,factor.names=c("N1","N2","N3"))
crossed <- cross.design(des.control, des.noise)
crossed
summary(crossed)
