# Compute MSA H coefficients from the weighted data
wcoefH<-function(completedata, completeweight) {
X<-as.matrix(completedata)
X<-X-min(X)
completeweight<-chile$pond[complete.cases(data)]
Sw<-cov.wt(X,completeweight)$cov
Smaxw <- cov.wt(apply(X, 2, sort),completeweight)$cov
Hijw <- Sw/Smaxw
diag(Sw) <- 0
diag(Smaxw) <- 0
Hiw <- apply(Sw, 1, sum)/apply(Smaxw, 1, sum)
Hw <- sum(Sw)/sum(Smaxw)
OLw <- list(Hij = Hijw, Hi = Hiw, H = Hw)
return(OLw)
}
