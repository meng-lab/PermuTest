fish3D.test <- function(d) {
 # define the function 
  .logN <- function(t,n_at,n_bt,n_ac,n_bc,st,sc,sa,sb,fa,fb) {
    logN <- lchoose(n_at,t)+
      lchoose(n_bt,st-t)+
      lchoose(n_ac,fa-t)+
      lchoose(n_bc,fb+t-st)
    return(logN)}
#names
  feature_meth_A <- d[1]
  nonfeature_meth_A <- d[2]
  feature_unmeth_A <- d[3]
  nonfeature_unmeth_A <- d[4]
  feature_meth_B <- d[5]
  nonfeature_meth_B <- d[6]
  feature_unmeth_B <- d[7]
  nonfeature_unmeth_B <- d[8]
#sum
  st <- feature_meth_A + feature_meth_B
  sc <- nonfeature_meth_A + nonfeature_meth_B
  sa <- feature_meth_A + nonfeature_meth_A##ºÍn_atÖØ¸´
  sb <- feature_meth_B + nonfeature_meth_B
  n_at <- feature_meth_A + nonfeature_meth_A
  n_bt <- feature_meth_B + nonfeature_meth_B
  n_ac <- feature_unmeth_A + nonfeature_unmeth_A
  n_bc <- feature_unmeth_B + nonfeature_unmeth_B
#sum
  fa <- feature_meth_A+feature_unmeth_A
  fb <- feature_meth_B+feature_unmeth_B
  
  if (min(st,sa,sc,sb,n_at,n_bt,n_ac,n_bc,
          fa,
          fb)==0) {
    p = 1;
    log2.OR = NaN;
  } else {
    journey <- max(0):min(st,sa)
    logways <- journey
    for (i in 1:length(journey)) {
      t <- journey[i]
      logways[i] <- .logN(t,n_at,n_bt,n_ac,n_bc,st,sc,sa,sb,fa,fb)
    }
    c <- exp(logways - max(logways))#########?ÎªÉ¶¼õ
    c <- c/sum(c)
    id <- which(journey==feature_meth_A )
    p <- sum(c[c<=c[id]])
    ra <- (feature_meth_A/n_at)/(feature_unmeth_A/n_ac)
    rb <- (feature_meth_B/n_bt)/(feature_unmeth_B/n_bc)
    log2.OR <- log2(ra/rb) 
  }
  return(list(pvalue=p,log2.OR=log2.OR))
}

# test
example<- array(c(i, 200, 300, 400,
             2000, 4000, 6000, 8000),
           dim = c(2, 2, 2), 
           dimnames = list(c("Feature","Non-Feature"),
                           c("Meth","Unmeth"),
                           c("CondA","CondB")))
fish3D.test(example)
