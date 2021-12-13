# compute the 95% Confident Interval by bootstrapping
# input: vector of sample
bootstrap_ci = function(x){
  x.bar = mean(x); n = length(x)
  B = 100000;  x.bar.re = numeric(B)
  for (i in 1:B) {
    x.bar.re[i] = mean(sample(x, n, repl=T))  }
  L.re = quantile(x.bar.re - x.bar, .025)
  names(L.re) = 0
  U.re = quantile(x.bar.re - x.bar, .975)
  names(U.re) = 0
  return(c(x.bar - U.re, x.bar, x.bar - L.re))
  #return(quantile(x.bar.re, c(0.025, 0.5, 0.975)))
}

# example:
x = c( 29,  30,  53,  75,  89,  34,  21,  12,  58,  84,  
       92, 117, 115, 119, 109, 115, 134, 253, 289, 287)
bootstrap_ci(x)