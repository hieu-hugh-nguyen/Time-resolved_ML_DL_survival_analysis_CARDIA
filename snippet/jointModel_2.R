jointModel_2 <- function (lmeObject, survObject, timeVar, parameterization = c("value", "slope", "both"), 
                        method = c("weibull-PH-aGH", "weibull-PH-GH", "weibull-AFT-aGH", "weibull-AFT-GH", 
                                   "piecewise-PH-aGH", "piecewise-PH-GH", "Cox-PH-aGH", "Cox-PH-GH", "spline-PH-aGH", 
                                   "spline-PH-GH", "ch-Laplace"), interFact = NULL, derivForm = NULL, lag = 0, 
                        scaleWB = NULL, CompRisk = FALSE, init = NULL, control = list(), ...) {
  cl <- match.call()
  if (!inherits(lmeObject, "lme"))
    stop("\n'lmeObject' must inherit from class lme.")
  if (length(lmeObject$group) > 1)
    stop("\nnested random-effects are not allowed in lme().")
  if (!is.null(lmeObject$modelStruct$corStruct))
    warning("correlation structure in 'lmeObject' is ignored.\n")
  if (!is.null(lmeObject$modelStruct$varStruct))
    warning("variance structure in 'lmeObject' is ignored.\n")        
  if (!inherits(survObject, "coxph") && !inherits(survObject, "survreg"))
    stop("\n'survObject' must inherit from class coxph or class survreg.")
  if (!is.matrix(survObject$x))
    stop("\nuse argument 'x = TRUE' in ", 
         if (inherits(survObject, "coxph")) "'coxph()'." else "'survreg()'.")
  if (length(timeVar) != 1 || !is.character(timeVar))
    stop("\n'timeVar' must be a character string.")
  method. <- match.arg(method)
  method <- switch(method., "weibull-AFT-GH" = , "weibull-AFT-aGH" = "weibull-AFT-GH",
                   "weibull-PH-GH" = , "weibull-PH-aGH" = "weibull-PH-GH", 
                   "piecewise-PH-GH" = , "piecewise-PH-aGH" = "piecewise-PH-GH",
                   "Cox-PH-GH" = , "Cox-PH-aGH" = "Cox-PH-GH", "spline-PH-GH" =, 
                   "spline-PH-aGH" = "spline-PH-GH", "ch-Laplace" = "ch-Laplace")
  parameterization <- match.arg(parameterization)
  if (method == "Cox-PH-GH" && !inherits(survObject, "coxph"))
    stop("\nfor 'method = Cox-PH-GH', 'survObject' must inherit from class coxph.")
  if (parameterization %in% c("slope", "both") && method %in% c("Cox-PH-GH", "ch-Laplace"))
    stop("\nthe slope parameterization is not currently available for methods 'Cox-PH-GH' & 'ch-Laplace'.")
  if (parameterization %in% c("slope", "both") && is.null(derivForm)) {
    stop("\nwhen parameterization is 'slope' or 'both' you need to specify the 'derivForm' argument.")
  }
  if (parameterization %in% c("slope", "both") && !is.list(derivForm)) {
    stop("\nthe 'derivForm' argument must be a list with components 'fixed' (a formula),\n\t'indFixed'", 
         "(a numeric vector), 'random' (a formula) and 'indRandom' (a numeric vector).")
  }
  if (!is.null(interFact) && !is.list(interFact)) {
    stop("\nthe 'interFact' argument must be a list -- check the help file for more info.")
  }
  if (!is.null(interFact) && method %in% c("Cox-PH-GH", "ch-Laplace")) {
    stop("\nincluding interaction terms is not currently available for methods 'Cox-PH-GH' & 'ch-Laplace'.")
  }
  if (CompRisk && (method != "spline-PH-GH" || is.null(survObject$strata))) {
    stop("\nto fit a competing risks joint model you must choose as method 'spline-PH-GH'",
         " and include a strata() in the specification of the coxph().")
  }
  # survival process
  formT <- formula(survObject)
  if (inherits(survObject, "coxph")) {
    W <- survObject$x
    keepW <- suppressWarnings(!is.na(survObject$coefficients))
    W <- W[, keepW, drop = FALSE]
    if (CompRisk) {
      nRisks <- length(unique(survObject$strata))  
    } else {
      nRisks <- 1
    }
    surv <- survObject$y
    if (attr(surv, "type") == "right") {
      LongFormat <- FALSE
      Time <- survObject$y[, 1]
      d <- survObject$y[, 2]
    } else if (attr(surv, "type") == "counting") {
      LongFormat <- TRUE
      if (is.null(survObject$model))
        stop("\nplease refit the Cox model including in the ", 
             "call to coxph() the argument 'model = TRUE'.")
      Time <- survObject$y[, 2]
      d <- survObject$y[, 3]
    }
    idT <- if (!is.null(survObject$model$cluster)) {
      as.vector(unclass(survObject$model$cluster))
    } else {
      if (!CompRisk) seq_along(Time)
      else rep(seq_len(length(Time)/nRisks), each = nRisks)
    }
    idT <- match(idT, unique(idT))
  } else {
    W <- survObject$x[, -1, drop = FALSE]
    Time <- exp(survObject$y[, 1])
    d <- survObject$y[, 2]
    idT <- seq_along(Time)
    LongFormat <- FALSE
    nRisks <- 1
  }
  nT <- length(unique(idT))
  if (LongFormat && is.null(survObject$model$cluster))
    stop("\nuse argument 'model = TRUE' and cluster() in coxph().")
  if (!length(W))
    W <- NULL
  if (sum(d) < 5)
    warning("\nmore than 5 events are required.")
  WintF.vl <- WintF.sl <- as.matrix(rep(1, length(Time)))
  if (!is.null(interFact)) {
    if (!is.null(interFact$value))
      WintF.vl <- if (is.null(survObject$model)) {
        model.matrix(interFact$value, data = interFact$data)
      } else {
        model.matrix(interFact$value, data = survObject$model)
      }
    if (!is.null(interFact$slope))
      WintF.sl <- if (is.null(survObject$model)) {
        model.matrix(interFact$slope, data = interFact$data)
      } else {
        model.matrix(interFact$slope, data = survObject$model)
      }
  }
  # longitudinal process
  id <- lmeObject$groups[[1]]
  id <- match(id, unique(id))
  b <- data.matrix(ranef(lmeObject))
  dimnames(b) <- NULL
  nY <- nrow(b)
  if (nY != nT)
    stop("sample sizes in the longitudinal and event processes differ; ", 
         "maybe you forgot the cluster() argument.\n")
  TermsX <- lmeObject$terms
  data <- lmeObject$data[all.vars(TermsX)]
  data <- data[complete.cases(data), ]
  formYx <- formula(lmeObject)
  mfX <- model.frame(TermsX, data = data)
  X <- model.matrix(formYx, mfX)
  formYz <- formula(lmeObject$modelStruct$reStruct[[1]])    
  mfZ <- model.frame(terms(formYz), data = data)
  TermsZ <- attr(mfZ, "terms")
  Z <- model.matrix(formYz, mfZ)
  y.long <- model.response(mfX, "numeric")
  data.id <- data[!duplicated(id), ]
  data.id <- data.id[idT, ]
  if (!timeVar %in% names(data))
    stop("\n'timeVar' does not correspond to one of the columns in the model.frame of 'lmeObject'.")
  # check if there are any longitudinal measurements after the event times
  max.timeY <- tapply(data[[timeVar]], factor(id, unique(id)), max)
  max.timeT <- tapply(Time, factor(idT, unique(idT)), max)
  # if (!isTRUE(all(max.timeT >= max.timeY))) {
  #   idnams <- factor(lmeObject$groups[[1]])
  #   stop("\nit seems that there are longitudinal measurements taken after the event times for some subjects ",
  #        "(i.e., check subject(s): ", paste(levels(idnams)[(max.timeT < max.timeY)], collapse = ", "), ").")
  # }
  # extra design matrices for the longitudinal part
  data.id[[timeVar]] <- pmax(Time - lag, 0)
  if (parameterization %in% c("value", "both")) {
    mfX.id <- model.frame(TermsX, data = data.id)
    mfZ.id <- model.frame(TermsZ, data = data.id)
    Xtime <- model.matrix(formYx, mfX.id)
    Ztime <- model.matrix(formYz, mfZ.id)
    long <- c(X %*% fixef(lmeObject)) + rowSums(Z * b[id, ])
  }
  if (parameterization %in% c("slope", "both")) {
    mfX.deriv <- model.frame(terms(derivForm$fixed), data = data)
    TermsX.deriv <- attr(mfX.deriv, "terms")
    mfZ.deriv <- model.frame(terms(derivForm$random), data = data)
    TermsZ.deriv <- attr(mfZ.deriv, "terms")
    mfX.deriv.id <- model.frame(TermsX.deriv, data = data.id)
    mfZ.deriv.id <- model.frame(TermsZ.deriv, data = data.id)      
    Xtime.deriv <- model.matrix(derivForm$fixed, mfX.deriv.id)
    Ztime.deriv <- model.matrix(derivForm$random, mfZ.deriv.id)
    Xderiv <- model.matrix(derivForm$fixed, mfX.deriv)
    Zderiv <- model.matrix(derivForm$random, mfZ.deriv)        
    long.deriv <- as.vector(c(Xderiv %*% fixef(lmeObject)[derivForm$indFixed]) + 
                              if (length(derivForm$indRandom) > 1 || derivForm$indRandom) 
                                rowSums(Zderiv * b[id, derivForm$indRandom, drop = FALSE])
                            else
                              rep(0, nrow(Zderiv)))
  }
  if (parameterization == "value")
    long.deriv <- NULL
  if (parameterization == "slope")
    long <- NULL        
  # response vectors and design matrices
  y <- list(y = y.long, logT = log(Time), d = d, lag = lag)
  x <- list(X = X, Z = Z, W = W, WintF.vl = WintF.vl, 
            WintF.sl = WintF.sl, idT = idT, nRisks = nRisks)
  x <- switch(parameterization, 
              "value" = c(x, list(Xtime = Xtime, Ztime = Ztime)),
              "slope" = c(x, list(Xtime.deriv = Xtime.deriv, Ztime.deriv = Ztime.deriv)),
              "both" = c(x, list(Xtime = Xtime, Ztime = Ztime, Xtime.deriv = Xtime.deriv, 
                                 Ztime.deriv = Ztime.deriv)))
  # control values
  ind.noadapt <- method. %in% c("weibull-AFT-GH", "weibull-PH-GH", "piecewise-PH-GH", 
                                "Cox-PH-GH", "spline-PH-GH")
  con <- list(only.EM = FALSE, iter.EM = if (method == "spline-PH-GH") 120 else 50, 
              iter.qN = 350, optimizer = "optim", tol1 = 1e-03, tol2 = 1e-04, 
              tol3 = if (!CompRisk) sqrt(.Machine$double.eps) else 1e-09, numeriDeriv = "fd", eps.Hes = 1e-06, 
              parscale = NULL, step.max = 0.1, backtrackSteps = 2, 
              knots = NULL, ObsTimes.knots = TRUE,
              lng.in.kn = if (method == "piecewise-PH-GH") 6 else 5, ord = 4, 
              equal.strata.knots = TRUE, typeGH = if (ind.noadapt) "simple" else "adaptive", 
              GHk = if (ncol(Z) < 3 && nrow(Z) < 2000) 15 else 9, 
              GKk = if (method == "piecewise-PH-GH" || length(Time) > nRisks*nT) 7 else 15, verbose = FALSE)
  if (method == "Cox-PH-GH") {
    con$only.EM <- TRUE
    con$iter.EM <- 200
    con$GHk <- if (ncol(Z) == 1) 15 else if (ncol(Z) == 2) 11 else 9
  }
  control <- c(control, list(...))
  namC <- names(con)
  con[(namc <- names(control))] <- control
  if (con$typeGH != "simple" && !"GHk" %in% namc) {
    con$GHk <- if (ncol(Z) <= 3 && nrow(Z) < 2000) 5 else 3
  }
  if (length(noNms <- namc[!namc %in% namC]) > 0) 
    warning("unknown names in 'control': ", paste(noNms, collapse = ", "))
  if (method == "Cox-PH-GH" && !con$only.EM)
    stop("with method 'Cox-PH-GH' only the EM algorithm is used.\n")
  if (method == "Cox-PH-GH" && any(!is.na(match(c("iter.qN", "optimizer"), namc))))
    warning("method 'Cox-PH-GH' uses only the EM algorithm.\n")
  # extra design matrices for 'method = "weibull-AFT-GH"' and 'method = "weibull-PH-GH"'
  # extra design matrices for 'method = "spline-PH-GH"' and 'method = "spline-PH-Laplace"'
  if (method %in% c("weibull-AFT-GH", "weibull-PH-GH", "spline-PH-GH", "spline-PH-Laplace")) {
    wk <- gaussKronrod(con$GKk)$wk
    sk <- gaussKronrod(con$GKk)$sk
    if (LongFormat) {
      Time0 <- survObject$y[, 1]
      P <- (Time - Time0) / 2
      P1 <- (Time + Time0) / 2
      st <- outer(P, sk) + P1
    } else {
      P <- as.vector(Time)/2
      st <- outer(P, sk + 1)
    }
    dimnames(st) <- names(P) <- NULL
    id.GK <- rep(seq_along(Time), each = con$GKk)
    data.id2 <- data.id[id.GK, , drop = FALSE]
    data.id2[[timeVar]] <- pmax(c(t(st)) - lag, 0)
    if (parameterization %in% c("value", "both")) {
      mfX <- model.frame(TermsX, data = data.id2)
      mfZ <- model.frame(TermsZ, data = data.id2)
      Xs <- model.matrix(formYx, mfX)
      Zs <- model.matrix(formYz, mfZ)
    }
    if (parameterization %in% c("slope", "both")) {
      mfX.deriv <- model.frame(TermsX.deriv, data = data.id2)
      mfZ.deriv <- model.frame(TermsZ.deriv, data = data.id2)
      Xs.deriv <- model.matrix(derivForm$fixed, mfX.deriv)
      Zs.deriv <- model.matrix(derivForm$random, mfZ.deriv)
    }
    Ws.intF.vl <- WintF.vl[id.GK, , drop = FALSE]
    Ws.intF.sl <- WintF.sl[id.GK, , drop = FALSE]
    x <- c(x, list(P = P, st = c(t(st)), wk = wk, Ws.intF.vl = Ws.intF.vl, 
                   Ws.intF.sl = Ws.intF.sl))
    x <- switch(parameterization,
                "value" = c(x, list(Xs = Xs, Zs = Zs)),
                "slope" = c(x, list(Xs.deriv = Xs.deriv, Zs.deriv = Zs.deriv)),
                "both" = c(x, list(Xs.deriv = Xs.deriv, Zs.deriv = Zs.deriv, Xs = Xs, Zs = Zs)))
    if (method == "spline-PH-GH" || method == "spline-PH-Laplace") {
      strt <- if (is.null(survObject$strata)) gl(1, length(Time)) else survObject$strata
      nstrt <- length(levels(strt))
      split.Time <- split(Time, strt)
      ind.t <- if (LongFormat) {
        unlist(tapply(idT, idT, 
                      function (x) c(rep(FALSE, length(x) - 1), TRUE)))
      } else {
        rep(TRUE, length(Time))
      }
      kn <- if (con$equal.strata.knots) {
        kk <- if (is.null(con$knots)) {
          pp <- seq(0, 1, length.out = con$lng.in.kn + 2)
          pp <- tail(head(pp, -1), -1)
          quantile(Time[ind.t], pp, names = FALSE)
        } else {
          con$knots
        }
        kk <- kk[kk < max(Time)]
        rr <- rep(list(sort(c(rep(range(Time, st), con$ord), kk))), nstrt)
        names(rr) <- names(split.Time)
        rr
      } else {
        spt <- if (length(Time) > nT & !CompRisk) 
          mapply(function (x, y){
            x[unlist(tapply(y, y, 
                            function (z) c(rep(FALSE, length(z) - 1), TRUE)))]
          }, split.Time, split(idT, strt), SIMPLIFY = FALSE)
        else split.Time
        lapply(spt, function (t) {
          kk <- if (is.null(con$knots)) {
            pp <- seq(0, 1, length.out = con$lng.in.kn + 2)
            pp <- tail(head(pp, -1), -1)
            quantile(t, pp, names = FALSE)
          } else {
            con$knots
          }
          kk <- kk[kk < max(t)]
          sort(c(rep(range(Time, st), con$ord), kk))
        })
      }
      con$knots <- kn
      W2 <- mapply(function (k, t) splineDesign(k, t, ord = con$ord), kn, 
                   split.Time, SIMPLIFY = FALSE)
      if (any(sapply(W2, colSums) == 0))
        stop("\nsome of the knots of the B-splines basis are set outside the range",
             "\n   of the observed event times for one of the strata; refit the model", 
             "\n   setting the control argument 'equal.strata.knots' to FALSE.")
      W2 <- mapply(function (w2, ind) {
        out <- matrix(0, length(Time), ncol(w2))
        out[strt == ind, ] <- w2
        out
      }, W2, levels(strt), SIMPLIFY = FALSE)
      W2 <- do.call(cbind, W2)
      strt.s <- rep(strt, each = con$GKk)
      split.Time <- split(c(t(st)), strt.s)
      W2s <- mapply(function (k, t) splineDesign(k, t, ord = con$ord), 
                    kn, split.Time, SIMPLIFY = FALSE)
      W2s <- mapply(function (w2s, ind) {
        out <- matrix(0, length(Time) * con$GKk, ncol(w2s))
        out[strt.s == ind, ] <- w2s
        out
      }, W2s, levels(strt), SIMPLIFY = FALSE)
      W2s <- do.call(cbind, W2s)
      y <- c(y, list(strata = strt))
      x <- c(x, list(W2 = W2, W2s = W2s))
    }
  }
  # extra design matrices for 'method = "piecewise-PH-GH"'
  if (method == "piecewise-PH-GH") {
    wk <- gaussKronrod(con$GKk)$wk
    sk <- gaussKronrod(con$GKk)$sk
    nk <- length(sk)
    if (is.null(con$knots) || !is.numeric(con$knots)) {
      Q <- con$lng.in.kn + 1
      qs <- if (con$ObsTimes.knots) {
        unique(quantile(Time, seq(0, 1, len = Q + 1), 
                        names = FALSE)[-c(1, Q + 1)])
      } else {
        unique(quantile(Time[d == 1], seq(0, 1, len = Q - 1), 
                        names = FALSE))                
      }
      qs <- qs + 1e-06
      if (max(qs) > max(Time))
        qs[which.max(qs)] <- max(Time) - 1e-06
      con$knots <- qs
      qs <- c(0, qs, max(Time) + 1)
      Q <- length(qs) - 1
    } else {
      qs <- c(0, sort(con$knots) + 1e-06, max(Time) + 1)
      Q <- length(qs) - 1
    }
    ind <- findInterval(Time, qs, rightmost.closed = TRUE)
    D <- matrix(0, length(ind), Q)
    D[cbind(seq_along(ind), ind)] <- 1
    D <- D * d
    Tiq <- outer(Time, qs, pmin)
    Lo <- Tiq[, 1:Q]
    Up <- Tiq[, 2:(Q+1)]
    T <- Up - Lo
    P <- T / 2
    P[P < con$tol3] <- as.numeric(NA)
    P1 <- (Up + Lo) / 2
    st <- matrix(0, nY, nk*Q)
    skQ <- rep(sk, Q)
    for (i in seq_len(nY)) {
      st[i, ] <- rep(P[i, ], each = nk) * skQ + rep(P1[i, ], each = nk)
    }
    y <- c(y, list(ind.D = ind))
    id.GK <- rep(seq_len(nY), rowSums(!is.na(st)))
    P <- c(t(P))
    data.id2 <- data.id[rep(seq_len(nY), each = nk*Q), ]
    data.id2[[timeVar]] <- pmax(c(t(st)) - lag, 0)
    data.id2 <- data.id2[!is.na(data.id2[[timeVar]]), ]
    if (parameterization %in% c("value", "both")) {
      mfX <- model.frame(TermsX, data = data.id2)
      mfZ <- model.frame(TermsZ, data = data.id2)
      Xs <- model.matrix(formYx, mfX)
      Zs <- model.matrix(formYz, mfZ)
    }
    if (parameterization %in% c("slope", "both")) {
      mfX.deriv <- model.frame(TermsX.deriv, data = data.id2)
      mfZ.deriv <- model.frame(TermsZ.deriv, data = data.id2)
      Xs.deriv <- model.matrix(derivForm$fixed, mfX.deriv)
      Zs.deriv <- model.matrix(derivForm$random, mfZ.deriv)
    }
    Ws.intF.vl <- WintF.vl[id.GK, , drop = FALSE]
    Ws.intF.sl <- WintF.sl[id.GK, , drop = FALSE]
    x <- c(x, list(P = P[!is.na(P)], st = st[!is.na(st)], wk = wk, 
                   id.GK = id.GK, Q = Q, Ws.intF.vl = Ws.intF.vl, Ws.intF.sl = Ws.intF.sl))
    x <- switch(parameterization,
                "value" = c(x, list(Xs = Xs, Zs = Zs)),
                "slope" = c(x, list(Xs.deriv = Xs.deriv, Zs.deriv = Zs.deriv)),
                "both" = c(x, list(Xs.deriv = Xs.deriv, 
                                   Zs.deriv = Zs.deriv, Xs = Xs, Zs = Zs))
    )
  }
  # extra design matrices for 'method = "Cox-PH-GH"' with event times prior to observed time for the ith subject
  if (method == "Cox-PH-GH") {
    unqT <- sort(unique(Time[d == 1]))
    times <- lapply(Time, function (t) unqT[t >= unqT])
    ind.len <- sapply(times, length)
    indT <- rep(1:nrow(data.id), ind.len)
    data.id2 <- data.id[indT, ]
    data.id2[timeVar] <- pmax(unlist(times, use.names = FALSE) - lag, 0)
    if (parameterization %in% c("value", "both")) {
      mfX <- model.frame(TermsX, data = data.id2)
      mfZ <- model.frame(TermsZ, data = data.id2)
      Xtime2 <- model.matrix(formYx, mfX)
      Ztime2 <- model.matrix(formYz, mfZ)
    }
    if (parameterization %in% c("slope", "both")) {
      mfX.deriv <- model.frame(TermsX.deriv, data = data.id2)
      mfZ.deriv <- model.frame(TermsZ.deriv, data = data.id2)
      Xtime2.deriv <- model.matrix(derivForm$fixed, mfX.deriv)
      Ztime2.deriv <- model.matrix(derivForm$random, mfZ.deriv)
    }
    x <- c(x, list(indT = indT))
    x <- switch(parameterization,
                "value" = c(x, list(Xtime2 = Xtime2, Ztime2 = Ztime2)),
                "slope" = c(x, list(Xtime2.deriv = Xtime2.deriv, 
                                    Ztime2.deriv = Ztime2.deriv)),
                "both" = c(x, list(Xtime2.deriv = Xtime2.deriv, 
                                   Ztime2.deriv = Ztime2.deriv, Xtime2 = Xtime2, Ztime2 = Ztime2))
    )
  }
  # initial values
  VC <- lapply(pdMatrix(lmeObject$modelStruct$reStruct), "*", 
               lmeObject$sigma^2)[[1]]
  if (con$typeGH != "simple") {
    Vs <- vector("list", nY)
    inv.VC <- solve(VC)
    for (i in 1:nY) {
      Z.i <- Z[id == i, , drop = FALSE]
      Vs[[i]] <- solve(crossprod(Z.i) / lmeObject$sigma^2 + inv.VC)        
    }
    con$inv.chol.VCs <- lapply(Vs, function (x) solve(chol(solve(x))))
    con$det.inv.chol.VCs <- sapply(con$inv.chol.VCs, det)
  }
  con$inv.chol.VC <- solve(chol(solve(VC)))
  con$det.inv.chol.VC <- det(con$inv.chol.VC)
  con$ranef <- b
  if (all(VC[upper.tri(VC)] == 0))
    VC <- diag(VC)
  init.surv <- initial.surv(Time, d, W, WintF.vl, WintF.sl, id, 
                            times = data[[timeVar]], method, parameterization, long = long, 
                            long.deriv = long.deriv, 
                            extra = list(W2 = x$W2, control = con, ii = idT, strata = survObject$strata),
                            LongFormat = CompRisk | length(Time) > nT)
  if (method == "Cox-PH-GH" && length(init.surv$lambda0) < length(unqT))
    init.surv$lambda0 <- basehaz(survObject)$hazard
  initial.values <- c(list(betas = fixef(lmeObject), sigma = lmeObject$sigma, D = VC), init.surv)
  if (!is.null(init)) {
    nams1 <- names(init)
    nams2 <- names(initial.values)
    if (!is.list(init) || length(noNms <- nams1[!nams1 %in% nams2])) {
      warning("unknown names in 'init': ", paste(noNms, collapse = ", "))
    } else {
      initial.values[nams1] <- init
    }
  }
  # remove objects
  rmObjs <- c(names(x), "y.long", "mfX", "mfZ", "data.id2")
  rm(list = rmObjs); gc()
  # joint model fit
  out <- switch(method,
                "Cox-PH-GH" = phGH.fit(x, y, id, initial.values, parameterization, derivForm, con),
                "weibull-AFT-GH" = weibullAFTGH.fit(x, y, id, initial.values, scaleWB, parameterization, derivForm, con),
                "weibull-PH-GH" = weibullPHGH.fit(x, y, id, initial.values, scaleWB, parameterization, derivForm, con),
                "piecewise-PH-GH" = piecewisePHGH.fit(x, y, id, initial.values, parameterization, derivForm, con),
                "spline-PH-GH" = splinePHGH.fit(x, y, id, initial.values, parameterization, derivForm, con),
                "ch-Laplace" = chLaplace.fit(x, y, id, initial.values, b, parameterization, derivForm, con))
  # check for problems with the Hessian at convergence
  H <- out$Hessian
  if (any(is.na(H) | !is.finite(H))) {
    warning("infinite or missing values in Hessian at convergence.\n")
  } else {
    ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
    if (!all(ev >= -1e-06 * abs(ev[1]))) 
      warning("Hessian matrix at convergence is not positive definite.\n")
  }
  out$coefficients <- out$coefficients[!sapply(out$coefficients, is.null)]
  out$x <- x
  out$y <- y
  out$times <- data[[timeVar]]
  out$data <- data
  out$data.id <- data.id
  out$method <- method
  out$termsYx <- TermsX
  out$termsYz <- TermsZ
  if (parameterization %in% c("slope", "both")) {
    out$termsYx.deriv <- TermsX.deriv
    out$termsYz.deriv <- TermsZ.deriv
  }
  out$termsT <- survObject$terms
  out$formYx <- formYx
  out$formYz <- formYz
  out$formT <- formT
  out$timeVar <- timeVar
  out$control <- con
  out$parameterization <- parameterization
  out$derivForm <- derivForm
  out$interFact <- interFact
  out$CompRisk <- CompRisk
  out$LongFormat <- LongFormat
  out$assignY <- attr(lmeObject$fixDF, "assign")[-1]
  out$assignT <- survObject$assign
  out$call <- cl
  class(out) <- "jointModel"
  out
}

gaussKronrod <-
  function (k = 15) {
    sk <- c(-0.949107912342758524526189684047851, -0.741531185599394439863864773280788, -0.405845151377397166906606412076961, 0,
            0.405845151377397166906606412076961, 0.741531185599394439863864773280788, 0.949107912342758524526189684047851, -0.991455371120812639206854697526329,
            -0.864864423359769072789712788640926, -0.586087235467691130294144838258730, -0.207784955007898467600689403773245, 0.207784955007898467600689403773245,
            0.586087235467691130294144838258730, 0.864864423359769072789712788640926, 0.991455371120812639206854697526329)
    wk15 <- c(0.063092092629978553290700663189204, 0.140653259715525918745189590510238, 0.190350578064785409913256402421014,
              0.209482141084727828012999174891714, 0.190350578064785409913256402421014, 0.140653259715525918745189590510238, 0.063092092629978553290700663189204,
              0.022935322010529224963732008058970, 0.104790010322250183839876322541518, 0.169004726639267902826583426598550, 0.204432940075298892414161999234649,
              0.204432940075298892414161999234649, 0.169004726639267902826583426598550, 0.104790010322250183839876322541518, 0.022935322010529224963732008058970)
    wk7 <- c(0.129484966168869693270611432679082, 0.279705391489276667901467771423780, 0.381830050505118944950369775488975, 
             0.417959183673469387755102040816327, 0.381830050505118944950369775488975, 0.279705391489276667901467771423780, 0.129484966168869693270611432679082)
    if (k == 7) 
      list(sk = sk[1:7], wk = wk7)
    else
      list(sk = sk, wk = wk15)
  }

initial.surv <-
  function (Time, d, W, WintF.vl, WintF.sl, id, times, method, 
            parameterization, long = NULL, long.deriv = NULL, extra = NULL, LongFormat) {
    old <- options(warn = (-1))
    on.exit(options(old))
    if (!is.null(long)) {
      long.id <- tapply(long, id, tail, 1)
      if (parameterization == "value") 
        longD.id <- NULL
    }
    if (!is.null(long.deriv)) {
      longD.id <- tapply(long.deriv, id, tail, 1)
      if (parameterization == "slope") 
        long.id <- NULL
    }
    idT <- extra$ii
    WW <- if (!LongFormat) {
      cbind(W, long.id, longD.id)
    } else {
      cbind(W, long.id[idT], longD.id[idT])
    }
    if (method %in% c("Cox-PH-GH", "weibull-PH-GH", "piecewise-PH-GH", 
                      "spline-PH-GH", "spline-PH-Laplace")) {
      if (!LongFormat) {
        DD <- data.frame(id = id, Time = Time[id], d = d[id], times = times)
        if (!is.null(long)) {
          DD$long <- long * WintF.vl[id, , drop = FALSE]
          k <- ncol(DD$long)
        }
        if (!is.null(long.deriv)) {
          DD$longD <- long.deriv * WintF.sl[id, , drop = FALSE]
          l <- ncol(DD$longD)
        }
        dW <- as.data.frame(W[id, , drop = FALSE], row.names = row.names(DD))
        if (ncol(dW)) {
          names(dW) <- paste("W", seq_along(dW), sep = "")
          DD <- cbind(DD, dW)
        }
      } else {
        DD <- data.frame(Time = Time, d = d)
        if (!is.null(long)) {
          DD$long <- as.vector(long.id[idT]) * WintF.vl
          k <- ncol(DD$long)
        }
        if (!is.null(long.deriv)) {
          DD$longD <- as.vector(longD.id[idT]) * WintF.sl
          l <- ncol(DD$longD)
        }
        dW <- as.data.frame(W, row.names = row.names(DD))
        if (ncol(dW)) {
          names(dW) <- paste("W", seq_along(dW), sep = "")
          DD <- cbind(DD, dW)
        }
        DD$strata <- extra$strata
      }
      if (!LongFormat) {
        DD$start <- DD$times
        DD$stop <- unlist(lapply(split(DD[c("id", "start", "Time")], DD$id), 
                                 function (d) c(d$start[-1], d$Time[1])))
        DD$event <- ave(DD$d, DD$id, FUN = function(x) {
          if (length(x) == 1) {
            x
          } else {
            x[seq(length(x) - 1)] <- 0
            x
          }
        })
      }
      baseCovs <- if (ncol(dW)) {
        paste("+", paste(names(dW), collapse = " + "))
      } else 
        NULL
      form <- if (!LongFormat) {
        switch(parameterization,
               "value" = paste("Surv(start, stop, event) ~", "long", baseCovs),
               "slope" = paste("Surv(start, stop, event) ~", "longD", baseCovs),
               "both" = paste("Surv(start, stop, event) ~", "long + longD", baseCovs))
      } else {
        switch(parameterization,
               "value" = paste("Surv(Time, d) ~", "long", baseCovs),
               "slope" = paste("Surv(Time, d) ~", "longD", baseCovs),
               "both" = paste("Surv(Time, d) ~", "long + longD", baseCovs))
      }
      if (!is.null(DD$strata))
        form <- paste(form, "+ strata(strata)")
      form <- as.formula(form)
      cph <- coxph(form, data = DD)
      coefs <- cph$coefficients
      out <- switch(parameterization,
                    "value" = list(alpha = coefs[1:k], gammas = coefs[-(1:k)]), 
                    "slope" = list(Dalpha = coefs[1:l], gammas = coefs[-(1:l)]),
                    "both" = list(alpha = coefs[1:k], Dalpha = coefs[(k+1):(k+l)], 
                                  gammas = coefs[-(1:(k+l))])
      )
      if (method == "Cox-PH-GH") {
        out$lambda0 <- basehaz(cph, FALSE)$hazard
      }
      if (method == "weibull-PH-GH") {
        dat <- data.frame(Time = Time, d = d)
        init.fit <- survreg(Surv(Time, d) ~ WW, data = dat)
        coefs <- - init.fit$coef / init.fit$scale
        out$gammas <- c(coefs[1], out$gammas)
        out$sigma.t <- 1 / init.fit$scale
      }
      if (method == "piecewise-PH-GH") {
        dat <- data.frame(Time = Time, d = d)
        cph. <- coxph(Surv(Time, d) ~ WW, data = dat, x = TRUE)
        init.fit <- piecewiseExp.ph(cph., knots = extra$control$knots)
        coefs <- init.fit$coef
        out$xi <- exp(coefs[grep("xi", names(coefs))])
      }
      if (method == "spline-PH-GH" || method == "spline-PH-Laplace") {
        if (is.null(extra$strata)) {
          dat <- data.frame(Time = Time, d = d, as.data.frame(WW))
          rn <- tapply(row.names(dat), idT, tail, 1)
          ind <- row.names(dat) %in% rn
          dat <- dat[ind, ]
          init.fit <- survreg(Surv(Time, d) ~ ., data = dat)
          coefs <- init.fit$coef
          xi <- 1 / init.fit$scale
          phi <- exp(coefs[1])
          logh <- -log(phi * xi * dat$Time^(xi - 1))
          out$gammas.bs <- as.vector(lm.fit(extra$W2[ind, ], logh)$coefficients)
        } else {
          dat <- data.frame(Time = Time, d = d)
          dat <- cbind(dat, as.data.frame(WW))
          strata <- extra$strata
          split.dat <- split(dat, strata)
          gg <- NULL
          for (i in seq_along(split.dat)) {
            ii <- strata == levels(strata)[i]
            SpD.i <- split.dat[[i]]
            idT.i <- idT[ii]
            W2.i <- extra$W2[ii, ]
            rn <- tapply(row.names(SpD.i), idT.i, tail, 1)
            ind <- row.names(SpD.i) %in% rn
            SpD.i <- SpD.i[ind, ]
            init.fit <- survreg(Surv(Time, d) ~ ., data = SpD.i)
            coefs <- init.fit$coef
            xi <- 1 / init.fit$scale
            phi <- exp(coefs[1])
            logh <- -log(phi * xi * SpD.i$Time^(xi - 1))
            gg <- c(gg, as.vector(lm.fit(W2.i[ind, ], logh)$coefficients))
          }
          out$gammas.bs <- gg[!is.na(gg)]
        }
        out
      }
    }
    if (method == "weibull-AFT-GH") {
      dat <- data.frame(Time = Time, d = d)
      if (!is.null(long.id)) {
        long.id <- c(long.id) * WintF.vl
        k <- ncol(WintF.vl)
      }
      if (!is.null(longD.id)) {
        longD.id <- c(longD.id) * WintF.sl
        l <- ncol(WintF.sl)
      }         
      WW <- cbind(W, long.id, longD.id)
      init.fit <- survreg(Surv(Time, d) ~ WW, data = dat)
      coefs <- - init.fit$coef
      nk <- if (is.null(W)) 1 else ncol(W) + 1
      out <- switch(parameterization, 
                    "value" = list(gammas = coefs[1:nk], alpha = coefs[-(1:nk)], sigma.t = 1 / init.fit$scale),
                    "slope" = list(gammas = coefs[1:nk], Dalpha = coefs[-(1:nk)], sigma.t = 1 / init.fit$scale),
                    "both" = list(gammas = coefs[1:nk], alpha = coefs[seq(nk+1, nk+k)], 
                                  Dalpha = coefs[-seq(1, nk+k)], sigma.t = 1 / init.fit$scale)
      )
    }
    if (method == "ch-Laplace") {
      dat <- data.frame(Time = Time, d = d)
      init.fit <- survreg(Surv(Time, d) ~ WW, data = dat)
      coefs <- - coef(init.fit) / init.fit$scale
      min.x <- min(logT)
      max.x <- max(logT)
      kn <- if (is.null(extra$control$knots)) {
        kk <- seq(0, 1, length.out = extra$control$lng.in.kn + 2)[-c(1, extra$control$lng.in.kn + 2)]
        quantile(log(Time)[d == 1], kk, names = FALSE)
      } else {
        extra$control$knots
      }
      kn <- sort(c(rep(c(min.x, max.x), extra$control$ord), kn))
      W <- splineDesign(kn, log(Time), ord = extra$control$ord)
      nk <- ncol(W)
      nx <- NCOL(X)
      logH <- coefs[1] + logT / init.fit$scale
      coefs <- c(as.vector(lm.fit(W, logH)$coefficients), coefs[-1])
      out <- list(gammas = c(sort(coefs[1:nk]), if (nx > 1) coefs[seq(nk + 1, nk + nx - 1)] else NULL), 
                  alpha = coefs[nk + nx])
    }
    out
  }


piecewisePHGH.fit <-
  function (x, y, id, initial.values, parameterization, derivForm, control) {
    # response vectors
    logT <- as.vector(y$logT)
    d <- as.vector(y$d)
    ind.D <- y$ind.D
    y <- as.vector(y$y)
    # design matrices
    X <- x$X
    Xtime <- x$Xtime
    Xs <- x$Xs
    Xtime.deriv <- x$Xtime.deriv
    Xs.deriv <- x$Xs.deriv    
    Z <- x$Z
    Ztime <- x$Ztime
    Zs <- x$Zs
    Ztime.deriv <- x$Ztime.deriv
    Zs.deriv <- x$Zs.deriv
    WW <- x$W
    WintF.vl <- x$WintF.vl
    WintF.sl <- x$WintF.sl
    Ws.intF.vl <- x$Ws.intF.vl
    Ws.intF.sl <- x$Ws.intF.sl
    X <- dropAttr(X); Z <- dropAttr(Z); WW <- dropAttr(WW)
    WintF.vl <- dropAttr(WintF.vl); WintF.sl <- dropAttr(WintF.sl)
    Ws.intF.vl <- dropAttr(Ws.intF.vl); Ws.intF.sl <- dropAttr(Ws.intF.sl)
    if (parameterization == "value") {
      Xtime <- dropAttr(Xtime); Ztime <- dropAttr(Ztime); Xs <- dropAttr(Xs); Zs <- dropAttr(Zs)
    } else if (parameterization == "slope") {
      Xtime.deriv <- dropAttr(Xtime.deriv); Ztime.deriv <- dropAttr(Ztime.deriv)
      Xs.deriv <- dropAttr(Xs.deriv); Zs.deriv <- dropAttr(Zs.deriv)
    } else {
      Xtime <- dropAttr(Xtime); Ztime <- dropAttr(Ztime); Xs <- dropAttr(Xs); Zs <- dropAttr(Zs)
      Xtime.deriv <- dropAttr(Xtime.deriv); Ztime.deriv <- dropAttr(Ztime.deriv)
      Xs.deriv <- dropAttr(Xs.deriv); Zs.deriv <- dropAttr(Zs.deriv)
    }
    indFixed <- derivForm$indFixed
    indRandom <- derivForm$indRandom
    # sample size settings
    ncx <- ncol(X)
    ncz <- ncol(Z)
    ncww <- if (!is.null(WW)) ncol(WW) else 0
    n <- length(logT)
    N <- length(y)
    ni <- as.vector(tapply(id, id, length))
    Q <- x$Q
    # crossproducts and others
    XtX <- crossprod(X)
    ZtZ <- lapply(split(Z, id), function (x) crossprod(matrix(x, ncol = ncz)))
    names(ZtZ) <- NULL
    ZtZ <- matrix(unlist(ZtZ), n, ncz * ncz, TRUE)
    outer.Ztime <- lapply(1:n, function (x) Ztime[x, ] %o% Ztime[x, ])
    # Gauss-Hermite quadrature rule components
    GH <- gauher(control$GHk)
    b <- as.matrix(expand.grid(rep(list(GH$x), ncz)))
    k <- nrow(b)
    wGH <- as.matrix(expand.grid(rep(list(GH$w), ncz)))    
    wGH <- 2^(ncz/2) * apply(wGH, 1, prod) * exp(rowSums(b * b))
    if (control$typeGH == "simple") {
      b <- sqrt(2) * t(control$inv.chol.VC %*% t(b))
      wGH <- wGH * control$det.inv.chol.VC
    } else { 
      b <- sqrt(2) * b
      VCdets <- control$det.inv.chol.VCs
    }
    dimnames(b) <- NULL
    b2 <- if (ncz == 1) b * b else t(apply(b, 1, function (x) x %o% x))
    Ztb <- Z %*% t(b)
    if (parameterization %in% c("value", "both")) {
      Ztime.b <- Ztime %*% t(b)
      Zsb <- Zs %*% t(b)
    }
    if (parameterization %in% c("slope", "both")) {
      if (length(indRandom) > 1 || indRandom) {
        Ztime.b.deriv <- Ztime.deriv %*% t(b[, indRandom, drop = FALSE])
        Zsb.deriv <- Zs.deriv %*% t(b[, indRandom, drop = FALSE])
      } else {
        Ztime.b.deriv <- matrix(0, nrow(Ztime.deriv), k)
        Zsb.deriv <- matrix(0, nrow(Zs.deriv), k)
      }
    }
    # Gauss-Kronrod rule
    nk <- control$GKk
    id.GK <- x$id.GK
    st <- x$st
    ind.K <- rep(unlist(lapply(ind.D, seq_len)), each = nk)
    wk <- unlist(lapply(ind.D, function (n) rep(x$wk, n)))
    wkP <- wk * rep(x$P, each = nk)
    # pseudo-adaptive Gauss-Hermite
    if (control$typeGH != "simple") {
      lis.b <- vector("list", n)
      for (i in 1:n)
        lis.b[[i]] <- t(control$inv.chol.VCs[[i]] %*% t(b)) + 
          rep(control$ranef[i, ], each = k)
      lis.b2 <- lapply(lis.b, function (b) if (ncz == 1) b * b else
        t(apply(b, 1, function (x) x %o% x)))
      for (i in 1:n) {
        Ztb[id == i, ] <- Z[id == i, , drop = FALSE] %*% t(lis.b[[i]])
        if (parameterization %in% c("value", "both")) {
          Ztime.b[i, ] <- Ztime[i, , drop = FALSE] %*% t(lis.b[[i]])
          Zsb[id.GK == i, ] <- Zs[id.GK == i, ] %*% t(lis.b[[i]])
        }
        if (parameterization %in% c("slope", "both") && 
            (length(indRandom) > 1 || indRandom)) {
          Ztime.b.deriv[i, ] <- Ztime.deriv[i, , drop = FALSE] %*% t(lis.b[[i]][, indRandom, drop = FALSE])
          Zsb.deriv[id.GK == i, ] <- Zs.deriv[id.GK == i, ] %*% t(lis.b[[i]][, indRandom, drop = FALSE])
        }            
      }
    }
    # initial values
    betas <- as.vector(initial.values$betas)
    sigma <- initial.values$sigma
    gammas <- if (!is.null(WW)) as.vector(initial.values$gammas) else NULL
    alpha <- as.vector(initial.values$alpha)
    Dalpha <- as.vector(initial.values$Dalpha)
    xi <- initial.values$xi
    D <- initial.values$D
    diag.D <- !is.matrix(D)
    if (!diag.D) dimnames(D) <- NULL else names(D) <- NULL
    # fix environments for functions
    environment(opt.survPC) <- environment(gr.survPC) <- environment()
    environment(opt.longPC) <- environment(gr.longPC) <- environment(H.longPC) <- environment()
    environment(LogLik.piecewiseGH) <- environment(Score.piecewiseGH) <- environment()
    old <- options(warn = (-1))
    on.exit(options(old))
    # EM iterations
    iter <- control$iter.EM
    Y.mat <- matrix(0, iter + 1, ncx + 1)
    T.mat <- matrix(0, iter + 1, switch(parameterization, 
                                        "value" = ncww + length(xi) + ncol(WintF.vl), 
                                        "slope" = ncww + length(xi) + ncol(WintF.sl), 
                                        "both" = ncww + length(xi) + ncol(WintF.vl) + ncol(WintF.sl)))
    B.mat <- if (diag.D) matrix(0, iter + 1, ncz) else matrix(0, iter + 1, ncz * ncz)
    lgLik <- numeric(iter + 1)
    conv <- TRUE
    for (it in 1:iter) {
      # save parameter values in matrix
      Y.mat[it, ] <- c(betas, sigma)
      T.mat[it, ] <- switch(parameterization, "value" = c(gammas, alpha, log(xi)), 
                            "slope" = c(gammas, Dalpha, log(xi)), "both" = c(gammas, alpha, Dalpha, log(xi)))
      B.mat[it,] <- D
      
      # linear predictors
      eta.yx <- as.vector(X %*% betas)
      eta.tw <- if (!is.null(WW)) as.vector(WW %*% gammas) else 0
      if (parameterization %in% c("value", "both")) {
        Y <- as.vector(Xtime %*% betas) + Ztime.b
        Ys <- as.vector(Xs %*% betas) + Zsb
        eta.t <- eta.tw + c(WintF.vl %*% alpha) * Y
        eta.s <- c(Ws.intF.vl %*% alpha) * Ys
      }
      if (parameterization %in% c("slope", "both")) {
        Y.deriv <- as.vector(Xtime.deriv %*% betas[indFixed]) + Ztime.b.deriv
        Ys.deriv <- as.vector(Xs.deriv %*% betas[indFixed]) + Zsb.deriv
        eta.t <- if (parameterization == "both")
          eta.t + c(WintF.sl %*% Dalpha) * Y.deriv
        else
          eta.tw + c(WintF.sl %*% Dalpha) * Y.deriv
        eta.s <- if (parameterization == "both")
          eta.s + c(Ws.intF.sl %*% Dalpha) * Ys.deriv 
        else
          c(Ws.intF.sl %*% Dalpha) * Ys.deriv
      }
      
      # E-step
      mu.y <- eta.yx + Ztb
      logNorm <- dnorm(y, mu.y, sigma, TRUE)
      log.p.yb <- rowsum(logNorm, id, reorder = FALSE); dimnames(log.p.yb) <- NULL
      log.hazard <- log(xi[ind.D]) + eta.t
      log.survival <- - exp(eta.tw) * rowsum(xi[ind.K] * wkP * exp(eta.s), id.GK, reorder = FALSE)
      dimnames(log.survival) <- NULL
      log.p.tb <- d * log.hazard + log.survival
      log.p.b <- if (control$typeGH == "simple") {
        rep(dmvnorm(b, rep(0, ncz), D, TRUE), each = n)
      } else {
        matrix(dmvnorm(do.call(rbind, lis.b), rep(0, ncz), D, TRUE), n, k, byrow = TRUE)
      }
      p.ytb <- exp(log.p.yb + log.p.tb + log.p.b)
      if (control$typeGH != "simple")
        p.ytb <- p.ytb * VCdets
      p.yt <- c(p.ytb %*% wGH)
      p.byt <- p.ytb / p.yt
      post.b <- if (control$typeGH == "simple") {
        p.byt %*% (b * wGH)
      } else {
        sapply(seq_len(ncz), function (i)
          (p.byt * t(sapply(lis.b, "[", seq_len(k), i))) %*% wGH)
      }
      post.vb <- if (control$typeGH == "simple") {
        if (ncz == 1) {
          c(p.byt %*% (b2 * wGH)) - c(post.b * post.b)
        } else {
          (p.byt %*% (b2 * wGH)) - t(apply(post.b, 1, function (x) x %o% x))
        }
      } else {
        dd <- sapply(seq_len(ncz^2), function (i)
          (p.byt * t(sapply(lis.b2, "[", seq_len(k), i))) %*% wGH)
        bb <- apply(post.b, 1, function (x) x %o% x)
        dd - if (ncz == 1) c(bb) else t(bb)
      }
      
      # compute log-likelihood
      log.p.yt <- log(p.yt)
      lgLik[it] <- sum(log.p.yt[is.finite(log.p.yt)], na.rm = TRUE)
      
      # print results if verbose
      if (control$verbose) {
        cat("\n\niter:", it, "\n")
        cat("log-likelihood:", lgLik[it], "\n")
        cat("betas:", round(betas, 4), "\n")
        cat("sigma:", round(sigma, 4), "\n")
        if (!is.null(WW)) cat("gammas:", round(gammas, 4), "\n")
        if (parameterization %in% c("value", "both")) cat("alpha:", round(alpha, 4), "\n")
        if (parameterization %in% c("slope", "both")) cat("alphaD:", round(Dalpha, 4), "\n")
        cat("xi:", round(xi, 4), "\n")
        cat("D:", if (!diag.D) round(D[lower.tri(D, TRUE)], 4) else round(D, 4), "\n")
      }
      
      # check convergence
      if (it > 5 && lgLik[it] > lgLik[it - 1]) {
        thets1 <- c(Y.mat[it - 1, ], T.mat[it - 1, ], B.mat[it - 1, ])
        thets2 <- c(Y.mat[it, ], T.mat[it, ], B.mat[it, ])
        check1 <- max(abs(thets2 - thets1) / (abs(thets1) + control$tol1)) < control$tol2
        check2 <- (lgLik[it] - lgLik[it - 1]) < control$tol3 * (abs(lgLik[it - 1]) + control$tol3)
        if (check1 || check2) {
          conv <- FALSE
          if (control$verbose)
            cat("\n\nconverged!\n")
          break
        }
      }
      if (iter == 0) break
      
      # M-step
      Zb <- rowSums(Z * post.b[id, ], na.rm = TRUE)
      mu <- y - eta.yx
      tr.tZZvarb <- sum(ZtZ * post.vb, na.rm = TRUE)
      sigman <- sqrt(c(crossprod(mu, mu - 2 * Zb) + crossprod(Zb) + tr.tZZvarb) / N)
      Dn <- if (control$typeGH == "simple") {
        matrix(colMeans(p.byt %*% (b2 * wGH), na.rm = TRUE), ncz, ncz)
      } else {
        matrix(colMeans(dd, na.rm = TRUE), ncz, ncz)
      }
      Dn <- if (diag.D) diag(Dn) else 0.5 * (Dn + t(Dn))
      Hbetas <- nearPD(H.longPC(betas))
      scbetas <- gr.longPC(betas)
      betasn <- betas - c(solve(Hbetas, scbetas))
      list.thetas <- list(gammas = gammas, alpha = alpha, Dalpha = Dalpha, log.xi = log(xi))
      list.thetas <- list.thetas[!sapply(list.thetas, is.null)]
      thetas <- unlist(as.relistable(list.thetas))
      
      gr.survPC(thetas)
      
      optz.surv <- optim(thetas, opt.survPC, gr.survPC, method = "BFGS", 
                         control = list(maxit = if (it < 5) 20 else 5, 
                                        parscale = if (it < 5) rep(0.01, length(thetas)) else rep(0.1, length(thetas))))
      thetasn <- relist(optz.surv$par, skeleton = list.thetas)
      
      # update parameter values
      betas <- betasn
      sigma <- sigman
      D <- Dn
      gammas <- thetasn$gammas
      alpha <- thetasn$alpha
      Dalpha <- thetasn$Dalpha
      xi <- exp(thetasn$log.xi)        
    }
    list.thetas <- list(betas = betas, log.sigma = log(sigma), gammas = gammas, 
                        alpha = alpha, Dalpha = Dalpha, log.xi = log(xi), 
                        D = if (diag.D) log(D) else chol.transf(D))    
    list.thetas <- list.thetas[!sapply(list.thetas, is.null)]
    thetas <- unlist(as.relistable(list.thetas))
    lgLik <- - LogLik.piecewiseGH(thetas)
    # if not converged, start quasi-Newton iterations
    if (conv && !control$only.EM) {
      if (is.null(control$parscale))
        control$parscale <- rep(0.01, length(thetas))
      if (control$verbose)
        cat("\n\nquasi-Newton iterations start.\n\n")
      out <- if (control$optimizer == "optim") {
        optim(thetas, LogLik.piecewiseGH, Score.piecewiseGH, method = "BFGS",
              control = list(maxit = control$iter.qN, parscale = control$parscale, 
                             trace = 10 * control$verbose))
      } else {
        nlminb(thetas, LogLik.piecewiseGH, Score.piecewiseGH, scale = control$parscale, 
               control = list(iter.max = control$iter.qN, trace = 1 * control$verbose))
      }
      if ((conv <- out$convergence) == 0 || - out[[2]] > lgLik) {
        lgLik <- - out[[2]]
        thetas <- relist(out$par, skeleton = list.thetas)
        betas <- thetas$betas
        sigma <- exp(thetas$log.sigma)
        gammas <- thetas$gammas
        alpha <- thetas$alpha
        Dalpha <- thetas$Dalpha
        xi <- exp(thetas$log.xi)
        D <- thetas$D
        D <- if (diag.D) exp(D) else chol.transf(D)
        it <- it + if (control$optimizer == "optim") out$counts[1] else out$iterations
        # compute posterior moments for thetas after quasi-Newton
        eta.yx <- as.vector(X %*% betas)
        eta.tw <- if (!is.null(WW)) as.vector(WW %*% gammas) else 0
        if (parameterization %in% c("value", "both")) {
          Y <- as.vector(Xtime %*% betas) + Ztime.b
          Ys <- as.vector(Xs %*% betas) + Zsb
          eta.t <- eta.tw + c(WintF.vl %*% alpha) * Y
          eta.s <- c(Ws.intF.vl %*% alpha) * Ys
        }
        if (parameterization %in% c("slope", "both")) {
          Y.deriv <- as.vector(Xtime.deriv %*% betas[indFixed]) + Ztime.b.deriv
          Ys.deriv <- as.vector(Xs.deriv %*% betas[indFixed]) + Zsb.deriv
          eta.t <- if (parameterization == "both")
            eta.t + c(WintF.sl %*% Dalpha) * Y.deriv
          else
            eta.tw + c(WintF.sl %*% Dalpha) * Y.deriv
          eta.s <- if (parameterization == "both")
            eta.s + c(Ws.intF.sl %*% Dalpha) * Ys.deriv 
          else
            c(Ws.intF.sl %*% Dalpha) * Ys.deriv
        }
        exp.eta.tw <- exp(eta.tw)
        mu.y <- eta.yx + Ztb
        logNorm <- dnorm(y, mu.y, sigma, TRUE)
        log.p.yb <- rowsum(logNorm, id)
        log.hazard <- log(xi[ind.D]) + eta.t
        log.survival <- - exp(eta.tw) * rowsum(xi[ind.K] * wkP * exp(eta.s), id.GK, reorder = FALSE)
        dimnames(log.survival) <- NULL
        log.p.tb <- d * log.hazard + log.survival            
        log.p.b <- if (control$typeGH == "simple") {
          rep(dmvnorm(b, rep(0, ncz), D, TRUE), each = n)
        } else {
          matrix(dmvnorm(do.call(rbind, lis.b), rep(0, ncz), D, TRUE), n, k, byrow = TRUE)
        }
        p.ytb <- exp(log.p.yb + log.p.tb + log.p.b)
        if (control$typeGH != "simple")
          p.ytb <- p.ytb * VCdets
        p.yt <- c(p.ytb %*% wGH)
        p.byt <- p.ytb / p.yt
        post.b <- if (control$typeGH == "simple") {
          p.byt %*% (b * wGH)
        } else {
          sapply(seq_len(ncz), function (i)
            (p.byt * t(sapply(lis.b, "[", seq_len(k), i))) %*% wGH)
        }
        post.vb <- if (control$typeGH == "simple") { 
          if (ncz == 1) {
            c(p.byt %*% (b2 * wGH)) - c(post.b * post.b)
          } else {
            (p.byt %*% (b2 * wGH)) - t(apply(post.b, 1, function (x) x %o% x))
          }
        } else {
          dd <- sapply(seq_len(ncz^2), function (i)
            (p.byt * t(sapply(lis.b2, "[", seq_len(k), i))) %*% wGH)
          bb <- apply(post.b, 1, function (x) x %o% x)
          dd - if (ncz == 1) c(bb) else t(bb)
        }
        Zb <- if (ncz == 1) post.b[id] else rowSums(Z * post.b[id, ], na.rm = TRUE)
      }
    }
    # calculate score vector
    Score <- Score.piecewiseGH(unlist(thetas))
    # calculate Hessian matrix
    if (control$verbose) cat("\ncalculating Hessian...\n")
    Hessian <- if (control$numeriDeriv == "fd") {
      fd.vec(unlist(thetas), Score.piecewiseGH, eps = control$eps.Hes)
    } else { 
      cd.vec(unlist(thetas), Score.piecewiseGH, eps = control$eps.Hes)
    }
    names(betas) <- names(initial.values$betas)
    if (!diag.D) dimnames(D) <- dimnames(initial.values$D) else names(D) <- names(initial.values$D)
    if (!is.null(WW))
      names(gammas) <- colnames(x$W)
    nm.alph <- colnames(x$WintF.vl)
    nm.alph <- if (!is.null(nm.alph)) {
      if (nm.alph[1] == "(Intercept)")
        c("", nm.alph[-1])
      else
        nm.alph
    } else {
      "alpha"
    }
    nm.Dalph <- colnames(x$WintF.sl)
    nm.Dalph <- if (!is.null(nm.Dalph)) {
      if (nm.Dalph[1] == "(Intercept)")
        c("", nm.Dalph[-1])
      else
        nm.Dalph
    } else {
      "alpha.s"
    }
    gg <- switch(parameterization, "value" = nm.alph, "slope" = nm.Dalph, "both" = c(nm.alph, nm.Dalph))
    if (parameterization %in% c("value", "both"))
      names(alpha) <- nm.alph
    if (parameterization %in% c("slope", "both"))
      names(Dalpha) <- nm.Dalph
    names(xi) <- paste("xi.", seq_len(Q), sep = "")
    nams <- c(paste("Y.", c(names(betas), "sigma"), sep = ""), paste("T.", c(names(gammas), gg, names(xi)), sep = ""),
              paste("B.", if (!diag.D) paste("D", seq(1, ncz * (ncz + 1) / 2), sep = "") else names(D), sep = ""))
    dimnames(Hessian) <- list(nams, nams)
    colnames(post.b) <- colnames(x$Z)
    list(coefficients = list(betas = betas, sigma = sigma, gammas = gammas, alpha = alpha, Dalpha = Dalpha, xi = xi, 
                             D = as.matrix(D)), Score = Score, Hessian = Hessian, logLik = lgLik, EB = list(post.b = post.b, post.vb = post.vb, 
                                                                                                            Zb = if (iter == 0) rowSums(Z * post.b[id, ], na.rm = TRUE) else Zb, 
                                                                                                            Ztimeb = if (parameterization %in% c("value", "both")) rowSums(Ztime * post.b) else NULL,
                                                                                                            Ztimeb.deriv = if (parameterization %in% c("slope", "both")) {
                                                                                                              if (indRandom) rowSums(Ztime.deriv * post.b[, indRandom, drop = FALSE]) else rep(0, nrow(Ztime.deriv))
                                                                                                            } else NULL), 
         iters = it, convergence = conv, n = n, N = N, ni = ni, d = d, id = id)
  }

dropAttr <-
  function (mat) {
    d <- dim(mat)
    mat <- as.vector(mat)
    dim(mat) <- d
    mat
  }

gauher <-
  function (n) {
    m <- trunc((n + 1)/2)
    x <- w <- rep(-1, n)
    for (i in seq_len(m)) {
      z <- if (i == 1) {
        sqrt(2*n + 1) - 1.85575 * (2*n + 1)^(-0.16667)
      } else if (i == 2) {
        z - 1.14 * n^0.426 / z
      } else if (i == 3) {
        1.86 * z - 0.86 * x[1]
      } else if (i == 4) {
        1.91 * z - 0.91 * x[2]
      } else {
        2*z - x[i - 2]
      }
      for (its in seq_len(10)) {
        p1 <- 0.751125544464943
        p2 <- 0
        for (j in seq_len(n)) {
          p3 <- p2
          p2 <- p1
          p1 <- z * sqrt(2/j) * p2 - sqrt((j - 1)/j) * p3
        }
        pp <- sqrt(2*n) * p2
        z1 <- z
        z <- z1 - p1/pp
        if (abs(z - z1) <= 3e-14) 
          break
      }
      x[i] <- z
      x[n + 1 - i] <- -z
      w[i] <- 2 / (pp * pp)
      w[n + 1 - i] <- w[i]
    }
    list(x = x, w = w)
  }

gr.survPC <-
  function (thetas) {
    thetas <- relist(thetas, skeleton = list.thetas)
    gammas <- thetas$gammas
    alpha <- thetas$alpha
    Dalpha <- thetas$Dalpha
    xi <- exp(thetas$log.xi)
    eta.tw <- if (!is.null(WW)) as.vector(WW %*% gammas) else rep(0, n)
    eta.t <- switch(parameterization, 
                    "value" = eta.tw + c(WintF.vl %*% alpha) * Y, 
                    "slope" = eta.tw + c(WintF.sl %*% Dalpha) * Y.deriv, 
                    "both" = eta.tw + c(WintF.vl %*% alpha) * Y + c(WintF.sl %*% Dalpha) * Y.deriv)    
    exp.eta.s <- exp(switch(parameterization, 
                            "value" = c(Ws.intF.vl %*% alpha) * Ys, 
                            "slope" = c(Ws.intF.sl %*% Dalpha) * Ys.deriv, 
                            "both" = c(Ws.intF.vl %*% alpha) * Ys + c(Ws.intF.sl %*% Dalpha) * Ys.deriv))
    exp.eta.tw <- exp(eta.tw)
    Int <-  wkP * exp.eta.s
    Int2 <- xi[ind.K] * Int
    scgammas <- if (!is.null(WW)) {
      - colSums(WW * (d - c((p.byt * (exp.eta.tw * 
                                        rowsum(Int2, id.GK, reorder = FALSE))) %*% wGH)), na.rm = TRUE)
    } else NULL
    scalpha <- if (parameterization %in% c("value", "both")) {
      rr <- numeric(ncol(WintF.vl))
      for (k in seq_along(rr)) 
        rr[k] <- - sum((p.byt * (d * WintF.vl[, k] * Y - exp.eta.tw * 
                                   rowsum(Int2 * Ws.intF.vl[, k] * Ys, id.GK, reorder = FALSE))) %*% wGH, na.rm = TRUE)
      rr
    } else NULL
    scalpha.D <- if (parameterization %in% c("slope", "both")) {
      rr <- numeric(ncol(WintF.sl))
      for (k in seq_along(rr)) 
        rr[k] <- - sum((p.byt * (d * WintF.sl[, k] * Y.deriv - exp.eta.tw * 
                                   rowsum(Int2 * Ws.intF.sl[, k] * Ys.deriv, id.GK, reorder = FALSE))) %*% wGH, na.rm = TRUE)
      rr
    } else NULL
    scxi <- numeric(Q)
    for (i in 1:Q) {
      i1 <- ind.D == i
      i2 <- ind.K == i
      i3 <- ind.D >= i
      ki <- c((p.byt[i3, ] * (exp.eta.tw[i3] * rowsum(Int[i2, ], id.GK[i2], reorder = FALSE))) %*% wGH)
      kk <- numeric(n); kk[i3] <- ki
      scxi[i] <- - xi[i] * sum((d * i1)/xi[i] - kk)
    }
    c(scgammas, scalpha, scalpha.D, scxi)
  }

opt.survPC <-
  function (thetas) {
    thetas <- relist(thetas, skeleton = list.thetas)
    gammas <- thetas$gammas
    alpha <- thetas$alpha
    Dalpha <- thetas$Dalpha
    xi <- exp(thetas$log.xi)
    eta.tw <- if (!is.null(WW)) as.vector(WW %*% gammas) else 0
    eta.t <- switch(parameterization, 
                    "value" = eta.tw + c(WintF.vl %*% alpha) * Y, 
                    "slope" = eta.tw + c(WintF.sl %*% Dalpha) * Y.deriv, 
                    "both" = eta.tw + c(WintF.vl %*% alpha) * Y + c(WintF.sl %*% Dalpha) * Y.deriv)    
    eta.s <- switch(parameterization, 
                    "value" = c(Ws.intF.vl %*% alpha) * Ys, 
                    "slope" = c(Ws.intF.sl %*% Dalpha) * Ys.deriv, 
                    "both" = c(Ws.intF.vl %*% alpha) * Ys + c(Ws.intF.sl %*% Dalpha) * Ys.deriv)
    log.hazard <- log(xi[ind.D]) + eta.t
    log.survival <- - exp(eta.tw) * rowsum(xi[ind.K] * wkP * exp(eta.s), id.GK, reorder = FALSE)
    dimnames(log.survival) <- NULL
    log.p.tb <- d * log.hazard + log.survival    
    p.bytn <- p.byt * log.p.tb
    -sum(p.bytn %*% wGH, na.rm = TRUE)
  }


H.longPC <-
  function (betas) {
    eta.yx <- as.vector(X %*% betas)
    if (parameterization %in% c("value", "both")) {
      Ys <- as.vector(Xs %*% betas) + Zsb
      Ws.intF.vl.alph <- c(Ws.intF.vl %*% alpha)
      eta.s <- Ws.intF.vl.alph * Ys
    }
    if (parameterization %in% c("slope", "both")) {
      Ys.deriv <- as.vector(Xs.deriv %*% betas[indFixed]) + Zsb.deriv
      Ws.intF.sl.alph <- c(Ws.intF.sl %*% Dalpha)
      eta.s <- if (parameterization == "both")
        eta.s + Ws.intF.sl.alph * Ys.deriv 
      else
        Ws.intF.sl.alph * Ys.deriv
    }
    exp.eta.tw <- exp(eta.tw)
    H1 <- XtX / sigma^2
    Int <- xi[ind.K] * wkP * exp(eta.s)
    H2 <- H1
    H2 <- matrix(0, ncx, ncx)
    for (i in 1:ncx) {
      for (j in i:ncx) {
        XX <- if (parameterization == "value") {
          Ws.intF.vl.alph^2 * Xs[, i] * Xs[, j]
        } else if (parameterization == "slope") {
          if (i %in% indFixed && j %in% indFixed) {
            ii <- match(i, indFixed)
            jj <- match(j, indFixed)
            Ws.intF.sl.alph^2 * Xs.deriv[, ii] * Xs.deriv[, jj]
          } else
            0
        } else {
          if (i %in% indFixed && j %in% indFixed) {
            ii <- match(i, indFixed)
            jj <- match(j, indFixed)
            (Ws.intF.vl.alph * Xs[, i] + Ws.intF.sl.alph * Xs.deriv[, ii]) * 
              (Ws.intF.vl.alph * Xs[, j] + Ws.intF.sl.alph * Xs.deriv[, jj])
          } else if (i %in% indFixed && !j %in% indFixed) {
            ii <- match(i, indFixed)
            (Ws.intF.vl.alph * Xs[, i] + Ws.intF.sl.alph * Xs.deriv[, ii]) * 
              (Ws.intF.vl.alph * Xs[, j])
          } else if (!i %in% indFixed && j %in% indFixed) {
            jj <- match(j, indFixed)
            (Ws.intF.vl.alph * Xs[, i]) * (Ws.intF.vl.alph * Xs[, j] + 
                                             Ws.intF.sl.alph * Xs.deriv[, jj])
          } else {
            Ws.intF.vl.alph^2 * Xs[, i] * Xs[, j]
          }
        }
        ki <- exp.eta.tw * rowsum(Int * XX, id.GK, reorder = FALSE)
        kii <- c((p.byt * ki) %*% wGH)
        H2[i, j] <- sum(kii, na.rm = TRUE)
      }
    }
    H2[lower.tri(H2)] <- t(H2)[lower.tri(H2)]
    H1 + H2
  }



