####################################################################
# RUN THE FIRST STAGE MODEL
#   COMPUTING TIME IS ~12SEC (IN A 2.66GHz-4GBRAM PC UNDER WINDOWS)
####################################################################

# BUILT OBJECTS WHERE RESULTS WILL BE STORED:
#   ymat IS THE MATRIX FOR THE OUTCOME PARAMETERS
#   Slist IS THE LIST WITH (CO)VARIANCE MATRICES
ymat <- matrix(NA, 
               m, 
               df, 
               dimnames=list(regions, paste("spl", seq(df), sep="")))
Slist <- vector("list",m)
names(Slist) <- regions

####################################################################
# RUN THE FIRST-STAGE ANALYSIS

# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn = -1)

system.time(
for(i in seq(m)) {
  # PRINT ITERATION
  cat(i,"")

  # LOAD
  data <- datalist[[i]]

  # CREATE THE SPLINE
  # NB: KNOTS AND BOUNDARIES FIXED AT SAME VALUES
  btmean05 <- onebasis(data$tmean05,
                       fun = type,
                       degree = degree,
                       knots = knots,
                       Bound = bound)

  # RUN THE MODEL
  model <- glm(N_DEATHS ~ btmean05 + DOW + ns(TIME,7*14),
               family = quasipoisson(),
               data,
               na.action = na.exclude)
	
  # EXTRACT AND SAVE THE RELATED COEF AND VCOV
  predtmean05 <- crosspred(btmean05, model, cen=cen)
  ymat[i,] <- predtmean05$coef
  Slist[[i]] <- predtmean05$vcov
})

# RESET WARNING
options(warn = 0)

#
