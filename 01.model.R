################################################################################
# SUPPLEMENTAL MATERIAL of the article:
#   "Association of inter and intra-day temperature change with mortality"
#   Vicedo-Cabrera AM, Forsberg F, Tobias A, Zanobetti A, Schwartz J,
#     Armstrong B, Gasparrini A
#   American Journal of Epidemiology - 2016
#
# This code reproduces the analysis for London, UK
#
# 14 March 2016
# * an updated version of this code, (hopefully) compatible with future
#   versions of the software, is available at the personal website of the
#   last author (www.ag-myresearch.com)
################################################################################

# LOAD THE PACKAGES
library(dlnm) ; library(splines) ; library(foreign)
library(tsModel) ; library(Epi)

# LOAD THE DATA
load("london.RData")
data <- london

# DEFINE OUTCOME
data$out <- data$all

################################################################################
# DERIVE THE CROSS-BASIS

# KNOTS FOR EXPOSURE-RESPONSE FUNCTION
vk <- quantile(data$tmean, c(10, 75, 90) / 100, na.rm = T)

# KNOTS FOR THE LAG-RESPONSE FUNCTION
ldf <- 5
maxlag <- 21
lk <- logknots(maxlag, df = ldf)

# COMPUTE THE CROSS-BASIS
cb <- crossbasis(data$tmean, lag = maxlag, argvar = list(fun = "bs",
  degree = 2, knots = vk), arglag = list(knots = lk))
# SUMMARY
summary(cb)

################################################################################
# RUN THE MODEL AND OBTAIN PREDICTIONS

# RUN THE MODEL
model <- glm(out ~ cb + ns(date, 8 * length(unique(year))) + dow,
  family = quasipoisson(), data)

# DEFINE THE MINIMUM MORTALITY TEMPERATURE AND PERCENTILE (MMT AND MMP)
pred <- crosspred(cb, model)
(mmt <- pred$predvar[which.min(pred$allRRfit)])
(mmp <- sum(na.omit(data$tmean) < mmt) / length(na.omit(data$tmean)))

# CREATE THE BETWEEN-DAY CHANGE IN TEMPERTURE VARIABLES
# (POSITIVE CHANGE ABOVE AND BELOW THE MMT)
bdinc <- pmax(data$tmean - pmax(Lag(data$tmean, 1), mmt), 0)
bddec <- pmax(pmin(Lag(data$tmean, 1), mmt) - data$tmean, 0)

# CREATE THE WITHIN-DAY CHANGE IN TEMPERTURE VARIABLES
# (CHANGE ABOVE AND BELOW THE MMT)
wdhot <- ifelse(data$tmean > mmt, data$tmax - data$tmin, 0)
wdcold <- ifelse(data$tmean < mmt, data$tmax - data$tmin, 0)

# UPDATE MODEL WITH THE CHANGE VARIABLES
model.upd <- update(model, . ~ . + bdinc + bddec + wdhot + wdcold)

# PREDICT
pred.l <- crosspred(cb, model.upd, cen = mmt)

#
