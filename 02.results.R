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

################################################################################
## RR ESTIMATES OF THE DIFFERENT TEMPERATURE INDICATORS

# RR OF MEAN DAILY TEMPERATURE (AT 1 AND 99 PERCENTILES)
RR.h <- unlist(crosspred(cb, model.upd, at = quantile(data$tmean, 0.99,
  na.rm = T), cen = mmt)[c("allRRfit", "allRRlow", "allRRhigh")])
RR.c <- unlist(crosspred(cb, model.upd, at = quantile(data$tmean, 0.01,
  na.rm = T), cen = mmt)[c("allRRfit", "allRRlow", "allRRhigh")])

# BETWEEN-DAY INCREASE IN TEMPERATURE (bdinc)
beta <- coef(model.upd)["bdinc"]
var <- vcov(model.upd)["bdinc", "bdinc"]
median <- median(bdinc[bdinc > 0], na.rm = T)
RR.bdinc <- rbind(c(round(exp(beta * median), 3),
  round(exp((beta + c(-1, 1) * 1.96 * sqrt(var)) * median), 3)))

# BETWEEN-DAY DECREASE IN TEMPERATURE (bddec)
beta <- coef(model.upd)["bddec"]
var <- vcov(model.upd)["bddec", "bddec"]
median <- median(bddec[bddec > 0], na.rm = T)
RR.bddec <- rbind(c(round(exp(beta * median), 3),
  round(exp((beta + c(-1, 1) * 1.96 * sqrt(var)) * median), 3)))

# WITHIN-DAY CHANGE IN TEMPERATURE IN HOT DAYS (wdhot)
beta <- coef(model.upd)["wdhot"]
var <- vcov(model.upd)["wdhot", "wdhot"]
median <- median(wdhot[wdhot > 0], na.rm = T)
RR.wdhot <- rbind(c(round(exp(beta * median), 3),
  round(exp((beta + c(-1, 1) * 1.96 * sqrt(var)) * median), 3)))

# WITHIN-DAY CHANGE IN TEMPERATURE IN COLD DAYS (wdcold)
beta <- coef(model.upd)["wdcold"]
var <- vcov(model.upd)["wdcold", "wdcold"]
median <- median(wdcold[wdcold > 0], na.rm = T)
RR.wdcold <- rbind(c(round(exp(beta * median), 3),
  round(exp((beta + c(-1, 1) * 1.96 * sqrt(var)) * median), 3)))

# GENERATE THE TABLE
RR.table <- rbind(RR.h, RR.c, RR.bdinc, RR.bddec, RR.wdhot, RR.wdcold)
rownames(RR.table) <- c("RR.heat", "RR.cold", "RR.bdinc", "RR.bddec",
  "RR.wdhot", "RR.wdcold")
colnames(RR.table) <- c("RR", "RR.low", "RR.high")
formatC(RR.table, format = "f", digits = 3)

#
