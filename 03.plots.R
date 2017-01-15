################################################################################
# Updated version of the code for the analysis in:
#
#   "Association of inter and intra-day temperature change with mortality"
#   Vicedo-Cabrera AM, Forsberg F, Tobias A, Zanobetti A, Schwartz J,
#     Armstrong B, Gasparrini A
#   American Journal of Epidemiology - 2016
#   http://www.ag-myresearch.com/2016_vicedo-cabrera_aje.html
#
# Update: 15 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2016_vicedo-cabrera_AJE_Rcodedata
################################################################################



################################################################################
### WEB FIGURE 1a

d3 <- plot(pred.l, main = "Exposure-lag-response", xlab = "Temperature (?C)",
  zlab = "RR", phi = 35, theta = 220, ltheta = 170, shade = 0.1, cex.axis = 0.8,
  cex.lab = 0.8)
mtext("London 1993-2006", cex = 0.8)
mmt <- pred.l$predvar[pred.l$allRRfit == 1]
lines(trans3d(x = mmt, y = 0:21, z = pred.l$matRRfit[as.character(mmt), ],
  pmat = d3), lwd = 2, col = "red")

################################################################################
### WEB FIGURE 1b

plot(pred.l, "overall", xlab = "Temperature (?C)", ylab = "RR", cex.axis = 0.9,
  ylim = c(0.5, max(pred.l$allRRhigh)),
  main = "Overall cumulative exposure-response")
mtext("London 1993-2006", cex=0.8)

#
