options(stringsAsFactors = FALSE)
parmar = par("mar")
mfrow  = par("mfrow")
# install and load packages
libraries = c("locfit", "KernSmooth")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# Set Input and Output directories
path.input  = ""
path.output = ""
setwd(path.input)
# Read in Data 
sim.df = read.csv2("TXTSimulationEntire.csv")
sim.df = sim.df[1:9]
setwd(path.output)
# Limits of y-axis
min_y = min(sim.df$BL_SimulatedVolatility, sim.df$LM_SimulatedVolatility,
            sim.df$MPQA_SimulatedVolatility)
max_y = max(sim.df$BL_SimulatedVolatility, sim.df$LM_SimulatedVolatility,
            sim.df$MPQA_SimulatedVolatility)
#### Plot axes and labels only and save it as pdf ####
aplot = function(x, y, lex, pn){
     # Select Binwidth
     bw = dpill(x, y)
     ylab = paste(lex, "Simulated Volatility", sep = "")
     xlab = paste(lex, pn, "Proportion, h =", round(bw, 4), sep = " ")
     # Compute local regression line and bands 
     llr   = locfit(y ~ lp(x, h = bw, deg = 1), kern = "gauss", deg = 1)
     bands = scb(y ~ lp(x, h = bw, deg = 1), type = 1, ev = lfgrid(100),
                 simul = TRUE, kern = "gauss", deg = 1)
     plot(NULL, pch = 20, cex = 0.5, col="#00000010",
          ylab = ylab, xlab = xlab,
          cex.axis = 0.9, tck = - 0.03, mgp = c(1.5, 0.3, 0), cex.lab = 1,
          ylim = c(min_y, max_y), xlim = range(x))
     points(x, y, pch = '.', col = "#00000020")
     lines(llr, col = "blue", lwd = 2)
     lines(bands$x, bands$lower, col = "red", lwd = 2, lty = "dashed")
     lines(bands$x, bands$upper, col = "red", lwd = 2, lty = "dashed")
}
par(mar=c(3.0,3.1,1,1.7))
par(mfrow=c(2,3))
# Negative Proportion Plots
pn   = "Negative"
# BL Neg
x    = sim.df$BL_Neg_Proportion
y    = sim.df$BL_SimulatedVolatility
lex  = "BL"
aplot(x, y, lex, pn)
# LM Neg
x    = sim.df$LM_Neg_Proportion
y    = sim.df$LM_SimulatedVolatility
lex  = "LM"
aplot(x, y, lex, pn)
# MPQA Neg
x    = sim.df$MPQA_Neg_Proportion
y    = sim.df$MPQA_SimulatedVolatility
lex  = "MPQA"
aplot(x, y, lex, pn)
# Positive Proportion Plots
pn   = "Positive"
# BL Pos
x    = sim.df$BL_Pos_Proportion
y    = sim.df$BL_SimulatedVolatility
lex  = "Pos"
aplot(x, y, lex, pn)
# LM Pos
x    = sim.df$LM_Pos_Proportion
y    = sim.df$LM_SimulatedVolatility
lex  = "LM"
aplot(x, y, lex, pn)
# MPQA Neg
x    = sim.df$MPQA_Pos_Proportion
y    = sim.df$MPQA_SimulatedVolatility
lex  = "MPQA"
aplot(x, y, lex, pn)
# Return to previous margins
par(mar =  parmar)