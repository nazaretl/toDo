options(stringsAsFactors = FALSE)
parmar = par("mar")
mfrow  = par("mfrow")
# install and load packages
libraries = c("locfit", "KernSmooth")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# Functions
aplot = function(x, y, lex, pn, AttentionGroup = ""){
     # Select Binwidth
     bw = dpill(x, y)
     ylab = paste(lex, "Simulated Volatility", sep = " ")
     xlab = paste(lex, pn, "Proportion, h =", round(bw, 4), sep = " ")
     if(AttentionGroup != ""){
        main1 = paste(AttentionGroup, "Attention Group")
     }else{
        main1 = ""
     }
     bw   = dpill(x, y,)
     # Compute local regression line and bands
     llr   = locfit(y ~ lp(x, h = bw, deg = 1), kern = "gauss", deg = 1)
     bands = scb(y ~ lp(x, h = bw, deg = 1), type = 1, ev = lfgrid(100),
                 simul = TRUE, kern = "gauss", deg = 1)
     plot(NULL,
          pch = 20, cex = 0.5, col="#00000020",
          ylab = ylab, xlab = xlab,
          cex.axis = 0.9, tck = - 0.03, mgp = c(1.5, 0.3, 0), cex.lab = 0.9,
          ylim = c(min_y, max_y), xlim = range(x),
          main = main1, cex.main = 0.95)
     points(x, y, pch = '.', col = "#00000020")
     lines(llr, col = "blue", lwd = 2)
     lines(bands$x, bands$lower, col = "red", lwd = 2, lty = "dashed")
     lines(bands$x, bands$upper, col = "red", lwd = 2, lty = "dashed")
}
######
# Set Input directory
path.input  = ""
setwd(path.input)
# Load Data
sim.df = read.csv2("TXTSimulationAttention.csv")
sim.df = sim.df[1:10]
setwd(path.output)
### Set Attention Groups of interest
AttentionGroup = c("Low", "Extremely High")
# Calculate y-axis limits
  lim.df = sim.df[sim.df$Attention %in% AttentionGroup, ]
  min_y = min(lim.df$BL_SimulatedVolatility,
              lim.df$LM_SimulatedVolatility,
              lim.df$MPQA_SimulatedVolatility)
  max_y = max(lim.df$BL_SimulatedVolatility,
              lim.df$LM_SimulatedVolatility,
              lim.df$MPQA_SimulatedVolatility)
# Plot
par(mar=c(3.0,3.1,1.5,1.5))
par(mfrow=c(4,3))
for(i in 1:length(AttentionGroup)){
      sub_sim.df = sim.df[sim.df$Attention == AttentionGroup[i], ]
      # BL Negative Proportion
      x    = sub_sim.df$BL_Neg_Proportion
      y    = sub_sim.df$BL_SimulatedVolatility
      bw   = dpill(x, y)
      aplot(x, y, "BL", "Negative")
      # LM Negative Proportion
      x    = sub_sim.df$LM_Neg_Proportion
      y    = sub_sim.df$LM_SimulatedVolatility
      bw   = dpill(x, y,)
      aplot(x, y, "LM", "Negative", AttentionGroup = AttentionGroup[i])
      # MPQA Negative Proportion
      x    = sub_sim.df$MPQA_Neg_Proportion
      y    = sub_sim.df$MPQA_SimulatedVolatility
      bw   = dpill(x, y,)
      aplot(x, y, "MPQA", "Negative")
      # BL Positive Proportion
      x    = sub_sim.df$BL_Pos_Proportion
      y    = sub_sim.df$BL_SimulatedVolatility
      bw   = dpill(x, y,)
      aplot(x, y, "BL", "Positive")
      # LM Positive Proportion
      x    = sub_sim.df$LM_Pos_Proportion
      y    = sub_sim.df$LM_SimulatedVolatility
      bw   = dpill(x, y,)
      aplot(x, y, "LM", "Positive")
      # MPQA Positive Proportion
      x    = sub_sim.df$MPQA_Pos_Proportion
      y    = sub_sim.df$MPQA_SimulatedVolatility
      bw   = dpill(x, y,)
      aplot(x, y, "MPQA", "Positive")
}
####
par(mar =  parmar, mfrow = mfrow)