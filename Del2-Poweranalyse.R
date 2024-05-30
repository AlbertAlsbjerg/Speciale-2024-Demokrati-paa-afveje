#----------------------------------------------------------
# POWERANALYSE: CONJOINTEKSPERIMENT
#----------------------------------------------------------

# Beregning af antal respondenter med en alpha på 0.05, power på 90% og 3 
# niveauer for vores conjoint-attributer (AMCE estimation)
pa1 <- cjpowr_amce(amce = 0.05, power = 0.9, levels = 3)
pa1$n/(2*5) # 629 respondenter

# liste over variable til data frame over poweranalyse (fra cjpowR)
pa2_l <- expand.grid(
  # AMCE niveauer for grafer
  amce = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06), 
  # effektive antal respondenter
  n = seq(from = 100, to = 40000, length.out = 1000), 
  # ønskede alpha-niveau og niveauer for conjointattributer
  alpha = 0.05, 
  levels = 3,
  treat.prob = 0.5,
  sims = 0 # set to 0 if you want to plot something else than Type M error
)

# beregn data frame med cjpowR for AMCE
pa2 <- list2DF(do.call(cjpowr_amce, pa2_l))

# Tegn plot
ggplot(pa2[pa2$amce>.01,], aes(x = n, y = power, group = factor(amce), 
                               colour = factor(amce), linetype = factor(amce))) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c(c_lightblue, c_grey, c_blue, c_purple, c_red)) +
  #geom_point(aes(x=11500, y=.8), colour = c_black, size = 2) +
  theme_bw() +
  labs(y = "Power", x = "Effektive antal respondenter*",
       colour = "AMCE", linetype = "AMCE")  +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black"),
    # tilføj grid-linjer på x-aksen
    #panel.grid.major.x = element_line(size=.1, color="grey"),
    # add margins around the plot
    #plot.margin = margin(15,15,15,15),
    
  ) 

# eksporter kort som .png
ggsave("poweranalyse_kurve.pdf", device = "pdf", width = 7, height = 4)

