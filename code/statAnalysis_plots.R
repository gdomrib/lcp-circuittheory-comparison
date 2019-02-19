library(ggplot2)
library(grid)
library(readr)
library(gridExtra)



statsLBA <- read_delim("statsLBA.csv", ";", escape_double = FALSE, trim_ws = TRUE)
statsEIA <- read_delim("statsEIA.csv", ";", escape_double = FALSE, trim_ws = TRUE)
statsLBA_left <- read_delim("statsLBA_left.csv", ";", escape_double = FALSE, trim_ws = TRUE)
statsLBA_right <- read_delim("statsLBA_right.csv", ";", escape_double = FALSE, trim_ws = TRUE)
statsEIA_left <- read_delim("statsEIA_left.csv", ";", escape_double = FALSE, trim_ws = TRUE)
statsEIA_right <- read_delim("statsEIA_right.csv", ";", escape_double = FALSE, trim_ws = TRUE)

zonalStats <- rbind(statsLBA, statsEIA)
zonalStats$orderedPeriod <- factor(zonalStats$period, levels=c("LBA", "EIA"))

zonalStats_left <- rbind(statsLBA_left, statsEIA_left)
zonalStats_left$orderedPeriod <- factor(zonalStats_left$period, levels=c("LBA", "EIA"))

zonalStats_right <- rbind(statsLBA_right, statsEIA_right)
zonalStats_right$orderedPeriod <- factor(zonalStats_right$period, levels=c("LBA", "EIA"))

"

Distance to natural routes

"

# EXPLORATORY DATA ANALYSIS
distMeanLBA <- mean(statsLBA$LCP_dist)
distMeanEIA <- mean(statsEIA$LCP_dist)

ggplot(statsLBA, aes(LCP_dist)) +  
  geom_density(color=FALSE, fill="#fdae61", alpha=0.7) +  
  geom_vline(aes(xintercept=distMeanLBA), linetype="dotted", color="#636363", size = 1) +  
  labs(title="LBA - Distance to natural routes", x="distance (m)")


ggplot(statsEIA, aes(LCP_dist)) +  
  geom_density(color=FALSE, fill="#66c2a5", alpha=0.7) +  
  geom_vline(aes(xintercept=distMeanEIA), linetype="dotted", color="#636363", size = 1) +  
  labs(title="EIA - Distance to natural routes", x="distance (m)")


ggplot(zonalStats, aes(LCP_dist, fill=period)) +  
  geom_density(alpha=0.6, aes(color=period)) +  
  scale_color_manual(values=c("#66c2a5","#fdae61")) +  
  scale_fill_manual(values=c("#66c2a5","#fdae61")) +  
  labs(title="Distance to natural routes", x="distance (m)", fill="Period") +  
  theme_minimal() +  
  guides(color=FALSE)


# Density by Bank
distNR <- ggplot(zonalStats, aes(LCP_dist, fill=orderedPeriod)) +  
  geom_density(data=zonalStats, alpha=0.8, color="#636363") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  facet_grid(bank~orderedPeriod) +  
  xlab("distance (m)") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 30), legend.position = "none")


jpeg('plots/density_LCP_dist.jpeg', width=8, height=6, units = 'in', res = 300)
distNR
dev.off()


# BOXPLOT 
box_distNR <- ggplot(zonalStats, aes(orderedPeriod, LCP_dist, fill=orderedPeriod)) +  
  geom_boxplot(color="#636363") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  facet_wrap(~bank) + 
  labs(x="period", y="distance (m)") +  
  theme_minimal() +  
  theme(legend.position = "none")

jpeg('plots/boxplot_LCP_dist.jpeg', width=8, height=6, units = 'in', res = 300)
box_distNR
dev.off()

ks.test(statsLBA_left$LCP_dist, statsLBA_right$LCP_dist)
ks.test(statsLBA_left$LCP_dist, statsEIA_left$LCP_dist)
ks.test(statsLBA_left$LCP_dist, statsEIA_right$LCP_dist)
ks.test(statsLBA_right$LCP_dist, statsEIA_left$LCP_dist)
ks.test(statsLBA_right$LCP_dist, statsEIA_right$LCP_dist)
ks.test(statsEIA_left$LCP_dist, statsEIA_right$LCP_dist)

mean(statsLBA_left$LCP_dist)
mean(statsLBA_right$LCP_dist)
mean(statsEIA_left$LCP_dist)
mean(statsEIA_right$LCP_dist)

"

Circuit Theory

"

LBA_Buffer <- read_csv("LBA_6km_Buffer.csv")
EIA_Buffer <- read_csv("EIA_6km_Buffer.csv")

LBA_Buffer$LCP_dist <- statsLBA$LCP_dist
EIA_Buffer$LCP_dist <- statsEIA$LCP_dist

buffers <- rbind(LBA_Buffer, EIA_Buffer)

buffers$orderedPeriod <- factor(buffers$period, levels=c("LBA", "EIA"))


bf_left <- subset(buffers, bank=='left')
bf_right <- subset(buffers, bank=='right')

bf_lbaLeft <- subset(buffers, period=="LBA" & bank=='left')
bf_lbaRight <- subset(buffers, period=="LBA" & bank=='right')
bf_eiaLeft <- subset(buffers, period=="EIA" & bank=='left')
bf_eiaRight <- subset(buffers, period=="EIA" & bank=='right')




# EXPLORATORY DATA ANALYSIS
currentMeanLBA <- mean(LBA_Buffer$CS_6km_mea)
currentMeanEIA <- mean(statsEIA$CS_6km_mea)

ggplot(LBA_Buffer, aes(CS_6km_mea)) +  
  geom_density(color=FALSE, fill="#fdae61", alpha=0.7) +  
  geom_vline(aes(xintercept=currentMeanLBA), linetype="dotted", color="#636363", size = 1) +  
  labs(title="LBA Current Flow", x="mean")


ggplot(EIA_Buffer, aes(CS_6km_mea)) +  
  geom_density(color=FALSE, fill="#66c2a5", alpha=0.7) +  
  geom_vline(aes(xintercept=currentMeanEIA), linetype="dotted", color="#636363", size = 1) +  
  labs(title="EIA Current Flow", x="current mean")


ggplot(buffers, aes(CS_6km_mea, fill=period)) +  
  geom_density(alpha=0.6, aes(color=period)) +  
  scale_color_manual(values=c("#66c2a5","#fdae61")) +  
  scale_fill_manual(values=c("#66c2a5","#fdae61")) +  
  labs(title="Current Flow", x="current (mean)", fill="Period") +  
  theme_minimal() +  
  guides(color=FALSE)

ggplot(bf_left, aes(CS_6km_mea, fill=orderedPeriod)) +  
  geom_density(alpha=0.7, col=FALSE) +  
  facet_wrap(~bank, ncol=1) +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  labs(title="Current Mean - Left Bank", x="current (mean)", fill="Period") +  
  theme_minimal()

ggplot(bf_right, aes(CS_6km_mea, fill=orderedPeriod)) +  
  geom_density(alpha=0.7, col=FALSE) +  
  facet_wrap(~bank, ncol=1) +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  labs(title="Current Mean - Right Bank", x="current (mean)", fill="Period") +  
  theme_minimal()


# DENSITY BY BANK
currentFlow <- ggplot(buffers, aes(CS_6km_mea, fill=orderedPeriod)) +  
  geom_density(alpha=0.8, color="#636363") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  facet_grid(bank~orderedPeriod) +  
  xlab("current flow (mean)") +  
  theme_minimal() +  
  theme(legend.position = "none")

jpeg('plots/density_current_mean_6km.jpeg', width=8, height=6, units = 'in', res = 300)
currentFlow
dev.off()


# BOXPLOT 
box_currentFlow <- ggplot(buffers, aes(orderedPeriod, CS_6km_mea, fill=orderedPeriod)) +  
  geom_boxplot(color="#636363") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  facet_grid(~bank) +  
  labs(x="period", y="current flow (mean)") +  
  theme_minimal() +  
  theme(legend.position = "none", plot.subtitle = element_text(face="italic"))

jpeg('plots/boxplot_currentFlow_mean.jpeg', width=8, height=6, units = 'in', res = 300)
box_currentFlow
dev.off()

ks.test(bf_lbaLeft$CS_6km_mea, bf_lbaRight$CS_6km_mea)
ks.test(bf_lbaLeft$CS_6km_mea, bf_eiaLeft$CS_6km_mea)
ks.test(bf_lbaLeft$CS_6km_mea, bf_eiaRight$CS_6km_mea)
ks.test(bf_lbaRight$CS_6km_mea, bf_eiaLeft$CS_6km_mea)
ks.test(bf_lbaRight$CS_6km_mea, bf_eiaRight$CS_6km_mea)
ks.test(bf_eiaLeft$CS_6km_mea, bf_eiaRight$CS_6km_mea)

mean(bf_lbaLeft$CS_6km_mea)
mean(bf_lbaRight$CS_6km_mea)
mean(bf_eiaLeft$CS_6km_mea)
mean(bf_eiaRight$CS_6km_mea)


"
Comparison LCP - Circuit Theory
"

p1 <- ggplot(bf_lbaLeft, aes(LCP_dist, CS_6km_mea)) +  
  stat_smooth(aes(LCP_dist, CS_6km_mea, color=orderedPeriod), method=loess) +  
  scale_color_manual(values=c("#fdae61")) +  
  geom_jitter(color="black") +  
  labs(subtitle="LBA left") +  
  theme_minimal() +  
  theme(plot.subtitle = element_text(hjust = 0.5, face="plain", size=9), legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_text(size=7), axis.title.y=element_blank(), axis.text.y=element_text(size=7))

p2 <- ggplot(bf_lbaRight, aes(LCP_dist, CS_6km_mea)) +  
  stat_smooth(aes(LCP_dist, CS_6km_mea, color=orderedPeriod), method=loess) +  
  scale_color_manual(values=c("#fdae61")) +  
  geom_jitter(color="black") +  
  labs(subtitle="LBA right") +  
  theme_minimal() +  
  theme(plot.subtitle = element_text(hjust = 0.5, face="plain", size=9), legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_text(size=7), axis.title.y=element_blank(), axis.text.y=element_text(size=7))

p3 <- ggplot(bf_eiaRight, aes(LCP_dist, CS_6km_mea)) +  
  stat_smooth(aes(LCP_dist, CS_6km_mea, color=orderedPeriod), method=loess) +  
  scale_color_manual(values=c("#66c2a5")) +  
  geom_jitter(color="black") +  
  labs(subtitle="EIA right") +  
  theme_minimal() +  
  theme(plot.subtitle = element_text(hjust = 0.5, face="plain", size=9), legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_text(size=7), axis.title.y=element_blank(), axis.text.y=element_text(size=7))

p4 <- ggplot(bf_eiaLeft, aes(LCP_dist, CS_6km_mea)) +  
  stat_smooth(aes(LCP_dist, CS_6km_mea, color=orderedPeriod), method=loess) +  
  scale_color_manual(values=c("#66c2a5")) +  
  geom_jitter(color="black") +  
  labs(subtitle="EIA left") +  
  theme_minimal() +  
  theme(plot.subtitle = element_text(hjust = 0.5, face="plain", size=9), legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_text(size=7), axis.title.y=element_blank(), axis.text.y=element_text(size=7))


jpeg('plots/comparison_CS_LCP_FacetWrap.jpeg', width=9, height=6, units = 'in', res = 500)
grid.arrange(p1, p4, p2, p3, left = textGrob("current flow (mean)", gp=gpar(fontsize=10), rot=90), bottom = textGrob("distance (m)", gp=gpar(fontsize=10)))
dev.off()



ggplot(bf_right, aes(LCP_dist, CS_6km_mea, color=orderedPeriod, shape=orderedPeriod)) +  
  geom_jitter(size=2) +  
  geom_density2d(data=bf_right, aes(LCP_dist, CS_6km_mea, color=orderedPeriod), alpha=0.5) +  
  scale_color_manual(values=c("#fdae61","#66c2a5")) +  
  theme_minimal() +  
  labs(title="Zonal Stats - Right", x="distance to natural routes (m)", y="current (mean)", color="Period") +  
  guides(shape=FALSE)

ggplot(bf_left, aes(LCP_dist, CS_6km_mea, color=orderedPeriod, shape=orderedPeriod)) +  
  geom_jitter(size=2) +  
  geom_density2d(data=bf_left, aes(LCP_dist, CS_6km_mea, color=orderedPeriod), alpha=0.5) +  
  scale_color_manual(values=c("#fdae61","#66c2a5")) +  
  theme_minimal() +  
  labs(title="Zonal Stats - Left", x="distance to natural routes (m)", y="current (mean)", color="Period") +  
  guides(shape=FALSE)



# LBA - Current and Height
ggplot(statsLBA, aes(CS_mean,height, color=bank)) +  
  geom_jitter(size=2)

# LBA - Current and aspect
ggplot(statsLBA, aes(CS_mean,aspect, color=bank)) +  
  geom_jitter(size=1.5)

ggplot(statsLBA_left, aes(CS_mean,aspect, color=bank)) +  
  geom_jitter(size=1.5)
ggplot(statsLBA_right, aes(CS_mean,aspect, color=bank)) +  
  geom_jitter(size=1.5)



# LBA - Current and aspect
ggplot(statsEIA, aes(CS_mean,aspect, color=bank)) +  
  geom_jitter(size=1.5) +  
  geom_density2d(alpha=0.2)



"
DIFFERENCES PLOT
"

comparisonLCP_CS <- read_delim("Comparison_LCP-CS.csv", ";", escape_double = FALSE, trim_ws = TRUE)

comparisonLCP_CS$orderedPeriod <- factor(comparisonLCP_CS$period, levels=c("LBA", "EIA"))

comparison_lbaLeft <- subset(comparisonLCP_CS, period=="LBA" & bank=='left')
comparison_lbaRight <- subset(comparisonLCP_CS, period=="LBA" & bank=='right')
comparison_eiaLeft <- subset(comparisonLCP_CS, period=="EIA" & bank=='left')
comparison_eiaRight <- subset(comparisonLCP_CS, period=="EIA" & bank=='right')

comparison_LeftBank <- subset(comparisonLCP_CS, bank=='left')
comparison_RightBank <- subset(comparisonLCP_CS, bank=='right')

meanLBA_left <- mean(comparison_lbaLeft$Final_ranking)
meanLBA_right <- mean(comparison_lbaRight$Final_ranking)
meanEIA_left <- mean(comparison_eiaLeft$Final_ranking)
meanEIA_right <- mean(comparison_eiaRight$Final_ranking)


meanRanking <- data.frame(bank=c("left","right","left","right"), orderedPeriod=c("LBA","LBA","EIA","EIA"), Final_ranking=c(meanLBA_left, meanLBA_right, meanEIA_left, meanEIA_right))

ggplot(comparisonLCP_CS, aes(Final_ranking, fill=orderedPeriod)) +  
  geom_density(alpha=0.8, color="#636363") +  
  geom_rug(aes(Final_ranking, 0), position = position_jitter(height = 0), alpha=0.5,color="#636363") +  
  geom_vline(aes(xintercept=Final_ranking), meanRanking, linetype="dotted", color="black") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  theme_minimal() +  
  theme(legend.position = "none") +  
  xlab("difference ranking") +  
  facet_grid(bank~orderedPeriod, scales = "free")

ggplot(comparisonLCP_CS, aes(bank, Final_ranking, fill=orderedPeriod)) +  
  geom_boxplot(alpha=0.8, color="#636363") +  
  scale_fill_manual(values=c("#fdae61","#66c2a5")) +  
  theme_minimal() +  
  theme(legend.position = "none") +  
  scale_x_discrete(breaks = NULL) +  
  facet_grid(~orderedPeriod)
