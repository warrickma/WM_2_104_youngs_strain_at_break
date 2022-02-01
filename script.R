install.packages("data.table")
library(ggplot2)
library(scales)
library(svglite)
library(data.table)
data = read.csv("data.csv")
young_aov = aov(Young~Sample, data)
strainatbreak_aov = aov(Strain_at_break~Sample, data)
uts_aov = aov(UTS~Sample, data)
summary(young_aov)
summary(strainatbreak_aov)
summary(uts_aov)
TuckeyHSD(young_aov)
setDT(data)
str(data)
dataMelt = melt(data, id.vars = "Sample")
dataMelt_agg = dataMelt[, .(mean = mean(value), se = sd(value)/.N), by = .(Sample, variable)]
data_young = dataMelt_agg[(1:3),]
data_strain = dataMelt_agg[(4:6),]
data_uts = dataMelt_agg[(7:9),]
ggplot(data_young, aes(x = Sample, y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + 1.96*se, 
                    ymin = mean - 1.96*se,
                    width = 0.2),
                position = position_dodge(0.9)) +
  labs(y = "Young's Modulus (MPa)") +
  scale_x_discrete(labels=c("25 wt% PPS", "30 wt% PPS", "37 wt% PPS")) +
  scale_y_continuous(limits = c(0,12), n.breaks = 5) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=10, color = "black", face = "bold"),
    axis.text.y = element_text(size=10, color = "black", face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10, color = "black", face = "bold"),
    legend.title = element_text(size=10, color = "black", face = "bold"),
    legend.text = element_text(size=10, color = "black", face = "bold"),
    legend.position = "none")
ggsave("young.svg", width = 80, height = 75, units = "mm")

ggplot(data_strain, aes(x = Sample, y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + 1.96*se, 
                    ymin = mean - 1.96*se,
                    width = 0.2),
                position = position_dodge(0.9)) +
  labs(y = "Strain at Break (mm/mm)") +
  scale_x_discrete(labels=c("25 wt% PPS", "30 wt% PPS", "37 wt% PPS")) +
  scale_y_continuous(limits = c(0,0.70), n.breaks = 10, labels = scales::percent) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=10, color = "black", face = "bold"),
    axis.text.y = element_text(size=10, color = "black", face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10, color = "black", face = "bold"),
    legend.title = element_text(size=10, color = "black", face = "bold"),
    legend.text = element_text(size=10, color = "black", face = "bold"),
    legend.position = "none")
ggsave("strain.svg", width = 80, height = 75, units = "mm")

ggplot(data_uts, aes(x = Sample, y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + 1.96*se, 
                    ymin = mean - 1.96*se,
                    width = 0.2),
                position = position_dodge(0.9)) +
  labs(y = "Ultimate Tensile Stress (MPa)") +
  scale_x_discrete(labels=c("25 wt% PPS", "30 wt% PPS", "37 wt% PPS")) +
  scale_y_continuous(limits = c(0,3.5), n.breaks = 5) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size=10, color = "black", face = "bold"),
    axis.text.y = element_text(size=10, color = "black", face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10, color = "black", face = "bold"),
    legend.title = element_text(size=10, color = "black", face = "bold"),
    legend.text = element_text(size=10, color = "black", face = "bold"),
    legend.position = "none")
ggsave("uts.svg", width = 80, height = 75, units = "mm")
