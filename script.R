install.packages("data.table")
library(ggplot2)
library(scales)
library(svglite)
library(data.table)
data = read.csv("data.csv")
setDT(data)
str(data)
dataMelt = melt(data, id.vars = "Sample")
dataMelt_agg = dataMelt[, .(mean = mean(value), se = sd(value)/.N), by = .(Sample, variable)]
data_young = dataMelt_agg[(1:2),]
data_strain = dataMelt_agg[(3:4),]
data
ggplot(data_young, aes(x = Sample, y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + 1.96*se, 
                    ymin = mean - 1.96*se,
                    width = 0.2),
                position = position_dodge(0.9)) +
  labs(y = "Young's Modulus (MPa)") +
  scale_x_discrete(labels=c("Low PPS", "High PPS")) +
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
  scale_x_discrete(labels=c("Low PPS", "High PPS")) +
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
