
#####figures for choosing ID###
table22_90<- table22_trimmed %>%
  group_by(doy, ID) %>%
  summarize(
    Percentile90_GRVI = quantile(GRVI, probs = 0.9, na.rm = TRUE),
    Percentile90_GCC = quantile(GCC, probs = 0.9, na.rm = TRUE),
    Percentile90_N_GRVI = quantile(normalized_grvi, probs = 0.9, na.rm = TRUE),
    Percentile90_N_GCC = quantile(normalized_gcc, probs = 0.9, na.rm = TRUE),
  )




ggplot(table22_90, aes(x = doy, y = Percentile90_N_GCC)) +
  geom_point(color="green4") +
  labs(title = "90precent quantile GCC data",
       x = "DOY",
       y = "GCC") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12))+
  facet_wrap(~ID,scales = "free_x")
#ylim(0.32, 0.36)


table23_90<- table23_larg %>%
  group_by(doy, ID) %>%
  summarize(
    Percentile90_GRVI = quantile(GRVI, probs = 0.9, na.rm = TRUE),
    Percentile90_GCC = quantile(GCC, probs = 0.9, na.rm = TRUE),
    Percentile90_N_GRVI = quantile(normalized_grvi, probs = 0.9, na.rm = TRUE),
    Percentile90_N_GCC = quantile(normalized_gcc, probs = 0.9, na.rm = TRUE),
  )

ggplot(table23_90, aes(x = doy, y = Percentile90_GCC)) +
  geom_point(color="green4") +
  labs(title = "90precent quantile GCC data",
       x = "DOY",
       y = "GCC") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12))+
  facet_wrap(~ID,scales = "free_x")
#ylim(0.32, 0.36)