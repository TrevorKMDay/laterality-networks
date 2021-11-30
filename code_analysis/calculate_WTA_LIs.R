setwd("G:/My Drive/Research/laterality/laterality-wta/")

library(tidyverse)
library(rstatix)
library(data.table)

nets_lut <- tibble(
    net      = 1:16,
    network  = paste0("V", 1:16),
    net_name = c("DMN", "Vis", "FPN", NA, "DAN", NA, "VAN", "Sal", "CO", "SMd",
                 "SMI", "AUD", "TP", "MTL", "PMN", "PON")
  )

###############################################################################

# Binary LI

wta <- read_csv("../WTA-networks/wta-results.csv",
                col_names = c("sub", "net", "dice1", "dice1.ol", "dice1.x",
                              "dice1.y", "dice2", "dice2.ol", "dice2.x",
                              "dice2.y", "orig_n", "flip_n", "LI")) %>%
  select(sub, net, LI) %>%
  filter(
    !(net %in% c(4, 6))
  ) %>%
  left_join(nets_lut)


bli_summary <- wta %>%
  group_by(net, net_name) %>%
  nest() %>%
  mutate(
    n      = map_int(data, nrow),
    n_gt2  = map_int(data, ~sum(.x$LI > 0.2)),
    n_lt2  = map_int(data, ~sum(.x$LI < -0.2)),
    t_test = map(data, ~t.test(.x$LI)),
    t      = map_dbl(t_test, ~.x$statistic),
    p      = map_dbl(t_test, ~.x$p.value),
    m_LI   = map_dbl(data, ~mean(.x$LI)),
    sd_LI  = map_dbl(data, ~sd(.x$LI)),
    effect = m_LI / sd_LI,
    e_size = case_when(abs(effect) > 0.8 ~ "***",
                       abs(effect) > 0.5 ~ "**",
                       abs(effect) > 0.2 ~ "*",
                       TRUE ~ NA_character_)
  ) %>%
  arrange(desc(abs(effect)))

bli_summary$p.cor <- p.adjust(bli_summary$p, method = "BH")

png("wta_bLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(wta, aes(x = net_name, y = LI, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = c(-0.2, 0, 0.2), color = "red") +
  geom_text(data = bli_summary, y = -1, aes(label = n_lt2), size = 4) +
  geom_text(data = bli_summary, y =  1, aes(label = n_gt2), size = 4) +
  geom_text(data = bli_summary, y =  1.1, aes(label = e_size), size = 5,
            color = "red") +
  scale_y_continuous(limits = c(-1.1, 1.2)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Binary LI",
       caption = paste0("n=", length(unique(wta$sub))))

dev.off()

###############################################################################

# Continuous LI

cLI_list <- list.files("../mats-2633-subrds/", full.names = TRUE) %>%
  lapply(readRDS)

cLI <- rbindlist(cLI_list) %>%
  mutate(
    sub = rep(list.files("../mats-2633-subrds/"), each = 14) %>%
      str_remove(".rds")
  )

png("wta_cLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(cLI, aes(x = net_name, y = mean_diff, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(limits = c(-1, 1) * 0.1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Continuous LI",
       caption = paste0("n=", length(unique(cLI$sub))))

dev.off()

################################################################################

# t-test

t_test2 <- cLI %>%
  select(sub, net_name, model) %>%
  mutate(
    t = map_dbl(model, ~.x$statistic),
    n = 26706,
    d = t / sqrt(n)
  ) %>%
  left_join(nets_lut)

ggplot(t_test2, aes(x = net_name, y = d, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Cohen's d",
       caption = paste0("n=", length(unique(cLI$sub))))

t_test3 <- t_test2 %>%
  ungroup() %>%
  select(sub, net_name, d)

################################################################################

# Correlations

li_cor <- wta %>%
  select(sub, net_name, LI) %>%
  rename(bLI = LI) %>%
  left_join(cLI) %>%
  rename(cLI = mean_diff) %>%
  left_join(t_test3) %>%
  rename(tLI = d) %>%
  select(-model) %>%
  na.omit()

ggplot(li_cor, aes(x = bLI, y = cLI)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(net_name)) +
  labs(title = "bLI v. cLI")

ggplot(li_cor, aes(x = bLI, y = tLI)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(net_name)) +
  labs(title = "bLI v. tLI")

ggplot(li_cor, aes(x = cLI, y = tLI)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(net_name)) +
  labs(title = "cLI v. tLI")

bli_summary_short <- bli_summary %>%
  select(net_name, effect)

li_cor_mat <- li_cor %>%
  group_by(net_name) %>%
  na.omit() %>%
  summarize(
    n    = n(),
    bc   = cor(bLI, cLI),
    bc_t = bc * sqrt(nrow(.) / 2) / sqrt(1 - bc^2),
    bc_p = 1 - pt(bc_t, 2),
    bt   = cor(bLI, tLI),
    ct   = cor(cLI, tLI)
  ) %>%
  left_join(bli_summary_short) %>%
  select(net_name, effect, everything()) %>%
  arrange(desc(abs(effect)))

li_cor_mat_long <- li_cor_mat %>%
  select(net_name, effect, bc, bt, ct) %>%
  pivot_longer(-c(net_name, effect))

ggplot(li_cor_mat_long, aes(x = abs(effect), y = value, color = name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line()
