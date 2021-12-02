setwd("G:/My Drive/Research/laterality/laterality-wta/")

library(tidyverse)
library(rstatix)
library(data.table)
library(Hmisc)
library(corrplot)

nets_lut <- tibble(
    net      = 1:16,
    network  = paste0("V", 1:16),
    net_name = c("DMN", "Vis", "FPN", NA, "DAN", NA, "VAN", "Sal", "CO", "SMd",
                 "SMI", "AUD", "TP", "MTL", "PMN", "PON")
  )

###############################################################################

# Binary LI

# Read in data
wta <- read_csv("data/wta-results.csv",
                col_names = c("sub", "net", "dice1", "dice1.ol", "dice1.x",
                              "dice1.y", "dice2", "dice2.ol", "dice2.x",
                              "dice2.y", "orig_n", "flip_n", "LI")) %>%
  select(sub, net, LI) %>%
  filter(
    !(net %in% c(4, 6))
  ) %>%
  left_join(nets_lut)

# Summarize by network
bLI_summary <- wta %>%
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

# BH correction needs to be done outside of mutate
bli_summary$p.cor <- p.adjust(bli_summary$p, method = "BH")

dir.create("plots", showWarnings = FALSE)

png("plots/wta_bLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(wta, aes(x = net_name, y = LI, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = c(-0.2, 0, 0.2), color = "red") +
  geom_text(data = bLI_summary, y = -1, aes(label = n_lt2), size = 4) +
  geom_text(data = bLI_summary, y =  1, aes(label = n_gt2), size = 4) +
  geom_text(data = bLI_summary, y =  1.1, aes(label = e_size), size = 5,
            color = "red") +
  scale_y_continuous(limits = c(-1.1, 1.2), sec.axis = dup_axis()) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Binary LI",
       caption = paste0("n=", length(unique(wta$sub))))

dev.off()

###############################################################################

# Continuous LI

cLI_rds <-  "data/cLI.rds"

if (!file.exists(cLI_rds)) {

  # Get list of files and read in from RDS (will take a minute)
  cLI_list <- list.files("data/mats-2633/", full.names = TRUE) %>%
    lapply(readRDS)

  # Separate out processing from loading when loading is burdensome
  cLI <- rbindlist(cLI_list) %>%
    mutate(
      sub = rep(list.files("data/mats-2633/"), each = 14) %>%
        str_remove(".rds")
    )

  saveRDS(cLI, cLI_rds)

} else {
  cLI <- readRDS(cLI_rds)
}

png("plots/wta_cLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(cLI, aes(x = net_name, y = mean_diff, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(limits = c(-1, 1) * 0.1, sec.axis = dup_axis()) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Continuous LI",
       caption = paste0("n=", length(unique(cLI$sub))))

dev.off()

################################################################################

# t-test

t_test <- cLI %>%
  select(sub, net_name, model) %>%
  mutate(
    t = map_dbl(model, ~.x$statistic),
    n = 26706,
    d = t / sqrt(n)
  ) %>%
  left_join(nets_lut)

png("plots/wta_tLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(t_test, aes(x = net_name, y = d, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Cohen's d",
       caption = paste0("n=", length(unique(cLI$sub))))

dev.off()

################################################################################

wta_wide <- wta %>%
  select(sub, net_name, LI) %>%
  pivot_wider(names_from = net_name, values_from = LI) %>%
  select(-sub) %>%
  as.matrix() %>%
  rcorr()

n_comparisons <- 14 ^ 2 - 14
p_thresh <- 0.05 / n_comparisons

png("plots/wta_corrplot.png", width = 5, height = 5, units = "in", res = 300)

corrplot(wta_wide$r, method = "color",
         order = "hclust", hclust.method = "complete",
         p.mat = wta_wide$P, sig.level = p_thresh)

dev.off()

# Correlations

li_cor <- wta %>%
  select(sub, net_name, LI) %>%
  rename(bLI = LI) %>%
  left_join(
    select(cLI, sub, net_name, mean_diff)
  ) %>%
  rename(cLI = mean_diff) %>%
  left_join(
    select(t_test, sub, net_name, d)
  ) %>%
  rename(tLI = d) %>%
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

bLI_summary_short <- bLI_summary %>%
  select(net_name, effect)

li_cor_mat <- li_cor %>%
  na.omit() %>%
  group_by(net_name) %>%
  nest() %>%
  mutate(

    n = map_dbl(data, nrow),

    bLI_mean = map_dbl(data, ~mean(.x$bLI)),
    cLI_mean = map_dbl(data, ~mean(.x$cLI)),
    tLI_mean = map_dbl(data, ~mean(.x$tLI)),

    bLI_sd = map_dbl(data, ~sd(.x$bLI)),
    cLI_sd = map_dbl(data, ~sd(.x$cLI)),
    tLI_sd = map_dbl(data, ~sd(.x$tLI)),

    bLI_d = bLI_mean / bLI_sd,
    cLI_d = cLI_mean / cLI_sd,
    tLI_d = tLI_mean / tLI_sd,

    bc = map_dbl(data, ~cor(.x$bLI, .x$cLI)),
    bt = map_dbl(data, ~cor(.x$bLI, .x$tLI)),
    ct = map_dbl(data, ~cor(.x$cLI, .x$tLI))

  ) %>%
  select(net_name, n, ends_with("_d"), matches("^..$")) %>%
  arrange(desc(abs(bLI_d)))


  # dplyr::summarize(
  #   n   = n(),
  #   b_c = cor(bLI, cLI),
  #   b_t = cor(bLI, tLI),
  #   c_t = cor(cLI, tLI)
  # ) %>%
  # left_join(bLI_summary_short) %>%
  # select(net, net_name, effect, everything()) %>%
  # arrange(desc(abs(effect)))

write.csv(li_cor_mat, "clipboard")


# bc_t = bc * sqrt(nrow(.) / 2) / sqrt(1 - bc^2),
# bc_p = 1 - pt(bc_t, 2),

# Calculated t-stat for each r value within comparison
n_obs <- unique(li_cor_mat$n)
li_cor_mat_t <- li_cor_mat %>%
  ungroup() %>%
  select(matches("^..$")) %>%
  mutate(
    across(everything(), ~(.x * sqrt(n_obs / 2) / sqrt(1 - .x^2)))
  )

li_cor_mat_p <- li_cor_mat_t %>%
  mutate(
    across(everything(), ~(1 - pt(.x, 2)))
  )

li_cor_mat_pcor <- li_cor_mat_p * prod(dim(li_cor_mat_p))
