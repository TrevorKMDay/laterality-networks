path <- "/Research/laterality/laterality-wta"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(rstatix)
library(data.table)

nets_lut <- tibble(
    net      = 1:16,
    network  = paste0("V", 1:16),
    net_name = c("DMN", "Vis", "FPN", NA, "DAN", NA, "VAN", "Sal", "CO", "SMd",
                 "SMI", "AUD", "TP", "MTL", "PMN", "PON")
  )

nihtbx <- read_rds("../abcd-laterality/local_code/nih_nihtb_bl.rds")

###############################################################################

# Binary LI

# Read in data
pnet <- read_csv("code_processing/binary-networks/results_li.csv") %>%
  rename(
    net = ix,
    LI  = li
  ) %>%
  select(sub, net, LI) %>%
  filter(
    !(net %in% c(4, 6))
  ) %>%
  left_join(nets_lut) %>%
  mutate(
    # Net laterality should never be exactly 1, indicates some problem
    LI = replace(LI, abs(LI) == 1, NA)
  ) %>%
  filter(
    !is.na(LI)
  ) %>%
  mutate(
    sub = str_remove(sub, "results_li/")
  )

saveRDS(pnet, "data/pnet.rds")

# Summarize by network
bLI_summary <- pnet %>%
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
bLI_summary$p.cor <- p.adjust(bLI_summary$p, method = "BH")

dir.create("plots", showWarnings = FALSE)

png("plots/pnet_bLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(pnet, aes(x = net_name, y = LI, fill = net_name)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE, notchwidth = 0,
               outlier.alpha = 0.5) +
  geom_hline(yintercept = c(-0.2, 0, 0.2), color = "red") +
  geom_text(data = bLI_summary, y = -1, aes(label = n_lt2), size = 4) +
  geom_text(data = bLI_summary, y =  1, aes(label = n_gt2), size = 4) +
  geom_text(data = bLI_summary, y =  1.1, aes(label = e_size), size = 5,
            color = "red") +
  scale_y_continuous(limits = c(-1.1, 1.2)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Binary LI",
       caption = paste0("n=", length(unique(pnet$sub))))

dev.off()

###############################################################################

# Continuous LI

cLI_rds <-  "data/cLI_ARM1.rds"

if (!file.exists(cLI_rds)) {

  # Get list of files and read in from RDS (will take a minute)
  cLI_list <- list.files("data/mats-ARMS1/", full.names = TRUE) %>%
    lapply(readRDS)

  # Separate out processing from loading when loading is burdensome
  cLI <- rbindlist(cLI_list) %>%
    mutate(
      sub = rep(list.files("data/mats-ARMS1/"), each = 16) %>%
        str_remove(".rds")
    )

  saveRDS(cLI, cLI_rds)

} else {
  cLI <- readRDS(cLI_rds)
}

cLI <- cLI %>%
  filter(
    sub %in% pnet$sub,
    !(net %in% c(4, 6))
  )

cLI_effsize <- cLI %>%
  select(-ttest) %>%
  group_by(net_name) %>%
  nest() %>%
  mutate(
    m_LI = map_dbl(data, ~mean(.x$diff)),
    sd_LI = map_dbl(data, ~sd(.x$diff)),
    d = m_LI / sd_LI
  ) %>%
  arrange(desc(abs(d)))

png("plots/pnet_cLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(cLI, aes(x = net_name, y = diff, fill = net_name)) +
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

t_test <- cLI %>%
  select(sub, net_name, ttest) %>%
  mutate(
    t = map_dbl(ttest, ~.x$statistic),
    n = 29696,
    d = t / sqrt(n)
  ) %>%
  left_join(nets_lut) %>%
  arrange(desc(abs(d)))

png("plots/pnet_tLI.png", width = 6.5, height = 4, units = "in", res = 300)

ggplot(t_test, aes(x = net_name, y = d, fill = net_name)) +
  geom_boxplot(notch = TRUE) +
  geom_hline(yintercept = c(-0.2, 0, 0.2), color = "red") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Network", y = "Cohen's d",
       caption = paste0("n=", length(unique(cLI$sub))))

dev.off()

################################################################################

hand <- read_csv("../abcd-laterality/local_code/handedness/handedness.csv") %>%
  mutate(
    sub = paste0("sub-", subjectkey)
  ) %>%
  select(sub, handedness)

# Correlations

li_cor <- pnet %>%
  select(sub, net_name, LI) %>%
  rename(bLI = LI) %>%
  left_join(
    select(cLI, sub, net_name, diff),
    by = c("sub", "net_name")
  ) %>%
  rename(cLI = diff) %>%
  left_join(
    select(t_test, sub, net_name, d),
    by = c("sub", "net_name")
  ) %>%
  rename(tLI = d) %>%
  na.omit() %>%
  left_join(hand)

li_cor_nested <- li_cor %>%
  group_by(sub) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )

van <- li_cor %>%
  filter(
    net_name == "VAN"
  ) %>%
  arrange(bLI)

ggplot(li_cor, aes(x = bLI, y = cLI)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(net_name)) +
  labs(title = "bLI v. cLI") +
  theme_bw()

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
  group_by(net_name) %>%
  na.omit() %>%
  summarize(
    n   = n(),
    b_c = cor(bLI, cLI),
    b_t = cor(bLI, tLI),
    c_t = cor(cLI, tLI)
  ) %>%
  left_join(bLI_summary_short) %>%
  select(net, net_name, effect, everything()) %>%
  arrange(desc(abs(effect)))


# bc_t = bc * sqrt(nrow(.) / 2) / sqrt(1 - bc^2),
# bc_p = 1 - pt(bc_t, 2),

# Calculated t-stat for each r value within comparison
n_obs <- unique(li_cor_mat$n)
li_cor_mat_t <- li_cor_mat %>%
  select(matches("^._.$")) %>%
  mutate(
    across(everything(), ~(.x * sqrt(n_obs / 2) / sqrt(1 - .x^2)))
  )

li_cor_mat_p <- li_cor_mat_t %>%
  mutate(
    across(everything(), ~(1 - pt(.x, 2)))
  )

li_cor_mat_pcor <- li_cor_mat_p * prod(dim(li_cor_mat_p))

###############################################################################

li_nih <- li_cor %>%
  mutate(
    Identifiers = str_remove(sub, "sub-")
  ) %>%
  left_join(nihtbx) %>%
  select(Identifiers, net_name, bLI, cLI, starts_with("nihtbx"))

li_nih_bLI_cor <- li_nih %>%
  pivot_longer(c(bLI, cLI), values_to = "LI") %>%
  select(-Identifiers) %>%
  group_by(net_name, name) %>%
  nest() %>%
  mutate(
    cor = map(data, cor, use = "complete.obs"),
    LI_cor = map(cor, ~.x[11, ])
  )

li_corrplot <- li_nih_bLI_cor %>%
  select(net_name, name, cor) %>%
  mutate(
    cor = map(cor, as_tibble)
  ) %>%
  unnest(cor) %>%
  filter(
    row_number() %% 11 == 0
  ) %>%
  select(-LI) %>%
  pivot_longer(starts_with("nihtbx"), names_to = "nihtbx") %>%
  mutate(
    full_name = paste0(net_name, "_", name),
    nihtbx = str_remove(nihtbx, "nihtbx_"),
    r = value,
    t = r / sqrt((1 - r^2) / (2442 - 2)),
    p = 2 * pt(-abs(t), 2442 - 2),
    sig = p < (.05 / 28)
  )

ggplot(li_corrplot, aes(x = nihtbx, y = net_name, fill = value)) +
  geom_tile() +
  geom_text(aes(label = if_else(sig, "*", NA_character_)), color = "white") +
  scale_fill_gradient2(limits = c(-0.12, 0.12)) +
  facet_grid(cols = vars(name)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
