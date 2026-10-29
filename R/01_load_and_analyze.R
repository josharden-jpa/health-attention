# DATASETS USED (all from the U.S. Policy Agendas Project, comparativeagendas.net):
#   1. Democratic Party Platform
#   2. Republican Party Platform
#   3. Gallup's Most Important Problem
#   4. Budget Authority (Adjusted)
# Codebook: Jones, Baumgartner, Theriault, Epp, Khandekar & Little (2025)

library(tidyverse)

dem_plat  <- read_csv("data/dem_platform.csv")
rep_plat  <- read_csv("data/rep_platform.csv")
gallup    <- read_csv("data/gallup_mip.csv")
authority <- read_csv("data/budget_authority.csv")

# ---- BUDGET: health's share of total budget authority ----
budget_health <- authority |>
  filter(IsItMajorFunction == 1, majorfunction %in% c(550, 571)) |>   # 550 = Health, 571 = Medicare
  group_by(year) |>
  summarise(health_amt = sum(amount_nominal, na.rm = TRUE))

budget_total <- authority |>
  filter(majorfunction == 0) |>                                       # 0 = grand total row
  select(year, total_amt = amount_nominal)

budget_share <- left_join(budget_health, budget_total, by = "year") |>
  mutate(health_share = health_amt / total_amt)


# ---- PLATFORMS: health's share of policy sentences, per party and year ----
platform_share <- bind_rows(
  dem_plat |> mutate(party = "Democratic"),
  rep_plat |> mutate(party = "Republican")
) |>
  filter(!majortopic %in% c(-555, 0)) |>      # drop non-policy text
  group_by(party, year) |>
  summarise(health_share = mean(majortopic == 3), .groups = "drop")

# ---- GALLUP: public concern about health (majortopic 3) ----
gallup_health <- gallup |>
  filter(majortopic == 3) |>
  mutate(percent = ifelse(year == 1988, NA, percent))   # thin year (5 topics, whole-number %), likely a data artifact


# ---- FIGURE 1: health's share of attention across four arenas ----
fig1 <- ggplot() +
  geom_line(data = budget_share,
            aes(year, health_share, color = "Budget (health + Medicare)"),
            linewidth = 1) +
  geom_line(data = gallup_health,
            aes(year, percent, color = "Public (Gallup MIP)"),
            alpha = 0.7) +
  geom_point(data = platform_share,
             aes(year, health_share, color = paste(party, "platform")),
             size = 2.5) +
  scale_color_manual(values = c(
    "Budget (health + Medicare)" = "black",
    "Public (Gallup MIP)"        = "darkorange",
    "Democratic platform"        = "blue",
    "Republican platform"        = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Health's share", color = NULL,
       title = "Health's share of attention: public, parties, and budget",
       caption = "Shares use different denominators; compare timing and shape, not height.")

fig1

ggsave("output/fig1_health_attention.png", fig1, width = 9, height = 5, dpi = 300)




# ---- FIGURE 2: health's share of each party's platform ----
platform_plot <- platform_share |>
  complete(party, year = seq(1948, 2024, by = 4))   # adds a blank row for Republican 2020 so the line breaks there

fig2 <- ggplot(platform_plot, aes(year, health_share, color = party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Health's share of platform", color = NULL,
       title = "How much of each party's platform is about health?",
       caption = "Share of policy sentences coded Health. Republicans adopted no platform in 2020.")

fig2

ggsave("output/fig2_party_platforms.png", fig2, width = 9, height = 5, dpi = 300)

# ---- FIGURE 3: platform emphasis vs. budget share, election years ----
fig3_data <- platform_share |>
  inner_join(budget_share |> select(year, budget_share = health_share), by = "year") |>
  mutate(ratio = health_share / budget_share)

fig3 <- ggplot(fig3_data, aes(year, ratio, color = party)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_log10() +
  labs(x = NULL, color = NULL,
       y = "Platform share ÷ budget share (log scale)",
       title = "Do platforms give health more weight than the budget does?",
       caption = "Above the dashed line: health is a bigger slice of the platform than of the budget. Budget data ends 2017.")

fig3
ggsave("output/fig3_platform_vs_budget.png", fig3, width = 9, height = 5, dpi = 300)



# ---- FIGURE 3 ALT: same comparison, budget = Health function only (Medicaid in, Medicare out) ----

budget_health_only <- authority |>
  filter(IsItMajorFunction == 1, majorfunction == 550) |>
  select(year, health_only_amt = amount_nominal) |>
  left_join(budget_total, by = "year") |>
  mutate(budget_share = health_only_amt / total_amt)

fig3_alt_data <- platform_share |>
  inner_join(budget_health_only |> select(year, budget_share), by = "year") |>
  mutate(ratio = health_share / budget_share)


budget_health_only <- authority |>
  filter(IsItMajorFunction == 1, majorfunction == 550) |>     # 550 = Health only
  select(year, health_only_amt = amount_nominal) |>
  left_join(budget_total, by = "year") |>
  mutate(budget_share = health_only_amt / total_amt)

fig3_alt_data <- platform_share |>
  inner_join(budget_health_only |> select(year, budget_share), by = "year") |>
  mutate(ratio = health_share / budget_share)

fig3_alt <- ggplot(fig3_alt_data, aes(year, ratio, color = party)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_log10() +
  labs(x = NULL, color = NULL,
       y = "Platform share ÷ budget share (log scale)",
       title = "Same comparison, with Medicare left out of the budget",
       caption = "Budget = Health function (550) only, which includes Medicaid. Above the dashed line: health is a bigger slice of the platform than of the budget.")

fig3_alt

ggsave("output/fig3_alt_health_only.png", fig3_alt, width = 9, height = 5, dpi = 300)


bind_rows(dem_plat |> mutate(party = "Democratic"),
          rep_plat |> mutate(party = "Republican")) |>
  filter(majortopic == 3) |>
  count(subtopic, sort = TRUE) |>
  print(n = 25)



platform_matched <- bind_rows(
  dem_plat |> mutate(party = "Democratic"),
  rep_plat |> mutate(party = "Republican")
) |>
  filter(!majortopic %in% c(-555, 0)) |>
  mutate(is_health = majortopic == 3 &
           !str_detect(description, regex("medicare", ignore_case = TRUE))) |>
  group_by(party, year) |>
  summarise(health_share = mean(is_health), .groups = "drop")

fig3_matched_data <- platform_matched |>
  inner_join(budget_health_only |> select(year, budget_share), by = "year") |>
  mutate(ratio = health_share / budget_share)


# ---- FIGURE 3 MATCHED: Medicare sentences removed from platforms, Medicare out of budget ----
fig3_matched <- ggplot(fig3_matched_data, aes(year, ratio, color = party)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_log10() +
  labs(x = NULL, color = NULL,
       y = "Platform share ÷ budget share (log scale)",
       title = "Matched comparison: Medicare removed from both sides",
       caption = "Platform = Health sentences not mentioning 'Medicare'. Budget = Health function (550) only. Text search undercounts Medicare talk.")

fig3_matched

ggsave("output/fig3_matched.png", fig3_matched, width = 9, height = 5, dpi = 300)


# ---- COMPARISON: alt (Medicare sentences kept) vs. matched (removed) ----
fig3_compare_data <- bind_rows(
  fig3_alt_data     |> mutate(version = "Alt: Medicare sentences kept"),
  fig3_matched_data |> mutate(version = "Matched: Medicare sentences removed")
)

fig3_compare <- ggplot(fig3_compare_data, aes(year, ratio, color = party, linetype = version)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_log10() +
  facet_wrap(~party, ncol = 1) +
  labs(x = NULL, color = NULL, linetype = NULL,
       y = "Platform share ÷ budget share (log scale)",
       title = "Does removing Medicare sentences change the picture?")

fig3_compare


bind_rows(dem_plat |> mutate(party = "Democratic"),
          rep_plat |> mutate(party = "Republican")) |>
  filter(majortopic == 3) |>
  group_by(party) |>
  summarise(
    health_rows   = n(),
    mention_mcare = sum(str_detect(description, regex("medicare", ignore_case = TRUE))),
    mention_mcaid = sum(str_detect(description, regex("medicaid", ignore_case = TRUE))),
    share_mcare   = mention_mcare / health_rows
  )

ggsave("output/fig3_compare_medicare_check.png", fig3_compare, width = 9, height = 6, dpi = 300)


medicare_platform <- bind_rows(
  dem_plat |> mutate(party = "Democratic"),
  rep_plat |> mutate(party = "Republican")
) |>
  filter(!majortopic %in% c(-555, 0)) |>
  group_by(party, year) |>
  summarise(
    n_sentences = n(),
    n_mcare = sum(str_detect(description, regex("medicare", ignore_case = TRUE))),
    share = n_mcare / n_sentences,
    .groups = "drop"
  ) |>
  complete(party, year = seq(1948, 2024, by = 4))

medicare_platform |> filter(year %in% c(1988, 1992, 1996, 2016, 2024))

medicare_budget <- authority |>
  filter(IsItMajorFunction == 1, majorfunction == 571) |>
  select(year, medicare_amt = amount_nominal) |>
  left_join(budget_total, by = "year") |>
  mutate(share = medicare_amt / total_amt)


medicare_both <- bind_rows(
  medicare_platform |>
    mutate(series = "Platform sentences naming Medicare", line = party),
  medicare_budget |>
    select(year, share) |>
    mutate(series = "Medicare share of federal budget", line = "Budget")
)


fig4 <- ggplot(medicare_both, aes(year, share, color = line)) +
  geom_line() +
  geom_point(data = ~ filter(.x, line != "Budget")) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(Democratic = "blue", Republican = "red", Budget = "black")) +
  labs(title = "Is Medicare talk keeping pace with Medicare spending?",
       x = NULL, y = "Share", color = NULL,
       caption = "Platform series is a text-search floor. Panels use different denominators and y-scales: compare timing and shape, not height.")
fig4


# What are 1996 Medicare sentences actually saying?
bind_rows(dem_plat |> mutate(party = "Democratic"),
          rep_plat |> mutate(party = "Republican")) |>
  filter(year == 1996, str_detect(description, regex("medicare", ignore_case = TRUE))) |>
  select(party, majortopic, description) |>
  print(n = 30)

# Does 1992 talk about the elderly or seniors without saying "Medicare"?
bind_rows(dem_plat |> mutate(party = "Democratic"),
          rep_plat |> mutate(party = "Republican")) |>
  filter(year == 1992, majortopic == 3,
         str_detect(description, regex("elderly|senior|older americans|retire", ignore_case = TRUE))) |>
  select(party, description) |>
  print(n = 20)



both <- bind_rows(dem_plat |> mutate(party = "Democratic"),
                  rep_plat |> mutate(party = "Republican"))

# How many 1992 sentences fall under each topic?
both |> filter(year == 1992) |> count(party, majortopic) |> arrange(party, desc(n)) |> print(n = 40)

# Read a few 1992 Health sentences
both |> filter(year == 1992, majortopic == 3) |>
  mutate(description = str_trunc(description, 120)) |>
  select(party, description) |> slice_head(n = 10) |> print(n = 10)

both |> filter(year == 1992, majortopic == 3, party == "Republican") |>
  mutate(description = str_trunc(description, 120)) |>
  select(description) |> slice_sample(n = 10) |> print(n = 10)


ggsave("output/fig4_medicare_attention_vs_budget.png", fig4, width = 9, height = 6, dpi = 300)



party_gap <- bind_rows(
  dem_plat |> mutate(party = "Democratic"),
  rep_plat |> mutate(party = "Republican")
) |>
  filter(!majortopic %in% c(-555, 0)) |>
  group_by(party, year) |>
  summarise(n = n(), p = mean(majortopic == 3), .groups = "drop") |>
  pivot_wider(names_from = party, values_from = c(n, p)) |>
  filter(!is.na(p_Republican)) |>
  mutate(
    gap = p_Democratic - p_Republican,
    se  = sqrt(p_Democratic * (1 - p_Democratic) / n_Democratic +
                 p_Republican * (1 - p_Republican) / n_Republican),
    lo  = gap - 1.96 * se,
    hi  = gap + 1.96 * se
  )

party_gap |>
  summarise(mean_gap = mean(gap), sd_gap = sd(gap),
            years = n(), dem_higher = sum(gap > 0),
            clear_gap_years = sum(lo > 0 | hi < 0))

fig5 <- ggplot(party_gap, aes(year, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Democratic minus Republican health share of platform",
       x = NULL, y = "Gap (percentage points)",
       caption = "Bars are rough 95% intervals. Above 0 = Democrats give health more space.")
fig5

ggsave("output/fig5_party_gap.png", fig5, width = 9, height = 6, dpi = 300)


both |>
  filter(year %in% c(2000, 2016), majortopic == 3) |>
  count(year, party, subtopic) |>
  group_by(year, party) |>
  mutate(share_of_health = n / sum(n)) |>
  arrange(year, party, desc(n)) |>
  slice_head(n = 5) |>
  print(n = 20)


party_gap |> filter(lo > 0 | hi < 0) |> select(year, gap, lo, hi)


both |>
  filter(!majortopic %in% c(-555, 0)) |>
  group_by(party, year) |>
  summarise(total = n(), health = sum(majortopic == 3),
            share = health / total, .groups = "drop") |>
  filter(year %in% c(1964, 1984, 2000, 2008, 2012, 2016, 2024)) |>
  arrange(year, party)