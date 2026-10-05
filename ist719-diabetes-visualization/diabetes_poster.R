# =============================================================================
#  Diabetes and the other leading causes of death in the United States
#  IST 719 Information Visualization · Syracuse University
#  Akuete Giana MATHEY-APOSSAN
#
#  Data:  CDC / NCHS, "Leading Causes of Death: United States" (1999–2017)
#         https://data.cdc.gov/d/bi63-dtpu  (10,868 rows; 50 states + DC + US)
#  Run:   Rscript diabetes_poster.R
#  Needs: readr, dplyr, tidyr, ggplot2, forcats, stringr, patchwork, scales,
#         ggrepel, ragg, systemfonts. Save/run this file as UTF-8.
#  Output: output/diabetes_poster.pdf (24 x 32 in) and output/diabetes_poster.png
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(forcats); library(stringr); library(patchwork); library(scales)
  library(ggrepel)
})

# ---- 0. Settings ------------------------------------------------------------
DATA   <- "data/NCHS_-_Leading_Causes_of_Death__United_States.csv"
OUTDIR <- "output"
dir.create(OUTDIR, showWarnings = FALSE)
FONT   <- if ("Inter" %in% systemfonts::system_fonts()$family) "Inter" else "sans"

# One colour per job: orange = diabetes / the story, blue = comparison, grey = context
ORANGE <- "#C2410C"; BLUE <- "#1E4E8C"; GREY <- "#B8BDC7"; INK <- "#1F2430"
MUTED  <- "#5B6170"; PAPER <- "#FFFFFF"; SOFT <- "#FFF4EC"

theme_poster <- function(base = 15) {
  theme_minimal(base_size = base, base_family = FONT) +
    theme(
      plot.title    = element_text(face = "bold", size = base * 1.45, colour = INK,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = base * 1.0, colour = MUTED, margin = margin(b = 10),
                                   lineheight = 1.15),
      plot.caption  = element_text(size = base * 0.75, colour = MUTED, hjust = 0),
      axis.title    = element_text(size = base * 0.85, colour = MUTED),
      axis.text     = element_text(size = base * 0.8, colour = MUTED),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#ECEEF2", linewidth = 0.4),
      strip.text    = element_text(face = "bold", hjust = 0, size = base * 0.95, colour = INK),
      legend.position = "none",
      plot.title.position = "plot",
      plot.background = element_rect(fill = PAPER, colour = NA)
    )
}

# ---- 1. Load and tidy -------------------------------------------------------
raw <- read_csv(DATA, show_col_types = FALSE) |>
  rename(year = Year, cause = `Cause Name`, state = State,
         deaths = Deaths, rate = `Age-adjusted Death Rate`)

stopifnot(nrow(raw) == 10868)

us     <- raw |> filter(state == "United States")
states <- raw |> filter(state != "United States", cause != "All causes")   # avoid double counting

# ---- 2. Correlations: raw counts vs age-adjusted rates ----------------------
state_tot  <- states |> group_by(state, cause) |>
  summarise(deaths = sum(deaths), rate = mean(rate), .groups = "drop")

diab <- state_tot |> filter(cause == "Diabetes") |>
  select(state, diab_deaths = deaths, diab_rate = rate)

pairs <- state_tot |> filter(cause != "Diabetes") |> left_join(diab, by = "state")

cors <- pairs |> group_by(cause) |>
  summarise(
    r_counts = cor(deaths, diab_deaths),
    r_rates  = cor(rate, diab_rate),
    p_rates  = cor.test(rate, diab_rate)$p.value,
    r2       = r_rates^2,
    .groups = "drop") |>
  arrange(r_rates) |>
  mutate(cause = fct_inorder(cause))

write_csv(cors, file.path(OUTDIR, "correlations.csv"))

# ---- Panel A: dumbbell — why counts mislead ---------------------------------
pA <- ggplot(cors, aes(y = cause)) +
  geom_segment(aes(x = r_rates, xend = r_counts, yend = cause), colour = GREY, linewidth = 1.6) +
  geom_point(aes(x = r_counts), colour = GREY, size = 6) +
  geom_point(aes(x = r_rates), colour = BLUE, size = 6) +
  geom_text(aes(x = r_rates, label = number(r_rates, .01)), colour = BLUE,
            fontface = "bold", nudge_x = -0.045, size = 5, family = FONT) +
  geom_text(aes(x = r_counts, label = number(r_counts, .01)), colour = MUTED,
            nudge_x = 0.045, size = 4.6, family = FONT) +
  annotate("text", x = 0.99, y = nrow(cors) + 0.85, label = "Raw death counts",
           colour = MUTED, hjust = 1, size = 5, family = FONT, fontface = "bold") +
  annotate("text", x = max(cors$r_rates), y = nrow(cors) + 0.85,
           label = "Age-adjusted rates", colour = BLUE, hjust = 0.5, size = 5,
           family = FONT, fontface = "bold") +
  scale_x_continuous(limits = c(0, 1.1), breaks = seq(0, 1, .2), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.4))) +
  labs(title = "1. Why counts mislead",
       subtitle = "Pearson correlation with diabetes across 51 states (50 + DC), 1999–2017.\nBig states have more deaths from everything, so counts 'correlate' with any cause.",
       x = "Correlation with diabetes (r)", y = NULL) +
  theme_poster() + theme(panel.grid.major.y = element_blank())

# ---- Panel B: small multiples — the four strongest links on rates -----------
top4 <- cors |> slice_max(r_rates, n = 4) |> pull(cause) |> as.character()
lab_states <- c("West Virginia", "Mississippi", "Louisiana", "Hawaii", "Colorado", "Nevada")

scat <- pairs |> filter(cause %in% top4) |>
  left_join(cors |> select(cause, r_rates, p_rates, r2) |> mutate(cause = as.character(cause)),
            by = "cause") |>
  mutate(facet = sprintf("%s\nr = %.2f · R² = %.2f · %s", cause, r_rates, r2,
                         ifelse(p_rates < .001, "p < 0.001", sprintf("p = %.3f", p_rates))),
         facet = fct_reorder(facet, -r_rates))

pB <- ggplot(scat, aes(diab_rate, rate)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = ORANGE,
              fill = ORANGE, alpha = .12, linewidth = 1.1) +
  geom_point(colour = BLUE, size = 2.6, alpha = .85) +
  geom_text_repel(data = ~ filter(.x, state %in% lab_states), aes(label = state),
                  size = 3.6, colour = MUTED, family = FONT, min.segment.length = 0.2,
                  seed = 719, box.padding = 0.35) +
  facet_wrap(~ facet, nrow = 1, scales = "free_y") +
  labs(title = "2. On rates, diabetes travels with stroke, injuries, heart disease and cancer",
       subtitle = "Each dot is a state: age-adjusted deaths per 100,000, averaged 1999–2017. Shared risk factors (obesity, smoking, inactivity,\npoverty, rurality, access to care) likely link them. This is correlation, not causation.",
       x = "Diabetes death rate", y = "Death rate of the other cause") +
  theme_poster()

# ---- Panel C: tile map, 2017 diabetes rate ----------------------------------
grid <- tribble(
  ~abb, ~row, ~col,
  "AK",1,1, "ME",1,12, "VT",2,11, "NH",2,12,
  "WA",3,2,"ID",3,3,"MT",3,4,"ND",3,5,"MN",3,6,"IL",3,7,"WI",3,8,"MI",3,9,"NY",3,10,"MA",3,11,
  "OR",4,2,"NV",4,3,"WY",4,4,"SD",4,5,"IA",4,6,"IN",4,7,"OH",4,8,"PA",4,9,"NJ",4,10,"CT",4,11,"RI",4,12,
  "CA",5,2,"UT",5,3,"CO",5,4,"NE",5,5,"MO",5,6,"KY",5,7,"WV",5,8,"VA",5,9,"MD",5,10,"DE",5,11,
  "AZ",6,3,"NM",6,4,"KS",6,5,"AR",6,6,"TN",6,7,"NC",6,8,"SC",6,9,"DC",6,10,
  "OK",7,5,"LA",7,6,"MS",7,7,"AL",7,8,"GA",7,9,
  "HI",8,1,"TX",8,5,"FL",8,10)

abb <- tibble(state = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))
d17 <- states |> filter(year == 2017, cause == "Diabetes") |> left_join(abb, by = "state")
us17 <- us |> filter(year == 2017, cause == "Diabetes") |> pull(rate)
tiles <- grid |> left_join(d17, by = "abb")
stopifnot(!any(is.na(tiles$rate)))

pC <- ggplot(tiles, aes(col, -row)) +
  geom_tile(aes(fill = rate), colour = "white", linewidth = 1.4, width = .96, height = .96) +
  geom_text(aes(label = abb, colour = rate > 26), fontface = "bold", size = 5.2,
            nudge_y = .14, family = FONT) +
  geom_text(aes(label = number(rate, .1), colour = rate > 26), size = 3.9,
            nudge_y = -.2, family = FONT) +
  scale_fill_gradient(low = "#FDE7D6", high = "#7C2D12", limits = c(14, 35),
                      name = "Deaths per 100,000") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none") +
  guides(fill = guide_colourbar(title.position = "top")) +
  coord_equal() +
  labs(title = "3. Where diabetes deaths are highest (2017)",
       subtitle = sprintf("Age-adjusted diabetes death rate per 100,000. US average: %.1f.", us17)) +
  theme_poster() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(1.6, "cm"), legend.key.height = unit(.35, "cm"),
        legend.title = element_text(size = 11, colour = MUTED),
        legend.text = element_text(size = 10, colour = MUTED))

# ---- Panel D: highest 10 and lowest 5 states -------------------------------
rank17 <- d17 |> arrange(desc(rate))
hl <- bind_rows(head(rank17, 10) |> mutate(grp = "high"), tail(rank17, 5) |> mutate(grp = "low")) |>
  mutate(state = fct_reorder(state, rate))

pD <- ggplot(hl, aes(rate, state, fill = grp)) +
  geom_col(width = .72) +
  geom_vline(xintercept = us17, linetype = "dashed", colour = INK, linewidth = .5) +
  annotate("text", x = us17 + .3, y = 0.6, label = sprintf("US %.1f", us17), hjust = 0,
           size = 4.4, fontface = "bold", colour = INK, family = FONT) +
  geom_text(aes(label = number(rate, .1)), hjust = -.15, size = 4.4, colour = INK, family = FONT) +
  scale_fill_manual(values = c(high = ORANGE, low = GREY)) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  labs(title = "Highest 10 and lowest 5 states",
       subtitle = "The South and Appalachia lead; New England is lowest.",
       x = "Diabetes deaths per 100,000 (age-adjusted, 2017)", y = NULL) +
  theme_poster() + theme(panel.grid.major.y = element_blank())

# ---- Panel E: counts up, risk down (indexed, one axis) ----------------------
trend <- us |> filter(cause == "Diabetes") |> arrange(year) |>
  mutate(`Number of diabetes deaths` = deaths / first(deaths) * 100,
         `Age-adjusted death rate`   = rate / first(rate) * 100) |>
  select(year, deaths, rate, `Number of diabetes deaths`, `Age-adjusted death rate`) |>
  pivot_longer(c(`Number of diabetes deaths`, `Age-adjusted death rate`),
               names_to = "series", values_to = "index")

d99 <- us |> filter(cause == "Diabetes", year == 1999); d17u <- us |> filter(cause == "Diabetes", year == 2017)
pct_d <- (d17u$deaths / d99$deaths - 1) * 100; pct_r <- (d17u$rate / d99$rate - 1) * 100
ends  <- trend |> filter(year == 2017) |>
  mutate(lab = ifelse(str_detect(series, "Number"),
                      sprintf("%+.0f%%\n%s deaths", pct_d, comma(d17u$deaths)),
                      sprintf("%+.0f%%\n%.1f per 100k", pct_r, d17u$rate)))
starts <- trend |> filter(year == 2012)

pE <- ggplot(trend, aes(year, index, colour = series)) +
  geom_hline(yintercept = 100, colour = MUTED, linewidth = .4) +
  geom_line(linewidth = 1.6) + geom_point(size = 2.6) +
  geom_text(data = ends, aes(label = lab), hjust = -.12, size = 4.6, fontface = "bold",
            family = FONT, lineheight = .95) +
  geom_text(data = starts, aes(label = series, vjust = ifelse(str_detect(series, "Number"), -1.9, 2.2)), hjust = 0.5, size = 4.6,
            fontface = "bold", family = FONT) +
  scale_colour_manual(values = c(`Number of diabetes deaths` = ORANGE, `Age-adjusted death rate` = BLUE)) +
  scale_x_continuous(breaks = seq(1999, 2017, 2), limits = c(1999, 2020.5)) +
  labs(title = "4. More people die of diabetes each year, yet each person's risk has fallen",
       subtitle = sprintf("US, 1999–2017, indexed to 1999 = 100. Deaths grow with a larger, older population (%s → %s);\nthe age-adjusted rate removes that effect (%.1f → %.1f per 100,000).",
                          comma(d99$deaths), comma(d17u$deaths), d99$rate, d17u$rate),
       x = NULL, y = "Index (1999 = 100)") +
  theme_poster()

# ---- Panel F: rank among leading causes, 2017 -------------------------------
lead17 <- us |> filter(year == 2017, cause != "All causes") |>
  mutate(cause = fct_reorder(cause, deaths), is_d = cause == "Diabetes")
rank_d <- which(rev(levels(lead17$cause)) == "Diabetes")

pF <- ggplot(lead17, aes(deaths / 1000, cause, fill = is_d)) +
  geom_col(width = .72) +
  geom_text(aes(label = paste0(round(deaths / 1000), "k")), hjust = -.15, size = 4.2,
            colour = INK, family = FONT) +
  scale_fill_manual(values = c(`TRUE` = ORANGE, `FALSE` = GREY)) +
  scale_x_continuous(expand = expansion(mult = c(0, .15))) +
  labs(title = sprintf("Diabetes ranks %s", c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")[rank_d]),
       subtitle = "US leading causes, 2017", x = "Deaths (thousands)", y = NULL) +
  theme_poster() + theme(panel.grid.major.y = element_blank())

# ---- Text blocks -------------------------------------------------------------
text_panel <- function(title, body, fill = PAPER) {
  ggplot() + annotate("text", x = 0, y = 1, label = title, hjust = 0, vjust = 1,
                      fontface = "bold", size = 7, colour = INK, family = FONT) +
    annotate("text", x = 0, y = .82, label = body, hjust = 0, vjust = 1, size = 4.7,
             colour = INK, family = FONT, lineheight = 1.2) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + ylim(0, 1) + theme_void() +
    theme(plot.background = element_rect(fill = fill, colour = NA),
          plot.margin = margin(16, 16, 16, 16))
}

top_r <- cors |> arrange(desc(r_rates)) |> slice(1:3)
key <- text_panel("Key findings", paste0(
  "• Raw counts give r = ", number(min(cors$r_counts), .01), "–", number(max(cors$r_counts), .01),
  " for every cause:\n  a population artefact.\n\n",
  "• On rates, diabetes tracks ", tolower(top_r$cause[1]), " (r = ", number(top_r$r_rates[1], .01), "),\n  ",
  tolower(top_r$cause[2]), " (", number(top_r$r_rates[2], .01), ") and ", tolower(top_r$cause[3]),
  " (", number(top_r$r_rates[3], .01), ").\n\n",
  "• Deaths rose ", round(pct_d), "% but the age-adjusted\n  rate fell ", abs(round(pct_r)), "% (1999–2017).\n\n",
  "• Diabetes is the ", c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")[rank_d],
  " leading cause of death (2017)."), fill = SOFT)

why <- text_panel("Why it matters", str_wrap(
  "Diabetes deaths concentrate in the South and Appalachia, in the same states with high heart, lung and cancer death rates. Prevention aimed at shared risk factors (obesity, smoking, inactivity, access to care) can reduce several causes of death at once.", 48))
aud <- text_panel("Audience and action", paste(
  str_wrap("1) People with diabetes or prediabetes: screening and early management.", 46),
  str_wrap("2) Health systems and state health departments: focus resources on the highest-rate states.", 46),
  str_wrap("3) Food industry and policymakers: reduce added sugar; support healthier food environments.", 46), sep = "\n"))
meth <- text_panel("Method and limits", str_wrap(paste0(
  "Pearson correlations across 51 states (50 + DC) on age-adjusted rates averaged 1999–2017; p-values from two-sided tests (n = 51). ",
  "State-level (ecological) correlations do not prove that diabetes causes other deaths, and rates are not adjusted for income or smoking. ",
  "'All causes' rows were excluded to avoid double counting. Built entirely in R (ggplot2, patchwork)."), 60))

# ---- Assemble -----------------------------------------------------------------
header <- ggplot() +
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, family = FONT, fontface = "bold",
           size = 15.5, colour = INK, label = "Diabetes and the other leading causes of death in the US") +
  annotate("text", x = 0, y = .6, hjust = 0, vjust = 1, family = FONT, size = 8.2, colour = MUTED,
           lineheight = 1.1,
           label = "States with higher diabetes death rates also have higher stroke, heart disease and cancer death rates:\na link you only see when you compare rates, not raw counts.") +
  annotate("text", x = 0, y = .02, hjust = 0, vjust = 0, family = FONT, size = 5.4, colour = MUTED,
           label = "Akuete Giana MATHEY-APOSSAN  ·  IST 719 Information Visualization  ·  Syracuse University  ·  Data: CDC / NCHS, Leading Causes of Death, 1999–2017 (10,868 rows)") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + ylim(0, 1) + theme_void()

footer <- ggplot() +
  annotate("text", x = 0, y = .5, hjust = 0, family = FONT, size = 4.6, colour = MUTED,
           label = "Source: CDC, National Center for Health Statistics. NCHS – Leading Causes of Death: United States (data.cdc.gov/d/bi63-dtpu), 1999–2017. Rates per 100,000, age-adjusted to the 2000 US standard population.\nCode: github.com/gmatheya/applied-data-science-portfolio  ·  R 4 · ggplot2 · patchwork · ggrepel") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + ylim(0, 1) + theme_void()

row1 <- (pA | key) + plot_layout(widths = c(2.3, 1))
row3 <- (pC | pD) + plot_layout(widths = c(1.15, 1))
row4 <- (pE | pF) + plot_layout(widths = c(2.1, 1))
row5 <- (why | aud | meth) + plot_layout(widths = c(1, 1, 1.3))

poster <- wrap_elements(full = header) / row1 / pB / row3 / row4 / row5 / wrap_elements(full = footer) +
  plot_layout(heights = c(.5, 1.05, 1.05, 1.3, 1.0, .72, .12)) &
  theme(plot.margin = margin(14, 22, 14, 22))

ggsave(file.path(OUTDIR, "diabetes_poster.pdf"), poster, width = 24, height = 32,
       device = cairo_pdf, bg = PAPER)
ggsave(file.path(OUTDIR, "diabetes_poster.png"), poster, width = 24, height = 32,
       dpi = 110, bg = PAPER, device = ragg::agg_png)

# Individual panels for the portfolio page
for (nm in c("pA", "pB", "pC", "pD", "pE", "pF")) {
  ggsave(file.path(OUTDIR, paste0("panel_", nm, ".png")), get(nm),
         width = ifelse(nm == "pB", 16, 10), height = 6.5, dpi = 150, bg = PAPER,
         device = ragg::agg_png)
}
cat("Done. Wrote", OUTDIR, "\n"); print(cors)
