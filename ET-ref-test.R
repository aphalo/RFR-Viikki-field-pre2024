library(ggplot2)
library(ggpmisc)
library(dplyr)

load("minute_2017_2021.tb.rda")

minute_2015_2021.tb -> minute.tb

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(PAR_umol_CS, PAR_umol_BF)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(formula = y ~ x) +
  expand_limits(x = 1, y = 1) +
  scale_x_log10() +
  scale_y_log10()

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(PAR_umol_CS, PAR_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 1, y = 1) +
  scale_x_log10() +
  scale_y_log10()

minute.tb %>%
  filter(sun_elevation > 0 & PAR_umol_CS < 2200 & PAR_umol_CS > 0) %>%
  ggplot(aes(global_watt, PAR_umol_CS)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, PAR_umol_BF)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, PAR_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, red_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, far_red_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, red_far_red)) +
  geom_point(alpha = 0.05) +
  stat_poly_line() +
  stat_poly_eq() +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, blue_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, UVA_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ poly(x, 3, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = after_stat(eq.label))) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(global_watt, UVB_umol)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)


## Evapotranspiration

minute.tb %>%
  filter(sun_elevation > 0) %>%
  ggplot(aes(UVB_Den_Avg, ET_ref)) +
  geom_point(alpha = 0.05) +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(UVB_mmol_day, ET_ref_day)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_quant_eq(formula = y ~ poly(x, 2, raw = TRUE), color = "blue") +
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE), color = "red") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), color = "red",
               label.x = "right", label.y = "bottom",
               mapping = aes(label = paste(after_stat(eq.label),
                                           after_stat(rr.label),
                                           after_stat(n.label),
                                           sep = "*\", \"*"))) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVA_mmol_day = sum(UVA_umol) * 60 * 1e-3,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(UVA_mmol_day, ET_ref_day)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 1, raw = TRUE)) +
  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "blue") +
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), color = "red") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "red",
               label.x = "right", label.y = "bottom",
               mapping = aes(label = paste(after_stat(eq.label),
                                           after_stat(rr.label),
                                           after_stat(n.label),
                                           sep = "*\", \"*"))) +
  expand_limits(x = 0, y = 0)


minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(blue_mmol_day = sum(blue_umol) * 60 * 1e-3,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(blue_mmol_day, ET_ref_day)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 1, raw = TRUE)) +
  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "blue") +
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), color = "red") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "red",
               label.x = "right", label.y = "bottom",
               mapping = aes(label = paste(after_stat(eq.label),
                                           after_stat(rr.label),
                                           after_stat(n.label),
                                           sep = "*\", \"*"))) +
  expand_limits(x = 0, y = 0)


minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440 & !is.na(UVB_mmol_day)) %>%
  ggplot(aes(PAR_mol_day, ET_ref_day)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 1, raw = TRUE)) +
  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "blue") +
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), color = "red") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "red",
               label.x = "right", label.y = "bottom",
               mapping = aes(label = paste(after_stat(eq.label),
                                           after_stat(rr.label),
                                           after_stat(n.label),
                                           sep = "*\", \"*"))) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(PAR_mol_day, ET_ref_day)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 1, raw = TRUE)) +
  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "blue") +
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), color = "red") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), color = "red",
               label.x = "right", label.y = "bottom",
               mapping = aes(label = paste(after_stat(eq.label),
                                           after_stat(rr.label),
                                           after_stat(n.label),
                                           sep = "*\", \"*"))) +
  expand_limits(x = 0, y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            global_Mj_day = sum(global_watt) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440 & !is.na(UVB_mmol_day)) %>%
  ggplot(aes(day_of_year, global_Mj_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440 & !is.na(UVB_mmol_day)) %>%
  ggplot(aes(day_of_year, PAR_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            global_Mj_day = sum(global_watt) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, global_Mj_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, PAR_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, UVB_mmol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVA_mol_day = sum(UVA_umol) * 60 * 1e-6,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, UVA_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(blue_mol_day = sum(blue_umol) * 60 * 1e-6,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, blue_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(red_mol_day = sum(red_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, red_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(far_red_mol_day = sum(far_red_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, far_red_mol_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440) %>%
  ggplot(aes(day_of_year, ET_ref_day)) +
  geom_point() +
  expand_limits(y = 0)

minute.tb %>%
  group_by(calendar_year, day_of_year) %>%
  summarize(UVB_mmol_day = sum(UVB_umol) * 60 * 1e-3,
            PAR_mol_day = sum(PAR_umol) * 60 * 1e-6,
            ET_ref_day = sum(ET_ref) / 60,
            n = n(),
            .groups = "drop") %>%
  filter(!n > 1440 & !is.na(UVB_mmol_day)) %>%
  ggplot(aes(day_of_year, ET_ref_day)) +
  geom_point() +
  expand_limits(y = 0)
