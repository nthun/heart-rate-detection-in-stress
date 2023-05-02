# Extra analysis


# Physiology significance
library(ggsignif)

# Create table for allotation
annotation_df <- 
  phys_trans %>% 
  dplyr::select(id, marker, meanhr, lg_hf_percent, lg_rmssd, lg_eda_avg) %>% 
  pivot_longer(meanhr:lg_eda_avg, values_drop_na = TRUE) %>%
  left_join(phys_metrics, by = "name") %>% 
  group_by(metric) %>%
  nest() %>% 
  summarise(model = map(data, ~lmer(value ~ marker + (1|id), data = .x) %>% 
                               broom.mixed::tidy())) %>% 
  unnest(model) %>% 
  filter(effect == "fixed", term != "(Intercept)") %>% 
  mutate(term = str_remove(term, "marker")) %>% 
  transmute(metric, 
            start = "Baseline",
            end = term, 
            marker = end,
            label = paste0("p=",scales::pvalue(p.value)) %>% 
                    str_replace("=<", "<"),
            y = c(81.7, 82.4, 3.17, 3.21, 3.64, 3.665, 2.12, 2.16)) %>% 
  group_by(metric) %>% 
  mutate(top = max(y) * 1.015) %>% 
  ungroup()


# Plot physiological changes with significance
phys_trans %>% 
  dplyr::select(id, marker, meanhr, lg_hf_percent, lg_rmssd, lg_eda_avg) %>% 
  pivot_longer(meanhr:lg_eda_avg, values_drop_na = TRUE) %>%
  left_join(phys_metrics, by = "name") %>% 
  ggplot() +
  aes(x = marker, y = value, group = metric, color = metric) +
  stat_summary(fun.data = "mean_se", show.legend = FALSE, 
               geom = "errorbar", color = "gray", width = .2) +
  stat_summary(fun.data = "mean_se", geom = "point", show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", show.legend = FALSE, geom = "line", 
               size = 1.2, alpha = .6) +
  facet_wrap(~metric, scales = "free_y") +
  labs(y = "Mean value (SEM)",
       x = NULL) +
  theme(panel.grid = element_blank()) +
  geom_blank(data = annotation_df, aes(y = top)) +
  geom_signif(inherit.aes = FALSE, 
              data = annotation_df, manual = TRUE, 
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              color = "black", alpha = .6,
              textsize = 3, tip_length = .002, margin_top = 12, vjust = -.1)


### Figure: HBP/HRP moderates the association of objective and subjective change metrics


interaction_df <- 
  reactivity %>% 
  select(id, `meanhr_react_N-back`, meanhr_react_Handgrip,
         `stai_react_N-back`, stai_react_Handgrip) %>%
  left_join(select(experiment, id, ca, hrp, hrp_group, hbp_group), by = "id") %>%
  pivot_longer(`meanhr_react_N-back`:stai_react_Handgrip,
               names_to = c("name", "task"),
               names_pattern = "(.*_react)_(.*)") %>% 
  pivot_wider(names_from = "name", values_from = "value")

hbp_plot <- 
  interaction_df %>% 
  ggplot() +
  aes(y = stai_react, x = meanhr_react, color = hbp_group) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .2) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .2) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE, alpha = .7) +
  scale_color_brewer(palette = "Reds") +
  facet_wrap(~task, scales = "free_x") +
  labs(color = "HBP group",
       x = NULL,
       y = "STAI change") +
  theme(legend.position = c(0.93, 0.22), panel.grid = element_blank())

hrp_plot <-
  interaction_df %>% 
  ggplot() +
  aes(y = stai_react, x = meanhr_react, color = hrp_group) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .2) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .2) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE, alpha = .7) +
  scale_color_brewer(palette = "Blues") +
  facet_wrap(~task, scales = "free_x") +
  labs(color = "HRP group",
       x = "HR change",
       y = "STAI change") +
  theme(legend.position = c(0.93, 0.22), panel.grid = element_blank())

hbp_plot + hrp_plot + plot_layout(nrow = 2)

experiment %>% 
select(physical_expected_stress,
       mental_expected_stress,
       physical_expected_hr_change,
       mental_expected_hr_change) %>% 
correlation()

# Moderation plot by path analysis predictions ----------------------------



# Extra table -------------------------------------------------------------

hypo3_sum %>% 
  separate(model, into = c("task", NA, "name", "ca"), sep = "_", remove = FALSE) %>%
  left_join(phys_metrics, by = "name") %>% 
  filter(!term %in% c("(Intercept)", "ca", "hrp", "task_orderPhysical first")) %>% 
  mutate(term = str_remove(term, "mental_|physical_") %>% 
                recode(meanhr = "phys_change", hf.percent = "phys_change", rmssd = "phys_change", eda.avg = "phys_change"),
         sig = case_when(p.value <= .001 ~ "***",
                            p.value <= .01 ~ "**",
                            p.value <= .05 ~ "*",
                            TRUE ~ "")) %>% 
  ggplot() +
  aes(y = metric, x = term, fill = estimate, label = str_glue("{round(estimate, 2)}{sig}")) +
  geom_tile(show.legend = FALSE) +
  scale_fill_gradient2() +
  geom_text() +
  facet_wrap(~interaction(task, ca), ncol = 1, scales = "free") +
  labs(y = NULL, x = NULL)

# gt

hypo3_sum %>% 
  separate(model, into = c("task", NA, "name", "ca"), sep = "_") %>%
  left_join(phys_metrics, by = "name") %>% 
  filter(!term %in% c("(Intercept)", "ca", "hrp", "task_orderPhysical first")) %>% 
  mutate(term = str_remove(term, "mental_|physical_") %>% 
                recode(meanhr = "phys_change", hf.percent = "phys_change", 
                       rmssd = "phys_change", eda.avg = "phys_change"),
         sig = case_when(p.value <= .001 ~ "***",
                         p.value <= .01 ~ "**",
                         p.value <= .05 ~ "*",
                         TRUE ~ "")) %>% 
  transmute(task, ca, metric, term, 
            # value = str_glue("{round(estimate, 2)}{sig}"),
            estimate, sig
            ) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, sig)) %>% 
  arrange(task, ca, metric) %>% 
  gt(rowname_col = "ca",
     groupname_col = "task") %>% 
  cols_align(columns = metric, align = "left") %>% 
  fmt_number(columns = c(estimate_phys_change, estimate_expected_stress), decimals = 2) %>% 
  data_color(columns = c(estimate_phys_change, estimate_expected_stress), 
             palette = c("blue", "white", "red")) %>% 
  cols_merge(columns = c(estimate_phys_change, sig_phys_change)) %>% 
  cols_merge(columns = c(estimate_expected_stress, sig_expected_stress))
  
