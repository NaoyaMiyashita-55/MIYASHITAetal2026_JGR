library("tidyverse")
library("car")
library("lubridate")
library("scales")
library("ggpmisc")
library("cowplot")
library("patchwork")
library("car")
library("ggh4x")
library("ggbeeswarm") 
library("dunn.test")

#Figure2_1 #Precipitation
Figure2_rain =
  ggplot(data = read.csv(file = "MIYASHITAetal2026_Fig2.csv"),
         aes(x = as.Date(ymd(ymd, tz = "Japan")),y = rain))+
  geom_col(fill = "navy")+
  scale_x_date(date_breaks = "month",
                   labels = label_date_short(format = c("%Y", "%b"), sep = "\n"))+
  scale_y_reverse(expand = c(0, 0),
                  limits = c(90, 0))+
  theme_cowplot()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9, color = "white"),
        axis.text.y = element_text (size = 9),
        axis.title.x = element_text (size = 10, color = "white"),
        axis.title.y = element_text (size = 10),
        axis.line.x = element_line (color = "white"),
        panel.background = element_rect(color = "white"))+
  labs(x = "Date",
       y = expression(paste("Precipitation ", '[', mm~d^-1, ']')))

#Figure2_2 #WL
Figure2_WL =
  ggplot(data = read.csv(file = "MIYASHITAetal2026_Fig2.csv"),
         aes(x = as.Date(ymd(ymd, tz = "Japan")), y = WL))+
  geom_ribbon(aes(ymax = WL, ymin = 0), 
              fill = "blue", alpha = 0.3)+
  scale_x_date(date_breaks = "month",
                   labels = label_date_short(format = c("%Y", "%b"), sep = "\n"))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 1.8, by = 0.5),
                     position = "right")+
  theme_cowplot()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        panel.background = element_rect(color = "black"),
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.2),
        panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"))+
  labs(x = "Date",
       y = "Water level [m]")

#Figure2_3 #WL+Precipitation
aligned_plots3 <- align_plots(Figure2_rain, Figure2_WL, align="hv", axis="tblr")
Figure2 = ggdraw(aligned_plots3[[1]]) + draw_plot(aligned_plots3[[2]])+
  theme(plot.background = element_rect(fill = "white", colour = NA))

#Figure2_4 #Sampling date #Combine Figure2_3 and 2_4 in PowerPoint → Figure2
Figure2_samplingdate =
  ggplot(data = read.csv(file = "MIYASHITAetal2026_Fig2.csv"),
         aes(x = as.Date(ymd(ymd, tz = "Japan"))))+
  geom_point(aes(y=snapshot), shape="↓", size = 1.8)+
  geom_line(aes(y=continuous), color = "red", linewidth = 0.4)+
  scale_x_date(date_breaks = "month",
               labels = label_date_short(format = c("%Y", "%b"), sep = "\n"))+
  theme_cowplot()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        panel.background = element_rect(color = "black"))+
  labs(x = "Date",
       y = element_blank())

#Figure3 #Chl #pCO2 #Add pCO2=400 in Inkscape
parameter =
  as_labeller (c("1Chla" = "atop('Chl-'*italic(a),'['*\u00B5*g~L^-1*']')",
                 "0pCO2" = "atop(italic(p)*CO[2], '['*\u00B5*atm*']')"),label_parsed)
Figure3 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig3.csv"),
          aes (year_month, average)) +
  geom_col ( group = 1, width = 0.7, linewidth =0.3, color = "black", fill = "grey" )+
  geom_errorbar( group = 1, width = 0.4, linewidth =0.3,
                 aes ( ymax = average + 2 * se, 
                       ymin = average - 2 * se),
                 color = "black")+
  scale_x_discrete(labels = c("2022_09" = "9\n2022",
                              "2022_10" = "10", "2022_11" = "11",
                              "2022_12" = "12", "2023_01" = "1\n2023",
                              "2023_02" = "2", "2023_03" = "3",
                              "2023_04" = "4", "2023_05" = "5",
                              "2023_06" = "6", "2023_07" = "7",
                              "2023_08" = "8", "2023_09" = "9"))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  facet_wrap (vars(parameter), scales = "free_y",  strip.position = "left", ncol = 1,
              labeller = parameter)+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8.1),
        axis.text.y = element_text (size = 8.1),
        axis.title.x = element_text (size = 10),
        strip.text = element_text(size = 9, lineheight = 0.01),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5, linetype = "11"),
        panel.background = element_rect(color = "black"),
        panel.spacing.y = grid::unit(0.5,"lines"),
        strip.placement = "outside", strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text (lineheight = 0.01),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(x = "Month", y = element_blank())

#Figure4 #pCO2 #Chla
Figure4 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig4.csv"),
          aes ( x = Chl, y = pCO2)) +
  geom_smooth (method="glm", method.args = list(family = Gamma(link = "log")),
               color = "grey20", linewidth = 0.5)+
  geom_point ( aes(color = season, shape = season), size = 1)+
  scale_color_manual(values = c("darkorange1", "cornflowerblue", "limegreen", "firebrick1"),
                     labels = c("1Autumn" = "2022\nSep–Nov",
                                "2Winter" = "Dec–\n2023 Feb",
                                "3Spring" = "Mar–May",
                                "4Summer" = "Jun–Aug"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1Autumn" = "2022\nSep–Nov",
                                "2Winter" = "Dec–\n2023 Feb",
                                "3Spring" = "Mar–May",
                                "4Summer" = "Jun–Aug"))+
  guides(color = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))+
  theme_classic()+
  theme(axis.text = element_text (color = "black", size = 9),
        axis.title = element_text (color = "black", size = 10),
        legend.text = element_text (color = "black", size = 9),
        legend.position = "right",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(1.5, "lines"), 
        legend.box.margin = margin(0, 0, 0, -15),
        legend.spacing.y = unit(1.2, "lines"),
        panel.background = element_rect(color = "black"))+
  labs(x = expression(paste('Chl-', italic(a), ' [\u00B5'*g~L^-1*']')),
       y = expression(paste(italic(p), {CO[2]}, ' [\u00B5'*atm*']')))

#Figure4 #caption
GLM_pCO2_Chla = glm(pCO2 ~ Chl, family=Gamma(link="log"),
                    data = read_csv(file = "MIYASHITAetal2026_Fig4.csv"))
summary.glm(GLM_pCO2_Chla)


#Figure5_1 #pCO2 #no legend
Figure5_CO2_nonlegend =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
          aes ( x = time, y = pCO2_ave)) +
  geom_linerange ( aes ( ymax = pCO2_ave + 2 * pCO2_SE, 
                         ymin = pCO2_ave - 2 * pCO2_SE),
                   linewidth = 0.4)+
  geom_point ( aes(fill = PAR), shape = 23, size = 2, stroke = 0.25 )+
  scale_fill_viridis_c(option = "plasma")+
  facet_wrap2 ( vars(year_month), dir = "v", axes = "all", remove_labels = "x",
               labeller = as_labeller ( c( "2022_09_11" = "(a) Autumn: 2022 Sep–Nov",
                                           "2022_12_2023_02" = "(b) Winter: 2022 Dec–2023 Feb",
                                           "2023_03_05" = "(c) Spring: 2023 Mar–May",
                                           "2023_06_08" = "(d) Summer: 2023 Jun–Aug")),
               nrow=4, scales = "free_y", strip.position = "top")+
  scale_x_continuous ( breaks = seq ( 0, 24, by = 2))+
  theme_cowplot()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        legend.text = element_text (size = 9),
        legend.box = element_blank(),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        legend.title = element_text (size = 9),
        strip.text = element_text (size = 9, hjust = 0),
        panel.spacing = grid::unit(0.05, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.placement = "inside", 
        strip.background = element_blank(),
        legend.position = "none",
        legend.direction = "none")+
  labs(x = "Local time",
       y = expression(paste(italic(p), {CO[2]}, ' [\u00B5'*atm*']')),
       fill = expression(atop('PAR ', '[\u00B5'*mol~m^-2~s^-1*']')))
#with legend
Figure5_CO2 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
          aes ( x = time, y = pCO2_ave)) +
  geom_linerange ( aes ( ymax = pCO2_ave + 2 * pCO2_SE, 
                         ymin = pCO2_ave - 2 * pCO2_SE),
                   linewidth = 0.4)+
  geom_point ( aes(fill = PAR), shape = 23, size = 2, stroke = 0.25 )+
  scale_fill_viridis_c(option = "plasma")+
  facet_wrap2 ( vars(year_month), dir = "v", axes = "all", remove_labels = "x",
               labeller = as_labeller ( c( "2022_09_11" = "2022 Sep–Nov (Autumn)",
                                           "2022_12_2023_02" = "2022 Dec–2023 Feb (Winter)",
                                           "2023_03_05" = "2023 Mar–May (Spring)",
                                           "2023_06_08" = "2023 Jun–Aug (Summer)")),
               nrow=4, scales = "free_y", strip.position = "top")+
  scale_x_continuous ( breaks = seq ( 0, 24, by = 2))+
  theme_cowplot()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        legend.text = element_text (size = 9),
        legend.box = element_blank(),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        legend.title = element_text (size = 9, margin = margin(r = 10)),
        legend.key.height = unit (0.3, "cm"),
        legend.key.width = unit (1.0, "cm"),
        strip.text = element_text (size = 9, hjust = 0),
        panel.spacing = grid::unit(0.05, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.placement = "inside", 
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(x = "Local time",
       y = expression(paste(italic(p), {CO[2]}, ' [\u00B5'*atm*']')),
       fill = expression(atop('PAR ', '[\u00B5'*mol~m^-2~s^-1*']')))

#Figure5_2 #WT
Figure5_WT =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
          aes ( x = time, y = WT)) +
  geom_ribbon ( aes ( ymax = WT + 2 * WT_SE, 
                         ymin = WT - 2 * WT_SE),
                fill = "orangered", alpha = 0.2)+
  geom_line ( color = "orangered", linetype = "dashed" )+
  facet_wrap2 ( vars(year_month), dir = "v", axes = "all", remove_labels = "x",
               labeller = as_labeller ( c( "2022_09_11" = "2022 Sep–Nov (Autumn)",
                                           "2022_12_2023_02" = "2022 Dec–2023 Feb (Winter)",
                                           "2023_03_05" = "2023 Mar–May (Spring)",
                                           "2023_06_08" = "2023 Jun–Aug (Summer)")),
               nrow=4, scales = "free_y", strip.position = "top")+
  scale_x_continuous ( breaks = seq ( 0, 24, by = 2))+
  scale_y_continuous(position = "right")+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text (size = 9, color = "orangered"),
        legend.text = element_text (size = 9),
        axis.line.y = element_line (color = "orangered"),
        axis.ticks.y = element_line (color = "orangered"),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10, color = "orangered"),
        strip.text = element_text (size = 9, hjust = 0, color = "white"),
        panel.spacing = grid::unit(0.05, "lines"),
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.2),
        panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent",color = NA),
        strip.placement = "inside", 
        strip.background = element_blank())+
  labs(x = "",
       y = expression(paste('Water temperature ', '['^degree*C*']')))

#Figure5 #Combination
aligned_plots <- align_plots(Figure5_WT, Figure5_CO2_nonlegend, align="hv", axis="tblr")
diel_CO2_WT_3month = ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
Figure5_CO2_WT = plot_grid(plot_grid(diel_CO2_WT_3month + theme(legend.position = "none"),
                        label_size = 10))
PAR_legend = get_plot_component(Figure5_CO2, 'guide-box-bottom', return_all = FALSE)
Figure5 = plot_grid(Figure5_CO2_WT,
                                   NULL,
                                   plot_grid(NULL, PAR_legend, NULL,
                                             rel_widths = c(.1, 1, .1)),
                                   ncol = 1, rel_heights = c (1, .02, .08))+
  theme(plot.background = element_rect(fill = "transparent",color = NA))

#Figure6a-6d #ΔpCO2(T) #WT
dieldpCO2T_WT_48 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
          aes ( x = WT, y = dpCO2T_obs)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = 1, color = "gray") +
  geom_path(linewidth = 0.3, color = "gray30", linetype = "dotted")+
  geom_point ( aes(color = time), shape = 16 ,size = 1, alpha = 0.8 )+
  geom_smooth (method="lm", formula= y~x, color = "black", linewidth = 0.5)+
  scale_color_gradientn(colors = c("navy", "green", "yellow","chocolate", "maroon"),
                        breaks = c(0, 6, 12, 18,24),
                        limits = c(0, 24))+
  stat_poly_eq(formula = y ~ x, size = 2.5,
               aes(label = paste(stat(rr.label),
                                 stat(p.value.label),
                                 sep = "~~~")),
               label.x = "left", label.y = "bottom",
               parse = TRUE)+
  facet_wrap ( vars(year_month), scales = "free_x", dir = "v",
               labeller = as_labeller ( c("2022_09_11" = "2022 Sep–Nov (Autumn)",
                                          "2022_12_2023_02" = "2022 Dec–2023 Feb (Winter)",
                                          "2023_03_05" = "2023 Mar–May (Spring)",
                                          "2023_06_08" = "2023 Jun–Aug (Summer)")),
               nrow=1, strip.position = "top")+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8, vjust = 0.35),
        legend.text = element_text (size = 8, hjust = 0.5),
        axis.title.x = element_text (size = 9),
        axis.title.y = element_text (size = 9),
        legend.title = element_text (size = 9),
        legend.key.height = unit (0.6, "cm"),
        legend.key.width = unit (0.3, "cm"),
        strip.text = element_text (size = 6.8, color = "black", hjust = 0.5),
        strip.background = element_blank(),
        panel.spacing.x = grid::unit(0.6, "lines"),
        panel.spacing.y = grid::unit(0,"lines"),
        panel.background = element_rect(color = "black"),
        axis.line = element_line(color = "black"),
        legend.position = "right",
        legend.direction = "vertical")+
  labs(x = expression(paste('WT', ' ['^degree*C*']')),
       y = expression(paste('\u2206', italic(p),{CO[2(T)]}, ' [\u00B5'*atm*']')),
       color = "Local\ntime")

#Figure6e-6h #ΔpCO2(T) #PAR
  dieldpCO2T_PAR_48 =
    ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
            aes ( x = PAR, y = dpCO2T_obs)) +
    geom_hline(yintercept = 0, linewidth = 0.5, linetype = 1, color = "gray") +
    geom_path(linewidth = 0.3, color = "gray30", linetype = "dotted")+
    geom_point ( aes(color = time), shape = 16 ,size = 1, alpha = 0.8 )+
    geom_smooth (method="lm", formula= y~x, color = "black", linewidth = 0.5)+
    scale_color_gradientn(colors = c("navy", "green", "yellow","chocolate", "maroon"),
                          breaks = c(0, 6, 12, 18,24),
                          limits = c(0, 24))+
    stat_poly_eq(formula = y ~ x, size = 2.5,
                 aes(label = paste(stat(rr.label),
                                   stat(p.value.label),
                                   sep = "~~~")),
                 label.x = "left", label.y = "bottom",
                 parse = TRUE)+
    facet_wrap ( vars(year_month), scales = "free_x", dir = "v",
                 labeller = as_labeller ( c("2022_09_11" = "2022 Sep–Nov (Autumn)",
                                            "2022_12_2023_02" = "2022 Dec–2023 Feb (Winter)",
                                            "2023_03_05" = "2023 Mar–May (Spring)",
                                            "2023_06_08" = "2023 Jun–Aug (Summer)")),
                 nrow=1)+
    theme_classic()+
    theme(axis.text = element_text (color = "black"),
          axis.text.x = element_text (size = 8),
          axis.text.y = element_text (size = 8, vjust = 0.35),
          legend.text = element_text (size = 8, hjust = 0.5),
          axis.title.x = element_text (size = 9),
          axis.title.y = element_text (size = 9),
          legend.title = element_text (size = 9),
          legend.key.height = unit (0.6, "cm"),
          legend.key.width = unit (0.3, "cm"),
          strip.text = element_blank(),
          panel.spacing.x = grid::unit(0.6, "lines"),
          panel.spacing.y = grid::unit(0,"lines"),
          panel.background = element_rect(color = "black"),
          axis.line = element_line(color = "black"),
          strip.placement = "inside",
          strip.background = element_blank(),
          legend.position = "right",
          legend.direction = "vertical")+
    labs(x = expression(paste('PAR', ' [\u00B5'*mol~m^-2~s^-1*']')),
         y = expression(paste('\u2206',italic(p), {CO[2(T)]},' [\u00B5'*atm*']')),
         color = "Local\ntime")

#Figure #Combination
  dieldpCO2T <- wrap_plots(
    dieldpCO2T_WT_48,
    plot_spacer(),
    dieldpCO2T_PAR_48,
    ncol = 1,
    heights = c(1, 0.1, 1, 0.1, 1)
  ) + 
    plot_layout(guides = "collect") & 
    theme(
      legend.position = "right",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

#Figure7 #Processing (coloring) in PowerPoint
Figure7 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_diel_3month.csv"),
          aes (x = time)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = 1, color = "gray") +
  geom_linerange ( aes ( ymax = dpCO2T_obs + 2 * dpCO2T_obs_se, 
                         ymin = dpCO2T_obs - 2 * dpCO2T_obs_se),
                   linewidth = 0.5)+
  geom_point ( aes(y = dpCO2T_obs), shape = 23, size = 1, stroke = 0.5,
               fill = "white", color = "black")+
  geom_path ( aes(y = dpCO2T_model), linewidth = 0.5, linetype = "solid", color = "red", alpha = 0.75 )+
  facet_wrap2 ( vars(year_month), dir = "v", axes = "all", remove_labels = "x",
                labeller = as_labeller ( c( "2022_09_11" = "(a) Autumn: 2022 Sep–Nov",
                                            "2022_12_2023_02" = "(b) Winter: 2022 Dec–2023 Feb",
                                            "2023_03_05" = "(c) Spring: 2023 Mar–May",
                                            "2023_06_08" = "(d) Summer: 2023 Jun–Aug")),
                nrow=4, scales = "fixed", strip.position = "top")+
  scale_x_continuous ( breaks = seq ( 0, 24, by = 2))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        strip.text = element_text (size = 9, hjust = 0, color = "black"),
        panel.spacing = grid::unit(0.05, "lines"),
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.2),
        panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent",color = NA),
        strip.placement = "inside", 
        strip.background = element_blank())+
  labs(x = "Local time",
       y = expression(paste('\u2206',italic(p), {CO[2(T)]},' [\u00B5'*atm*']')))

#Figure8 #Processing in PowerPoint (combining legends into one)
Figure8_right=
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig8.csv"),
            aes ( x = WT_ave, y = tpCO2min)) +
    geom_point ( aes(color = season, shape = season), size = 1)+
    geom_smooth (method="glm", method.args = list(family = gaussian(link = "identity")),
                 color = "black", linewidth = 0.5)+
    stat_poly_eq(formula = y ~ x, size = 3,
                 aes(label = paste(stat(eq.label))),
                 label.x = "left", label.y = 0.1,
                 parse = TRUE)+
    stat_poly_eq(formula = y ~ x, size = 3, 
                 aes(label = paste(stat(p.value.label))),
                 label.x = "right", label.y = 0.1,
                 parse = TRUE)+
    scale_y_continuous (breaks = seq(0, 24, by = 3),
                        limits = c(0, 24))+
    scale_color_manual(values = c("darkorange1", "cornflowerblue", "limegreen", "firebrick1"),
                       labels = c("1Autumn" = "2022 Sep–Nov",
                                  "2Winter" = "Dec–2023 Feb",
                                  "3Spring" = "Mar–May",
                                  "4Summer" = "Jun–Aug"))+
    scale_shape_manual(values = c(22, 23, 24,25),
                       labels = c("1Autumn" = "2022 Sep–Nov",
                                  "2Winter" = "Dec–2023 Feb",
                                  "3Spring" = "Mar–May",
                                  "4Summer" = "Jun–Aug"))+
    theme_classic()+
    theme(axis.text = element_text (color = "black"),
          axis.text.x = element_text (size = 8),
          axis.text.y = element_text (size = 8),
          panel.grid.major.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
          panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
          panel.grid.major.y = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          panel.background = element_rect(color = "black"))+
    labs(x= expression(paste('Daily mean of ', 'WT ['^degree*C*']')),
         y = expression(paste(t[italic(p)*CO[2]*min], ' (local ', 'time)')))
  
Figure8_left =
    ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig8.csv"),
            aes ( x = sunrise2, y = tpCO2max)) +
    geom_boxplot(outlier.shape = NA, width = 0.8, staplewidth = 0.5, linewidth = 0.2) +
    geom_beeswarm(aes(color = season, shape = season), size = 1, alpha = 0.7)+
    scale_x_discrete(labels = c("09_4:30-5:00"="4:30-\n5:00",
                                "10_5:00-5:30"="5:00-\n5:30",
                                "11_5:30-6:00"="5:30-\n6:00",
                                "12_6:00-6:30"="6:00-\n6:30",
                                "13_6:30-7:00"="6:30-\n7:00",
                                "14_7:00-7:30"="7:00-\n7:30",
                                "15_7:30-8:00"="7:30-\n8:00"))+
    scale_y_continuous (breaks = seq(0, 24, by = 3),
                        limits = c(0, 24))+
    scale_color_manual(values = c("darkorange1", "cornflowerblue", "limegreen", "firebrick1"),
                       labels = c("1Autumn" = "2022 Sep–Nov",
                                  "2Winter" = "Dec–2023 Feb",
                                  "3Spring" = "Mar–May",
                                  "4Summer" = "Jun–Aug"))+
    scale_shape_manual(values = c(22, 23, 24,25),
                       labels = c("1Autumn" = "2022 Sep–Nov",
                                  "2Winter" = "Dec–2023 Feb",
                                  "3Spring" = "Mar–May",
                                  "4Summer" = "Jun–Aug"))+
    theme_classic()+
    theme(axis.text = element_text (color = "black"),
          axis.text.x = element_text (size = 8),
          axis.text.y = element_text (size = 8),
          panel.grid.major.y = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          panel.background = element_rect(color = "black"))+
    labs(x = expression(paste(Sunrise, ' (local', ' time)')),
         y = expression(paste(t[italic(p)*CO[2]*max], ' (local ', 'time)')))
  
Figure8 = (Figure8_left | plot_spacer() | Figure8_right) +
    plot_layout(widths = c(3, 0.05 , 1.8),
                guides = "collect",
                )&
    theme(legend.position = "top",
          legend.text = element_text (size = 7),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin = margin(t = 0, b = 0, l = 0),
          legend.box.margin = margin(t = 0, b = 0, l = 0),
          legend.key.width = unit(0.2, "cm"),
          legend.key.size = unit(0.3, "cm"),
          legend.spacing.x = unit(0.1, "lines"),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

dielmaxmin_data2 = filter(read_csv(file = "MIYASHITAetal2026_Fig8.csv"))
dunn.test(dielmaxmin_data2$tpCO2max, dielmaxmin_data2$sunrise, method = "holm")
dunn.test(dielmaxmin_data2$tpCO2max, dielmaxmin_data2$sunset, method = "holm")
dunn.test(dielmaxmin_data2$tpCO2min, dielmaxmin_data2$sunrise, method = "holm")
dunn.test(dielmaxmin_data2$tpCO2min, dielmaxmin_data2$sunset, method = "holm")

#Figure9 #Adding panel labels in Inkscape
labelmaxmin=as_labeller(c("suntime" = "'\u2206'*t~'[h]'",
                          "PAR_ave" = "atop('Daily mean of', 'PAR ['*\u00B5*mol~m^-2~s^-1*']')"),
                        label_parsed)

Figure9 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig9.csv"),
          aes ( x = value, y = pCO2_maxmin)) +
  geom_point ( aes(color = season, shape = season), size = 1)+
  geom_smooth (method="glm", method.args = list(family = Gamma(link = "log")),
               color = "black", linewidth = 0.5)+
  stat_poly_eq(formula = y ~ x, size = 2.5, 
               aes(label = paste(stat(p.value.label))),
               label.x = "left", label.y = 0.86,
               parse = TRUE)+
  scale_color_manual(values = c("darkorange1", "cornflowerblue", "limegreen", "firebrick1"),
                     labels = c("1Autumn" = "2022 Sep–Nov",
                                "2Winter" = "Dec–2023 Feb",
                                "3Spring" = "Mar–May",
                                "4Summer" = "Jun–Aug"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1Autumn" = "2022 Sep–Nov",
                                "2Winter" = "Dec–2023 Feb",
                                "3Spring" = "Mar–May",
                                "4Summer" = "Jun–Aug"))+
  facet_wrap (vars(variable), scales = "free_x",
              strip.position = "bottom",
               labeller = labelmaxmin)+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.text = element_text (size = 7),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t = 0, b = 0, l = -10),     
        legend.box.margin = margin(t = 0, b = 0, l = -10),  
        legend.key.width = unit(0.2, "cm"),  
        legend.key.size = unit(0.3, "cm"),  
        legend.spacing.x = unit(0, "lines"),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        panel.background = element_rect(color = "black"))+
  labs(x = element_blank(),
       y = expression(paste('\u2206', italic(p), {CO[2]}, ' [\u00B5'*atm*']')))

#Results #3.4
dieldata = read_csv(file = "MIYASHITAetal2026_diel_3month.csv")
dieldata_spring1 = filter(dieldata, year_month == "2023_03_05", 8<time&time<15)
cor.test(dieldata_spring1$WT, dieldata_spring1$dpCO2T)
dieldata_spring2 = filter(dieldata, year_month == "2023_03_05", 17<time|time<7.5)
cor.test(dieldata_spring2$WT, dieldata_spring2$dpCO2T)
dieldata_summer1 = filter(dieldata, year_month == "2023_06_08", 6.5<time&time<15.5)
cor.test(dieldata_summer1$WT, dieldata_summer1$dpCO2T)
dieldata_summer2 = filter(dieldata, year_month == "2023_06_08", 17<time|time<5.5)
cor.test(dieldata_summer2$WT, dieldata_summer2$dpCO2T)

#Table2 #Autumn
glm_diel1 = glm(dpCO2T_obs ~ WT+PAR, family=gaussian(link="identity"),
                data = filter(diel_data, year_month == "2022_09_11"))
vif (glm_diel1)
summary.glm(glm_diel1)
glm_diel1_2 = stepAIC(glm_diel1)
summary.glm(glm_diel1_2)

#Table2 #Winter
glm_diel2 = glm(dpCO2T_obs ~ WT+PAR, family=gaussian(link="identity"),
                data = filter(diel_data, year_month == "2022_12_2023_02"))
vif(glm_diel2)
summary.glm(glm_diel2)
glm_diel2_2 = stepAIC(glm_diel2)
summary.glm(glm_diel2_2)

#Table2 #Spring
glm_diel3 = glm(dpCO2T_obs ~ WT+PAR, family=gaussian(link="identity"),
                data = filter(diel_data, year_month == "2023_03_05"))
vif(glm_diel3)
summary.glm(glm_diel3)
glm_diel3_2 = stepAIC(glm_diel3)
summary.glm(glm_diel3_2)

#Table2 #Summer
glm_diel4 = glm(dpCO2T_obs ~ WT+PAR, family=gaussian(link="identity"),
                data = filter(diel_data, year_month == "2023_06_08"))
vif(glm_diel4)
summary.glm(glm_diel4)
glm_diel4_2 = stepAIC(glm_diel4)
summary.glm(glm_diel4_2)

#Table3
glm_pCO2maxmin = glm(pCO2_maxmin ~ WT_ave + PAR_ave + WT_maxmin + sunsetsunrise,
                     family = Gamma(link = "log"), data = dielmaxmin_data2)
vif (glm_pCO2maxmin)
summary.glm(glm_pCO2maxmin)
glm_pCO2maxmin_2 = stepAIC(glm_pCO2maxmin)
summary.glm(glm_pCO2maxmin_2)

#FigureS3
FigureS3 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_FigS3.csv"),
          aes(as.character(interval), 0))+
  geom_errorbar ( aes ( ymax = max*100, ymin = min*100),
                  linewidth = 0.4, width = 0.3)+
  scale_x_discrete(limit = c("1", "2", "3", "4", "6", "8", "12", "24"))+
  facet_wrap ( vars(year_month), dir = "v",
               labeller = as_labeller ( c( "2022_09_11" = "2022 Sep–Nov (Autumn)",
                                           "2022_12_2023_02" = "2022 Dec–2023 Feb (Winter)",
                                           "2023_03_05" = "2023 Mar–May (Spring)",
                                           "2023_06_08" = "2023 Jun–Aug (Summer)")),
               nrow=4, scales = "free_y", strip.position = "top")+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 9),
        axis.text.y = element_text (size = 9),
        axis.title.x = element_text (size = 10),
        axis.title.y = element_text (size = 10),
        strip.text = element_text (size = 9, hjust = 0),
        panel.spacing = grid::unit(0.05, "lines"),
        panel.background = element_rect(color = "black"),
        panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5, linetype = "dashed"),
        strip.placement = "inside", 
        strip.background = element_blank())+
  labs(x = "Measurements interval [h]",
       y = "Error [%]")