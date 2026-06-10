library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)
library(ggh4x)
library(ggbeeswarm)
library(ggpmisc)
library(MASS)
library(car)
library(performance)
library(dunn.test)
library(scales)

data_3month = read_csv("MIYASHITAetal2026_3month.csv")

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
       y = NULL)

#Figure3 #pCO2 #Chla #DOC #Add significance labels with Inkscape
parameter_Figure3 =
  as_labeller (c("0pCO2" = "atop(italic(p)*CO[2], '['*\u00B5*atm*']')",
                 "1Chla" = "atop('Chl-'*italic(a),'['*\u00B5*g~L^-1*']')",
                 "2DOC" = "atop('DOC', '[mg'*~L^-1*']')"),label_parsed)
pCO2_Chla_DOC_monthly =
  ggplot (data = read.csv(file = "MIYASHITAetal2026_Fig3.csv"),
          aes (year_month, average)) +
  geom_hline(data = data.frame(parameter = c("0pCO2","0pCO2","0pCO2","0pCO2")),
             aes(yintercept = c(1000,2000,3000,4000)),
             color = "gray90", linewidth = 0.5, linetype = "11")+
  geom_hline(data = data.frame(parameter = c("1Chla","1Chla","1Chla")),
             aes(yintercept = c(5,10,15)),
             color = "gray90", linewidth = 0.5, linetype = "11")+
  geom_hline(data = data.frame(parameter = c("2DOC","2DOC","2DOC")),
             aes(yintercept = c(3.0, 3.2, 3.4)),
             color = "gray90", linewidth = 0.5, linetype = "11")+
  geom_path(group = 1, linewidth = 0.3, color = "black")+
  geom_errorbar(group = 1, width = 0.3, linewidth =0.3,
                aes ( ymax = average + 1.96 * se, 
                      ymin = average - 1.96 * se),
                color = "black")+
  scale_x_discrete(labels = c("2022_09" = "9\n2022",
                              "2022_10" = "10", "2022_11" = "11",
                              "2022_12" = "12", "2023_01" = "1\n2023",
                              "2023_02" = "2", "2023_03" = "3",
                              "2023_04" = "4", "2023_05" = "5",
                              "2023_06" = "6", "2023_07" = "7",
                              "2023_08" = "8", "2023_09" = "9"))+
  facet_wrap (vars(parameter), scales = "free_y",  strip.position = "left", ncol = 1,
              labeller = parameter_Figure3)+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8.1),
        axis.text.y = element_text (size = 8.1),
        axis.title.x = element_text (size = 10, color = "black"),
        strip.text = element_text(size = 9, lineheight = 0.01, color = "black"),
        panel.background = element_rect(color = "black"),
        panel.spacing.y = grid::unit(0.5,"lines"),
        strip.placement = "outside", strip.background = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"))+
  labs(x = "Month", y = NULL)
Figure3  = pCO2_Chla_DOC_monthly +
  facetted_pos_scales(
    y = list(parameter == "0pCO2" 
             ~ scale_y_continuous(limits = c(400, 4000),
                                  breaks = c(400, 1000, 2000, 3000, 4000),
                                  expand = expansion(mult = c(0, .1))),
             parameter == "1Chla" 
             ~ scale_y_continuous(limits = c(0, 18),
                                  expand = expansion(mult = c(0, .1)))))

#Figure3 #Statistics #Multiple comparisons #pCO2
pCO2monthlydata = read_csv(file = "MIYASHITAetal2026_Fig3_stat1.csv")
friedman.test(pCO2 ~ month | time, data = pCO2monthlydata)
pairwise.wilcox.test(pCO2monthlydata$pCO2,
                     pCO2monthlydata$month,
                     paired = TRUE, p.adjust.method = "holm")

#Figure3 #Statistics #Multiple comparisons #Chla #DOC
ChlaDOCdata = read_csv(file = "MIYASHITAetal2026_Fig3_stat2.csv")
kruskal.test(ChlaDOCdata$Chla ~ ChlaDOCdata$month, data = ChlaDOCdata)
dunn.test(ChlaDOCdata$Chla, ChlaDOCdata$month, method = "holm")
kruskal.test(ChlaDOCdata$DOC ~ ChlaDOCdata$month, data = ChlaDOCdata)
dunn.test(ChlaDOCdata$DOC, ChlaDOCdata$month, method = "holm")

#Figure4 #pCO2 #Tw #PAR
facet_labs_3month = c("2022_09_11" = "(a) Autumn: 2022 Sep–Nov",
                      "2022_12_2023_02" = "(b) Winter: 2022 Dec–2023 Feb",
                      "2023_03_05" = "(c) Spring: 2023 Mar–May",
                      "2023_06_08" = "(d) Summer: 2023 Jun–Aug")
data_3month_2 = data_3month %>% 
  mutate( Tw_scaled = case_when( year_month == "2022_09_11" ~ 
                                   scales::rescale(Tw,
                                                   to = c(250, 1750),
                                                   from = c(20.1, 24.1)),
                                 year_month == "2022_12_2023_02" ~ 
                                   scales::rescale(Tw, 
                                                   to = c(250, 1750), 
                                                   from = c(4.7,8.7)), 
                                 year_month == "2023_03_05" ~ 
                                   scales::rescale(Tw, 
                                                   to = c(500, 3500), 
                                                   from = c(16.3, 20.3)), 
                                 year_month == "2023_06_08" ~ 
                                   scales::rescale(Tw, to = c(500, 3500), 
                                                   from = c(27.5, 31.5)) ), 
          Twmin_scaled = case_when( year_month == "2022_09_11" ~ 
                                      scales::rescale(Tw - 1.96*Tw_SE, 
                                                      to = c(250, 1750), 
                                                      from = c(20.1, 24.1)), 
                                    year_month == "2022_12_2023_02" ~ 
                                      scales::rescale(Tw - 1.96*Tw_SE, 
                                                      to = c(250, 1750), 
                                                      from = c(4.7,8.7)), 
                                    year_month == "2023_03_05" ~ 
                                      scales::rescale(Tw - 1.96*Tw_SE, 
                                                      to = c(500, 3500), 
                                                      from = c(16.3, 20.3)), 
                                    year_month == "2023_06_08" ~ 
                                      scales::rescale(Tw - 1.96*Tw_SE, 
                                                      to = c(500, 3500), 
                                                      from = c(27.5, 31.5)) ), 
          Twmax_scaled = case_when( year_month == "2022_09_11" ~ 
                                      scales::rescale(Tw + 1.96*Tw_SE, 
                                                      to = c(250, 1750), 
                                                      from = c(20.1, 24.1)), 
                                    year_month == "2022_12_2023_02" ~
                                      scales::rescale(Tw + 1.96*Tw_SE, 
                                                      to = c(250, 1750), 
                                                      from = c(4.7,8.7)), 
                                    year_month == "2023_03_05" ~ 
                                      scales::rescale(Tw + 1.96*Tw_SE, 
                                                      to = c(500, 3500), 
                                                      from = c(16.3, 20.3)), 
                                    year_month == "2023_06_08" ~ 
                                      scales::rescale(Tw + 1.96*Tw_SE, 
                                                      to = c(500, 3500), 
                                                      from = c(27.5, 31.5)) ) ) 
Figure4 = ggplot(data_3month_2, aes(time)) + 
  geom_ribbon( aes(ymin = Twmin_scaled, 
                   ymax = Twmax_scaled), 
               fill = "orangered", alpha = 0.2 ) +
  geom_line( aes(y = Tw_scaled), linewidth = 0.4,
             color = "orangered", linetype = "dashed" ) + 
  geom_linerange( aes( ymin = pCO2_ave - 1.96*pCO2_SE, 
                       ymax = pCO2_ave + 1.96*pCO2_SE ),
                  linewidth = 0.3 ) +
  geom_point( aes(y = pCO2_ave, fill = PAR), 
              shape = 23, size = 2, stroke = 0.25 ) + 
  scale_fill_viridis_c(option = "plasma") + 
  facet_wrap2( vars(year_month), 
               dir = "v", axes = "all", 
               remove_labels = "x", 
               labeller = as_labeller(facet_labs_3month), 
               nrow = 4, scales = "free_y", strip.position = "top" ) + 
  scale_x_continuous( breaks = seq(0, 24, by = 2), 
                      limits = c(0, 24) ) + 
  facetted_pos_scales(
    y = list(year_month == "2022_09_11" ~
               scale_y_continuous(
                 limits = c(250, 1750),
                 breaks = seq(500, 1500, 500),
                 sec.axis = sec_axis(
                   trans = ~ scales::rescale(.,
                                             to = c(20.1,24.1),
                                             from = c(250,1750)),
                   breaks = seq(20,24,1),
                   name = expression(T[w]*" [°C]"))),
             year_month == "2022_12_2023_02" ~
               scale_y_continuous(
                 limits = c(250, 1750),
                 breaks = seq(500, 1500, 500),
                 sec.axis = sec_axis(
                   trans = ~ scales::rescale(.,
                                             to = c(4.7,8.7),
                                             from = c(250,1750)),
                   breaks = seq(5,9,1),
                   name = expression(T[w]*" [°C]"))),
             year_month == "2023_03_05" ~
               scale_y_continuous(
                 limits = c(500, 3500),
                 breaks = seq(1000, 3000, 1000),
                 sec.axis = sec_axis(
                   trans = ~ scales::rescale(.,
                                             to = c(16.3,20.3),
                                             from = c(500, 3500)),
                   breaks = seq(17,20,1),
                   name = expression(T[w]*" [°C]"))),
             year_month == "2023_06_08" ~
               scale_y_continuous(
                 limits = c(500, 3500),
                 breaks = seq(1000, 3000, 1000),
                 sec.axis = sec_axis(
                   trans = ~ scales::rescale(.,
                                             to = c(27.5,31.5),
                                             from = c(500,3500)),
                   breaks = seq(28,32,1),
                   name = expression(T[w]*" [°C]")))))+
  theme_cowplot() + 
  theme( axis.text = element_text(color = "black"), 
         axis.text.x = element_text(size = 9), 
         axis.text.y = element_text(size = 9), 
         axis.text.y.right = element_text(color = "orangered", size = 9),
         axis.title.y.right = element_text(color = "orangered", size = 10),
         axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10), 
         axis.line.y.right = element_line(color = "orangered"),
         axis.ticks.y.right = element_line(color = "orangered"),
         strip.text = element_text(size = 9, hjust = 0), 
         panel.spacing = unit(0.05, "lines"), 
         strip.placement = "inside", 
         strip.background = element_blank(), 
         legend.text = element_text (size = 9),
         legend.box = element_blank(),
         legend.title = element_text (size = 9, margin = margin (r = 10)),
         legend.key.height = unit (0.3, "cm"),
         legend.key.width = unit (0.8, "cm"),
         legend.position = "bottom",
         legend.box.spacing = unit(0.2, "cm"),
         panel.grid.major.x = element_line(color = "gray80", linewidth = 0.15),
         panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.15, linetype = "dashed")) + 
  labs( x = "Local time", 
        y = expression( paste( italic(p), CO[2], ' [\u00B5'*atm*']' ) ), 
        fill = expression(atop('PAR ', '[\u00B5'*mol~m^-2~s^-1*']')))

#Figure5(Aa-Ac,Ba-Bc,Ca-Cc,Da-Dc) #ΔpCO2(T) #PAR #Day(PAR>0)
Figure5_1 =
  ggplot (data = filter(data_3month, !(timeblocks == "4_night")),
          aes ( x = PAR, y = dpCO2T_obs)) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = 1, color = "gray") +
  geom_path(linewidth = 0.3, color = "gray30", linetype = "dotted")+
  geom_point (aes(color= dTw, shape = dTw), ,size = 1, alpha = 0.8)+
  scale_color_manual(values = c("cool" = "#4400ff", "heat" = "#ff4500"),
                     labels = c("cool" = "Cooling", "heat" = "Heating"))+
  scale_shape_manual(values = c("cool" = 25, "heat" = 24),
                     labels = c("cool" = "Cooling", "heat" = "Heating"))+
  facet_grid ( timeblocks~year_month,
               labeller = as_labeller ( c("2022_09_11" = "Autumn (21.1–23.2 °C)",
                                          "2022_12_2023_02" = "Winter (5.9–7.7 °C)",
                                          "2023_03_05" = "Spring (17.2–19.2 °C)",
                                          "2023_06_08" = "Summer (28.6–30.6 °C)",
                                          "1_morning" = "Early morning",
                                          "2_day" = "Day",
                                          "3_afternoon" = "Afternoon")))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        axis.title.x = element_text (size = 9),
        axis.title.y = element_text (size = 9),
        legend.text = element_text (size = 9, hjust = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(color = "black",
                                         linewidth = 0.3),
        legend.margin = margin(0,10,0,10),
        legend.position = c(1, -0.065),
        legend.justification = c("right", "top"),
        legend.direction = "horizontal",
        panel.spacing.x = grid::unit(0.3, "lines"),
        panel.spacing.y = grid::unit(0.3,"lines"),
        panel.background = element_rect(color = "black"),
        axis.line = element_line(color = "black"),
        strip.placement = "inside",
        strip.background = element_blank())+
  labs(x = expression(paste('PAR', ' [\u00B5'*mol~m^-2~s^-1*']')),
       y = expression(paste('\u2206',italic(p), {CO[2(T)]},' [\u00B5'*atm*']')))

#Figure5(Ad,Bd,Cd,Dd) #ΔpCO2(T) #PAR #Night(PAR=0)
Figure5_2 =
  ggplot (data = filter(data_3month, timeblocks == "4_night"),
          aes ( x = Tw, y = dpCO2T_obs)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = 1, color = "gray") +
  geom_path(linewidth = 0.3, color = "gray30", linetype = "dotted")+
  geom_point (color = "#4400ff", shape = 25 ,size = 1, alpha = 0.8 )+
  facet_grid (timeblocks ~ year_month, scales = "free_x",
              labeller = as_labeller(c(
                "2022_09_11" = "",
                "2022_12_2023_02" = "",
                "2023_03_05" = "",
                "2023_06_08" = "",
                "4_night" = "Night")))+
  facetted_pos_scales(x = list(
    year_month == "2022_09_11" ~
      scale_x_continuous(limits = c(21.05, 22.35),
                         breaks = seq(21.1, 22.3, 0.3)),
    year_month == "2022_12_2023_02" ~
      scale_x_continuous(
        limits = c(5.75, 7.05),
        breaks = seq(5.8, 7.0, 0.3)),
    year_month == "2023_03_05" ~
      scale_x_continuous(
        limits = c(17.2, 18.5),
        breaks = seq(17.2, 18.4, 0.3)),
    year_month == "2023_06_08" ~
      scale_x_continuous(
        limits = c(28.65, 29.95),
        breaks = seq(28.7, 29.9, 0.3))))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        axis.title.x = element_text (size = 9),
        axis.title.y = element_text (size = 9),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = grid::unit(0.3, "lines"),
        panel.spacing.y = grid::unit(0.3,"lines"),
        panel.background = element_rect(color = "black"),
        axis.line = element_line(color = "black"))+
  labs(x = expression(paste({T[w]}, ' [°C]')),
       y = expression(paste('\u2206', italic(p),{CO[2(T)]}, ' [\u00B5'*atm*']')))

#Figure5
Figure5 =
  Figure5_1 / Figure5_2 +
  plot_layout(heights = c(3, 1))  

#Figure6 #Processing (coloring) in PowerPoint
Figure6 =
  ggplot (data = data_3month, aes (x = time)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = 1, color = "gray") +
  geom_linerange ( aes ( ymax = dpCO2T_obs + 1.96 * dpCO2T_obs_se, 
                         ymin = dpCO2T_obs - 1.96 * dpCO2T_obs_se),
                   linewidth = 0.5)+
  geom_point ( aes(y = dpCO2T_obs), shape = 23, size = 1, stroke = 0.5,
               fill = "white", color = "black")+
  geom_path ( aes(y = dpCO2T_model), linewidth = 0.5,
              linetype = "solid", color = "red", alpha = 0.75 )+
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

#Figure7a
Figure7a =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig7.csv"),
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
                     labels = c("1Autumn" = "Autumn: 2022 Sep–Nov",
                                "2Winter" = "Winter: 2022 Dec–2023 Feb",
                                "3Spring" = "Spring: 2023 Mar–May",
                                "4Summer" = "Summer: 2023 Jun–Aug"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1Autumn" = "Autumn: 2022 Sep–Nov",
                                "2Winter" = "Winter: 2022 Dec–2023 Feb",
                                "3Spring" = "Spring: 2023 Mar–May",
                                "4Summer" = "Summer: 2023 Jun–Aug"))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        panel.grid.major.y = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        plot.margin = unit(c(0.6, 0.1, 0, 0.1), "cm"),
        panel.background = element_rect(color = "black"),
        legend.position = c(0.825,1.07),
        legend.direction = "horizontal",
        legend.text = element_text (size = 8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t = 0, b = 0, l = 0),
        legend.box.margin = margin(t = 0, b = 0, l = 0),
        legend.key.width = unit(0.2, "cm"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.1, "lines"))+
  labs(x = expression(paste(t[sunrise], ' (local', ' time)')),
       y = expression(paste(t[italic(p)*CO[2]*max], ' (local ', 'time)')))

#Figure7a #Mutiple comparison
data_maxmin = filter(read_csv(file = "MIYASHITAetal2026_Fig7_Table3.csv"))
dunn.test(data_maxmin$tpCO2max, data_maxmin$sunrise, method = "holm")
dunn.test(data_maxmin$tpCO2max, data_maxmin$sunset, method = "holm")
dunn.test(data_maxmin$tpCO2min, data_maxmin$sunrise, method = "holm")
dunn.test(data_maxmin$tpCO2min, data_maxmin$sunset, method = "holm")

#Figure7b
Figure7b =
  ggplot (data = filter(read_csv(file = "MIYASHITAetal2026_Fig7.csv")),
          aes ( x = Tw_ave, y = tpCO2min)) +
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
                     labels = c("1Autumn" = "Autumn: 2022 Sep–Nov",
                                "2Winter" = "Winter: 2022 Dec–2023 Feb",
                                "3Spring" = "Spring: 2023 Mar–May",
                                "4Summer" = "Summer: 2023 Jun–Aug"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1Autumn" = "Autumn: 2022 Sep–Nov",
                                "2Winter" = "Winter: 2022 Dec–2023 Feb",
                                "3Spring" = "Spring: 2023 Mar–May",
                                "4Summer" = "Summer: 2023 Jun–Aug"))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        panel.grid.major.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
        legend.position = "none",
        plot.margin = unit(c(0.6, 0.1, 0, 0.1), "cm"),
        panel.background = element_rect(color = "black"))+
  labs(x= expression(paste('Daily mean ', T[w], ' [°C]')),
       y = expression(paste(t[italic(p)*CO[2]*min], ' (local ', 'time)')))

#Figure7 #Adding panel labels and p-values with PowerPoint
Figure7 = (Figure7a| plot_spacer() | Figure7b) +
  plot_layout(widths = c(3, 0.01 , 1.8))

#Figure8 #Adding panel labels with Inkscape
label_Figure8 =as_labeller(c("suntime" = "'\u2206'*t~'[h]'",
                          "PAR_ave" = "'Daily mean PAR ['*\u00B5*mol~m^-2~s^-1*']'"),
                        label_parsed)

Figure8=
  ggplot (data = read_csv(file = "MIYASHITAetal2026_Fig8.csv"),
          aes ( x = value, y = pCO2_maxmin)) +
  geom_point ( aes(color = season, shape = season), size = 1)+
  geom_smooth (method="glm", method.args = list(family = Gamma(link = "log")),
               color = "black", linewidth = 0.5)+
  stat_poly_eq(formula = y ~ x, size = 2.5, 
               aes(label = paste(stat(p.value.label))),
               label.x = "left", label.y = 0.86,
               parse = TRUE)+
  scale_color_manual(values = c("darkorange1", "cornflowerblue", "limegreen", "firebrick1"),
                     labels = c("1Autumn" = "Autumn",
                                "2Winter" = "Winter",
                                "3Spring" = "Spring",
                                "4Summer" = "Summer"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1Autumn" = "Autumn",
                                "2Winter" = "Winter",
                                "3Spring" = "Spring",
                                "4Summer" = "Summer"))+
  facet_wrap (vars(variable), scales = "free_x",
              strip.position = "bottom",
              labeller = label_Figure8)+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, color = "black"),
        strip.text = element_text(size = 7.5, color = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = c(0.5,1.08),
        legend.direction = "horizontal",
        legend.text = element_text (size = 8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t = 0, b = 0, l = -10),
        legend.box.margin = margin(t = 0, b = 0, l = -10),
        legend.key.width = unit(0.2, "cm"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0, "lines"),
        plot.margin = unit(c(0.7, 0.1, 0, 0.1), "cm"),
        panel.background = element_rect(color = "black"))+
  labs(y = expression(paste('\u2206', italic(p), {CO[2][' max−min']}, ' [\u00B5'*atm*']')))

#Table2 #TableS1 #Autumn
glm_diel_autumn = glm(dpCO2T_obs ~ Tw+PAR, family=gaussian(link="identity"),
                   data = filter(data_3month, year_month == "2022_09_11"))
vif (glm_diel_autumn)
summary.glm(glm_diel_autumn)
glm_diel_autumn_2 = stepAIC(glm_diel_autumn)
summary.glm(glm_diel_autumn_2)
glm_diel_autumn_PAR = glm(dpCO2T_obs ~ PAR, family=gaussian(link="identity"),
                       data = filter(data_3month, year_month == "2022_09_11"))
summary.glm(glm_diel_autumn_PAR)
glm_diel_autumn_Tw = glm(dpCO2T_obs ~ Tw, family=gaussian(link="identity"),
                      data = filter(data_3month, year_month == "2022_09_11"))
summary.glm(glm_diel_autumn_Tw)
r2(glm_diel_autumn)$R2
r2(glm_diel_autumn_PAR)$R2
r2(glm_diel_autumn_Tw)$R2

#Table2 #TableS1 #Winter
glm_diel_winter = glm(dpCO2T_obs ~ Tw+PAR, family=gaussian(link="identity"),
                   data = filter(data_3month, year_month == "2022_12_2023_02"))
vif(glm_diel_winter)
summary.glm(glm_diel_winter)
glm_diel_winter_2 = stepAIC(glm_diel_winter)
summary.glm(glm_diel_winter_2)
glm_diel_winter_PAR = glm(dpCO2T_obs ~ PAR, family=gaussian(link="identity"),
                       data = filter(data_3month, year_month == "2022_12_2023_02"))
glm_diel_winter_Tw = glm(dpCO2T_obs ~ Tw, family=gaussian(link="identity"),
                      data = filter(data_3month, year_month == "2022_12_2023_02"))
r2(glm_diel_winter)$R2
r2(glm_diel_winter_PAR)$R2
r2(glm_diel_winter_Tw)$R2

#Table2 #TableS1 #Spring
glm_diel_spring = glm(dpCO2T_obs ~ Tw+PAR, family=gaussian(link="identity"),
                   data = filter(data_3month, year_month == "2023_03_05"))
vif(glm_diel_spring)
summary.glm(glm_diel_spring)
glm_diel_spring_2 = stepAIC(glm_diel_spring)
summary.glm(glm_diel_spring_2)
glm_diel_spring_PAR = glm(dpCO2T_obs ~ PAR, family=gaussian(link="identity"),
                       data = filter(data_3month, year_month == "2023_03_05"))
glm_diel_spring_Tw = glm(dpCO2T_obs ~ Tw, family=gaussian(link="identity"),
                      data = filter(data_3month, year_month == "2023_03_05"))
r2(glm_diel_spring)$R2
r2(glm_diel_spring_PAR)$R2
r2(glm_diel_spring_Tw)$R2

#Table2 #TableS1 #Summer
glm_diel_summer = glm(dpCO2T_obs ~ Tw+PAR, family=gaussian(link="identity"),
                   data = filter(data_3month, year_month == "2023_06_08"))
vif(glm_diel_summer)
summary.glm(glm_diel_summer)
glm_diel_summer_2 = stepAIC(glm_diel_summer)
summary.glm(glm_diel_summer_2)
glm_diel_summer_PAR = glm(dpCO2T_obs ~ PAR, family=gaussian(link="identity"),
                       data = filter(data_3month, year_month == "2023_06_08"))
glm_diel_summer_Tw = glm(dpCO2T_obs ~ Tw, family=gaussian(link="identity"),
                      data = filter(data_3month, year_month == "2023_06_08"))
r2(glm_diel_summer)$R2
r2(glm_diel_summer_PAR)$R2
r2(glm_diel_summer_Tw)$R2

#Table3
glm_pCO2maxmin = glm(pCO2_maxmin ~ Tw_ave + PAR_ave + Tw_maxmin + sunsetsunrise,
                     family = Gamma(link = "log"), data = data_maxmin)
vif (glm_pCO2maxmin)
summary.glm(glm_pCO2maxmin)
glm_pCO2maxmin_2 = stepAIC(glm_pCO2maxmin)
summary.glm(glm_pCO2maxmin_2)
r2(glm_pCO2maxmin_2)

#FigureS4
FigureS3 =
  ggplot (data = read_csv(file = "MIYASHITAetal2026_FigS4.csv"),
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

#FigureS5
FigureS5 =
  ggplot (data = data_3month, aes ( x = dpCO2T_obs, y = dpCO2_ave)) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype = 1, color = "gray") +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype = 1, color = "gray") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.25, linetype ="dashed", color = "black")+
  geom_point (aes(color= dTw, shape = dTw), size = 1, alpha = 0.7)+
  geom_smooth (method="lm", formula= y~x, color = "#00ff04", linewidth = 0.25)+
  stat_poly_eq(formula = y ~ x, size = 3,
               aes(label = paste(stat(eq.label))),
               label.x = "right", label.y = "bottom",
               parse = TRUE)+
  scale_color_manual(values = c("cool" = "#4400ff", "heat" = "#ff4500"),
                     labels = c("cool" = "Cooling", "heat" = "Heating"))+
  scale_shape_manual(values = c("cool" = 25, "heat" = 24),
                     labels = c("cool" = "Cooling", "heat" = "Heating"))+
  facet_wrap ( vars(year_month), ncol = 2,
               strip.position = "top",
               labeller = as_labeller ( c("2022_09_11" = "Autumn",
                                          "2022_12_2023_02" = "Winter",
                                          "2023_03_05" = "Spring",
                                          "2023_06_08" = "Summer")))+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        axis.title.x = element_text (size = 9),
        axis.title.y = element_text (size = 9),
        legend.text = element_text (size = 9, hjust = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(0,10,0,10),
        legend.position = c(0.5, 1.125),
        legend.direction = "horizontal",
        panel.spacing.x = grid::unit(0.3, "lines"),
        panel.spacing.y = grid::unit(0.3,"lines"),
        panel.background = element_rect(color = "black"),
        plot.margin = unit(c(1, 0.1, 0.1, 0.1), "cm"),
        axis.line = element_line(color = "black"),
        strip.text = element_text (color = "black"),
        strip.placement = "inside",
        strip.background = element_blank())+
  labs(x = expression(paste('\u2206',italic(p), {CO[2(T)]},' [\u00B5'*atm*']')),
       y = expression(paste('\u2206',italic(p), {CO[2]},' [\u00B5'*atm*']')))

#FigureS6
FigureS6 =
  ggplot (data = data_3month, aes (x = time)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = 1, color = "gray") +
  geom_linerange ( aes ( ymax = dpCO2T_obs + 1.96 * dpCO2T_obs_se, 
                         ymin = dpCO2T_obs - 1.96 * dpCO2T_obs_se),
                   linewidth = 0.5)+
  geom_point ( aes(y = dpCO2T_obs), shape = 23, size = 1, stroke = 0.5,
               fill = "white", color = "black")+
  geom_path ( aes(y = dpCO2T_modelPAR), linewidth = 0.5, linetype = "solid", color = "#00ff44", alpha = 0.95 )+
  geom_path ( aes(y = dpCO2T_modelTw), linewidth = 0.5, linetype = "dashed", color = "#4400ff", alpha = 0.95 )+
  geom_path ( aes(y = dpCO2T_modelboth), linewidth = 0.5, linetype = "dotdash", color = "#ff4500", alpha = 0.95 )+
  facet_wrap2 ( vars(year_month), dir = "v", axes = "all", remove_labels = "x",
                labeller = as_labeller ( c( "2022_09_11" = "Autumn: 2022 Sep–Nov",
                                            "2022_12_2023_02" = "Winter: 2022 Dec–2023 Feb",
                                            "2023_03_05" = "Spring: 2023 Mar–May",
                                            "2023_06_08" = "Summer: 2023 Jun–Aug")),
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
        strip.placement = "inside", 
        strip.background = element_blank())+
  labs(x = "Local time",
       y = expression(paste('\u2206',italic(p), {CO[2(T)]},' [\u00B5'*atm*']')))

#FigureS7
FigureS7 =
  ggplot (data = data_3month, aes (x = dpCO2T_obs, y = dpCO2T_model)) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype = 1, color = "gray") +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype = 1, color = "gray") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.25, linetype ="dashed", color = "black")+
  geom_point ( aes(color = timeblocks, shape = timeblocks), size = 1)+
  scale_color_manual(values = c("#00cccc", "#cccc00", "#cc0000", "#0000cd"),
                     labels = c("1_morning" = "Early morning",
                                "2_day" = "Day",
                                "3_afternoon" = "Afternoon",
                                "4_night" = "Night"))+
  scale_shape_manual(values = c(22, 23, 24,25),
                     labels = c("1_morning" = "Early morning",
                                "2_day" = "Day",
                                "3_afternoon" = "Afternoon",
                                "4_night" = "Night"))+
  geom_smooth (method="lm", formula= y~x, color = "gray50", linewidth = 0.25)+
  stat_poly_eq(formula = y ~ x, size = 2.5,
               aes(label = paste(after_stat(eq.label))),
               label.x = "right", label.y = "bottom",
               parse = TRUE)+
  facet_wrap ( ~year_month, ncol = 2,
               labeller = as_labeller ( c( "2022_09_11" = "Autumn",
                                           "2022_12_2023_02" = "Winter",
                                           "2023_03_05" = "Spring",
                                           "2023_06_08" = "Summer")),
               strip.position = "top")+
  theme_classic()+
  theme(axis.text = element_text (color = "black"),
        axis.text.x = element_text (size = 8),
        axis.text.y = element_text (size = 8),
        axis.title.x = element_text (size = 9),
        axis.title.y = element_text (size = 9),
        legend.text = element_text (size = 8, hjust = 0.5),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(0, 10, 0, 10),
        legend.position = c(0.4, 1.125),
        legend.direction = "horizontal",
        panel.spacing.x = grid::unit(0.3, "lines"),
        panel.spacing.y = grid::unit(0.3,"lines"),
        panel.background = element_rect(color = "black"),
        plot.margin = unit(c(1, 0.1, 0.1, 0.1), "cm"),
        axis.line = element_line(color = "black"),
        strip.text = element_text (color = "black"),
        strip.placement = "inside",
        strip.background = element_blank())+
  labs(x = expression(paste('\u2206',italic(p), {CO[2(T)]["modeled"]},' [\u00B5'*atm*']')),
       y = expression(paste('\u2206',italic(p), {CO[2(T)]["observed"]},' [\u00B5'*atm*']')))