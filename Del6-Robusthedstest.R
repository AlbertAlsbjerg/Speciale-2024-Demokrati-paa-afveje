#----------------------------------------------------------
# ROBUSTHEDSTEST AF ANTAGELSER
#----------------------------------------------------------

# Funktion til at rette i attribut-navne, så lange labels i figuren ser gode ud
nye_labels <- function (m) {
  result <- m %>% 
    mutate(
      feature = case_when(
        feature == "Holdning til dagpenge" 
        ~ "Holdning til\ndagpenge",
        feature == "Holdning til klima" 
        ~ "Holdning til\nklima",
        feature == "Holdning til udlændinge" 
        ~ "Holdning til\nudlændinge",
        feature == "Udpegning af dommere" 
        ~ "Udpegning af\ndommere",
        feature == "Politikeres brud med loven" 
        ~ "Politikeres brud\nmed loven",
        .default = feature),
      
      level = case_when(
        level == "Hverken rød eller blå blok"
        ~ "Hverken rød\neller blå blok",
        level == "Hverken enig eller uenig"
        ~ "Hverken enig\neller uenig",
        level == " Hverken enig eller uenig"
        ~ " Hverken enig \neller uenig",
        level == "  Hverken enig eller uenig"
        ~ "  Hverken enig\n. eller uenig",
        .default = level)
    )
  
  return(result)
}

# Balancetest af randomiseringen

# Køn
m_b1 <- mm(D_cj,
           kvinde ~ cj_alder + cj_koen + cj_elite + cj_blok
           + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
           + cj_illiberal_medier + cj_illiberal_dommere 
           + cj_illiberal_inger_mette,
           id = ~ ResponseId,
           estimate = "mm",
           h0 = mean(D_cj$kvinde))

# Ideologi
m_b2 <- mm(D_cj,
           ideologi ~ cj_alder + cj_koen + cj_elite + cj_blok
           + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
           + cj_illiberal_medier + cj_illiberal_dommere 
           + cj_illiberal_inger_mette,
           id = ~ ResponseId,
           estimate = "mm",
           h0 = mean(D_cj$ideologi))

# Alder
m_b3 <- mm(D_cj,
           aarstal ~ cj_alder + cj_koen + cj_elite + cj_blok
           + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
           + cj_illiberal_medier + cj_illiberal_dommere 
           + cj_illiberal_inger_mette,
           id = ~ ResponseId,
           estimate = "mm",
           h0 = mean(D_cj$aarstal))

# Videregående uddannelse
m_b4 <- mm(D_cj,
           uddannelse_vu_num ~ cj_alder + cj_koen + cj_elite + cj_blok
           + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
           + cj_illiberal_medier + cj_illiberal_dommere 
           + cj_illiberal_inger_mette,
           id = ~ ResponseId,
           estimate = "mm",
           h0 = mean(D_cj$uddannelse_vu_num))

m_b1$outcome = "Kvinde"
m_b2$outcome = "Ideologi"
m_b3$outcome = "Gns.\nfødselsår"
m_b4$outcome = "Videregående\nuddannelse"
p_b <- rbind(m_b1, m_b2, m_b3, m_b4) %>% 
  mutate(statistic = "MM",
         feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed",
                                    feature == "Dagpenge perioden skal sættes ned"
                                    ~ "Dagpenge perioden\nskal sættes ned",
                                    feature == "Klimapolitikken er ikke ambitiøs nok"
                                    ~ "Klimapolitikken er\nikke ambitiøs nok",
                                    feature == "Udlændinge- og integrationspolitikken skal være strammere"
                                    ~ "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                    .default = feature),
                          levels = c("Alder",
                                     "Køn",
                                     "Elite",
                                     "Blok",
                                     "Dagpenge perioden\nskal sættes ned",
                                     "Klimapolitikken er\nikke ambitiøs nok",
                                     "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                     "Mediefrihed",
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level),
         level = case_when(level == "Hverken rød eller blå blok"
                           ~ "Hverken-eller",
                           level == "Hverken enig eller uenig"
                           ~ "Hverken-eller",
                           .default = level)
  )

# Visualiseres:
ggplot(p_b, aes(x = estimate, y = level)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(outcome = c("Kvinde", 
                                           "Ideologi", 
                                           "Gns.\nfødselsår", 
                                           "Videregående\nuddannelse"),
                               i = c(mean(D_cj$kvinde),
                                     mean(D_cj$ideologi),
                                     mean(D_cj$aarstal),
                                     mean(D_cj$uddannelse_vu_num))
             ),
             linetype = "longdash",
             color = c_black) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5) +
  geom_point() +
  labs(x = NULL,
       y = NULL,
  ) +
  facet_grid(cols = vars(outcome),
             rows = vars(feature),
             scales = "free") +
  guides(colour = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

# Eksporteres
ggsave(filename = "randomisering.pdf",
       width = 8, 
       height = 10,
       dpi = 500)



# Balancetest af carry-over effects
m_opgnr <- cj(D_cj, cj_valgt ~ cj_alder + cj_koen + cj_elite + cj_blok
              + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
              + cj_illiberal_medier + cj_illiberal_dommere 
              + cj_illiberal_inger_mette,
              id = ~ ResponseId,
              estimate = "mm",
              h0 = 0.5,
              by = ~ cj_task) %>% 
  mutate(statistic = "MM",
         feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed",
                                    feature == "Dagpenge perioden skal sættes ned"
                                    ~ "Dagpenge perioden\nskal sættes ned",
                                    feature == "Klimapolitikken er ikke ambitiøs nok"
                                    ~ "Klimapolitikken er\nikke ambitiøs nok",
                                    feature == "Udlændinge- og integrationspolitikken skal være strammere"
                                    ~ "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                    .default = feature),
                          levels = c("Alder",
                                     "Køn",
                                     "Elite",
                                     "Blok",
                                     "Dagpenge perioden\nskal sættes ned",
                                     "Klimapolitikken er\nikke ambitiøs nok",
                                     "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                     "Mediefrihed",
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level),
         level = case_when(level == "Hverken rød eller blå blok"
                           ~ "Hverken-eller",
                           level == "Hverken enig eller uenig"
                           ~ "Hverken-eller",
                           .default = level)
  )


# Visualiseres:
ggplot(m_opgnr, aes(x = estimate, y = level,
                    colour = cj_task, group = cj_task)) +
  geom_hline(yintercept = c(1.5, 2.5), colour = "lightgrey", linewidth = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  geom_vline(xintercept = .5,
             linetype = "longdash",
             color = c_black) +
  scale_colour_manual(values = c(c_lightblue, c_darkgrey, c_green,
                                 c_ku_red, c_purple)) +
  labs(x = NULL,
       y = NULL,
       colour = "Conjointopgave") +
  facet_grid(feature ~ .,
             scales = "free_y") +
  guides(colour = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.major.y = element_blank(),
  )

# Eksporterer plot
ggsave(filename = "opgavenummer.pdf",
       width = 8, 
       height = 10,
       dpi = 500)

# Tester samme med ANOVA
anova_opgnr <- cj_anova(D_cj,
                        cj_valgt ~ cj_alder + cj_koen + cj_elite + cj_blok
                        + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
                        + cj_illiberal_medier + cj_illiberal_dommere 
                        + cj_illiberal_inger_mette,
                        id = ~ ResponseId,
                        by = ~ cj_task)

xtable(anova_opgnr, digits = 3)


# Test af rækkefølgen på profiler
# Balancetest af profile order effects
m_order <- cj(D_cj, cj_valgt ~ cj_alder + cj_koen + cj_elite + cj_blok
              + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
              + cj_illiberal_medier + cj_illiberal_dommere 
              + cj_illiberal_inger_mette,
              id = ~ ResponseId,
              estimate = "mm",
              h0 = 0.5,
              by = ~ cj_profile) %>% 
  mutate(statistic = "MM",
         feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed",
                                    feature == "Dagpenge perioden skal sættes ned"
                                    ~ "Dagpenge perioden\nskal sættes ned",
                                    feature == "Klimapolitikken er ikke ambitiøs nok"
                                    ~ "Klimapolitikken er\nikke ambitiøs nok",
                                    feature == "Udlændinge- og integrationspolitikken skal være strammere"
                                    ~ "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                    .default = feature),
                          levels = c("Alder",
                                     "Køn",
                                     "Elite",
                                     "Blok",
                                     "Dagpenge perioden\nskal sættes ned",
                                     "Klimapolitikken er\nikke ambitiøs nok",
                                     "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                     "Mediefrihed",
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level),
         level = case_when(level == "Hverken rød eller blå blok"
                           ~ "Hverken-eller",
                           level == "Hverken enig eller uenig"
                           ~ "Hverken-eller",
                           .default = level)
  )

#m_order <- nye_labels(m_order)

# Visualiseres:
ggplot(m_order, aes(x = estimate, y = reorder(level, level),
                    colour = cj_profile, group = cj_profile)) +
  geom_hline(yintercept = c(1.5, 2.5), colour = "lightgrey", linewidth = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  geom_vline(xintercept = .5,
             linetype = "longdash",
             color = c_black) +
  scale_colour_manual(values = c(c_grey, c_black)) +
  labs(x = NULL,
       y = NULL,
       colour = "Conjointprofil") +
  facet_grid(feature ~ .,
             scales = "free_y") +
  #guides(colour = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.major.y = element_blank(),
  )

# Eksporterer plot
ggsave(filename = "profilnr.pdf",
       width = 8, 
       height = 10,
       dpi = 500)

# Tester samme med ANOVA
anova_order <- cj_anova(D_cj,
                        cj_valgt ~ cj_alder + cj_koen + cj_elite + cj_blok
                        + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration
                        + cj_illiberal_medier + cj_illiberal_dommere 
                        + cj_illiberal_inger_mette,
                        id = ~ ResponseId,
                        by = ~ cj_profile)

xtable(anova_order, digits = 3)

