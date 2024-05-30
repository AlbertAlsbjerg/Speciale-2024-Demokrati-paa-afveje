#----------------------------------------------------------
# ANALYSE: CONJOINT-EKSPERIMENT
#----------------------------------------------------------

# Opsætning af svy model design til brug for binomiale regressionsmodeler
svy_design <- svydesign(ids = ~ ResponseId,
                        data = D_cj)

# Modeller
model_basis <- cj_valgt ~ cj_alder + cj_koen + cj_elite + cj_blok + cj_pol_dagpenge + cj_pol_klima + cj_pol_integration + cj_illiberal_medier + cj_illiberal_dommere + cj_illiberal_inger_mette
model_illiberal <- cj_valgt ~ cj_illiberal_medier + cj_illiberal_dommere + cj_illiberal_inger_mette


#----------- BASISMODEL: ALLE ATTRIBUTTER -----------#

m1_amce <- cj(D_cj, model_basis,
              estimate = "amce",
              id = ~ ResponseId,
)

m1_mm <- cj(D_cj, model_basis,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
)

m1_amce$statistic <- "AMCE"
m1_mm$statistic <- "MM"

m1 <- rbind(m1_amce,
            m1_mm) %>% 
  mutate(level = str_trim(level),
         feature = case_when(feature == "Dagpenge perioden skal sættes ned"
                             ~ "Dagpenge perioden\nskal sættes ned",
                             feature == "Klimapolitikken er ikke ambitiøs nok"
                             ~ "Klimapolitikken er\nikke ambitiøs nok",
                             feature == "Udlændinge- og integrationspolitikken skal være strammere"
                             ~ "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                             feature == "Udpegning af dommere"
                             ~ "Udpegning af\ndommere",
                             feature == "Politikeres brud med loven"
                             ~ "Politikeres brud\nmed loven",
                             .default = feature
         ),
         feature = factor(feature, 
                          levels = c("Alder", "Køn", "Elite", "Blok",                                                       
                                     "Dagpenge perioden\nskal sættes ned",                                       
                                     "Klimapolitikken er\nikke ambitiøs nok",
                                     "Udlændinge- og\nintegrationspolitikken\nskal være strammere",
                                     "Mediefrihed",
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven"),
                          ordered = T),
         
         level = str_trim(level),
         level = case_when(level == "Hverken rød eller blå blok"
                           ~ "Hverken-eller",
                           level == "Hverken enig eller uenig"
                           ~ "Hverken-eller",
                           .default = level)
  )


print(xtable(select(m1, -outcome), digits = 3),
      include.rownames = FALSE, )

ggplot(m1, aes(x = estimate, y = level)) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5) +
  geom_point() +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("AMCE", "MM"),
                               i = c(0, 0.5)),
             linetype = "longdash",
             color = c_black) +
  labs(x = NULL,
       y = NULL
  ) +
  facet_grid(rows = vars(feature),
             cols = vars(statistic),
             scales = "free") +
  guides(colour = guide_legend(reverse = T),) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

# Eksporteres
ggsave(filename = "m1.pdf",
       #plot = , 
       width = 8, 
       height = 10,
       dpi = 500)


#----------- HOLDNINGSKONGRUENS -----------#

# Holdningskongruens 1
kongruens_amce <- cj(D_cj, cj_valgt ~
                       + cj_blok_samme
                     + cj_dagpenge_forskel + cj_klima_forskel + cj_integration_forskel
                     ,
                     estimate = "amce",
                     id = ~ ResponseId,
)

kongruens_mm <- cj(D_cj, cj_valgt ~
                     + cj_blok_samme
                   + cj_dagpenge_forskel + cj_klima_forskel + cj_integration_forskel
                   ,
                   estimate = "mm",
                   h0 = 0.5,
                   id = ~ ResponseId,
)


kongruens_amce$statistic <- "AMCE"
kongruens_mm$statistic <- "MM"

m_kongruens <- rbind(kongruens_amce,
                     kongruens_mm) %>% 
  mutate(level = str_trim(level),
         feature = case_when(feature == "Holdning til dagpenge (relativ)"
                             ~ "Holdning til\ndagpenge (relativ)",
                             feature == "Holdning til klima (relativ)"
                             ~ "Holdning til\nklima (relativ)",
                             feature == "Holdning til udlændinge (relativ)"
                             ~ "Holdning til\nudlændinge (relativ)",
                             .default = feature)
  )

print(xtable(select(m_kongruens, -outcome), digits = 3),
      include.rownames = FALSE)

ggplot(m_kongruens, aes(x = estimate, y = level)) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5) +
  geom_point() +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("AMCE", "MM"),
                               i = c(0, 0.5)),
             linetype = "longdash",
             color = c_black) +
  labs(x = NULL,
       y = NULL
  ) +
  facet_grid(rows = vars(feature),
             cols = vars(statistic),
             scales = "free") +
  guides(colour = guide_legend(reverse = T)) +
  # rediger x-aksen
  scale_x_continuous(
    breaks = c(0, .1, .2, .3, .4, .5, .6),
    #limits = c(-.2, .6)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0)
  )


# Eksporteres
ggsave(filename = "kongruens.pdf",
       #plot = , 
       width = 8, 
       height = 3,
       dpi = 500)


# Interaktionsanalyse med holdningskongruens

#ANOVA test for præfererenceheterogenitet
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_holdningskongruens_factor)

# Model mm
m2_mm <- cj(D_cj, 
            model_illiberal
            ,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
            by = ~ cj_holdningskongruens_factor
)

m2_mm$statistic <- "MM"

# Forskel i subgruppeanalyse med holdningskongruens 
D_cj_hk <- D_cj
D_cj_hk$cj_holdningskongruens_factor <- factor(D_cj_hk$cj_holdningskongruens_factor,
                                               levels = c("Lidt ens", 
                                                          "Lidt forskellig", 
                                                          "Helt forskellig", 
                                                          "Helt ens"))

m2_mm_diff <- cj(D_cj_hk, 
                 model_illiberal
                 ,
                 estimate = "mm_diff",
                 id = ~ ResponseId,
                 by = ~ cj_holdningskongruens_factor
)

m2_mm_diff$statistic <- "MM forskel\n(ref: Lidt ens)"

m2 <- rbind(m2_mm, m2_mm_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         BY = str_trim(BY),
         level = str_trim(level)
  )

print(xtable(select(m2, -outcome, -cj_holdningskongruens_factor), digits = 3),
      include.rownames = FALSE)

ggplot(m2, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Lidt ens)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Holdningskongruens",
       shape = NULL
  ) +
  facet_grid(rows = vars(feature), 
             cols = vars(statistic),
             scales = "free",
  ) +
  guides(colour = guide_legend(reverse = T),
         shape = F
  ) +
  scale_colour_manual(values = c("Helt ens" = c_lightblue, 
                                 "Helt forskellig" = c_grey, 
                                 "Lidt ens" = c_green, 
                                 "Lidt forskellig" = c_ku_red,
                                 
                                 "Helt ens - Lidt ens" = c_lightblue, 
                                 "Helt forskellig - Lidt ens" = c_grey, 
                                 "Lidt forskellig - Lidt ens" = c_ku_red),
                      breaks = c("Helt ens", "Lidt ens", "Lidt forskellig", "Helt forskellig")) +
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

# Eksporteres
ggsave(filename = "m2.pdf",
       #plot = , 
       width = 8, 
       height = 4.5,
       dpi = 500)


# Holdningskongruens på tværs af politiske emner
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_integration_forskel)

m2_u_mm <- cj(D_cj, 
              model_illiberal,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ cj_integration_forskel
)
m2_u_mm$emne <- "Udlændinge- og\nintegrationspolitik"

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_klima_forskel)
m2_k_mm <- cj(D_cj, 
              model_illiberal
              ,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ cj_klima_forskel
)
m2_k_mm$emne <- "Klimapolitik"

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_dagpenge_forskel)

m2_d_mm <- cj(D_cj,
              model_illiberal
              ,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ cj_dagpenge_forskel
)
m2_d_mm$emne <- "Dagpengepolitik"

m2_pol <- data.table::rbindlist(list(m2_u_mm, m2_k_mm, m2_d_mm),
                                use.names = F) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         BY = str_trim(BY),
         level = str_trim(level)
  )


m2_pol$statistic <- "MM"
print(xtable(select(m2_pol, -outcome, -cj_integration_forskel), digits = 3),
      include.rownames = FALSE)

# Plottes
ggplot(m2_pol, aes(x = estimate, y = level, colour = BY, group = BY)) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Kongruens"
  ) +
  facet_grid(
    cols = vars(emne),
    rows = vars(feature),
    scales = "free_y",
  ) +
  guides(colour = guide_legend(reverse = T)) +
  # rediger x-aksen
  scale_x_continuous(
    breaks = c(.3, .4, .5, .6, .7),
    limits = c(.28, .72)
  ) +
  scale_colour_manual(values = c(c_black, c_grey)) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,0,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.major.y = element_blank(),
  )

# Eksporteres
ggsave(filename = "m2_emner.pdf",
       width = 8, 
       height = 4,
       dpi = 500)


#----------- KONGRUENS PÅ TVÆRS AF BLOKKE -----------#

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_blok_samme)

# Interaktionsanalyse medpå tværs af blok-kongruens
m2_blok_mm <- cj(D_cj, 
                 model_illiberal,
                 estimate = "mm",
                 h0 = 0.5,
                 id = ~ ResponseId,
                 by = ~ cj_blok_samme
)
m2_blok_diff <- cj(D_cj, 
                   model_illiberal,
                   estimate = "mm_diff",
                   id = ~ ResponseId,
                   by = ~ cj_blok_samme
)

m2_blok_mm$statistic <- "MM"
m2_blok_diff$statistic <- "MM forskel\n(ref: Anden blok)"

m2_blok <- rbind(m2_blok_mm, m2_blok_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         BY = str_trim(BY),
         level = str_trim(level)
  )

print(xtable(select(m2_blok, -outcome, -cj_blok_samme), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m2_blok, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Anden blok)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, #c("MM"),
       y = NULL,
       colour = "Blok"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  ) +
  scale_colour_manual(values = c("Anden blok" = c_grey, 
                                 "Samme blok" = c_black, 
                                 "Samme blok - Anden blok" = c_black),
                      breaks = c("Anden blok", "Samme blok")) +
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

# Eksporteres
ggsave(filename = "m2_blok.pdf",
       width = 8, 
       height = 3,
       dpi = 500)

# EKSPLORATIV: Bloks betydning på valg af illiberal/liberal
# Interaktionsanalyse på tværs af blokke

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ blok)

m_blok_mm <- cj(D_cj, 
                model_illiberal,
                estimate = "mm",
                h0 = 0.5,
                id = ~ ResponseId,
                by = ~ blok
)

m_blok_diff <- cj(D_cj,
                  model_illiberal,
                  estimate = "mm_diff",
                  id = ~ ResponseId,
                  by = ~ blok
)

m_blok_mm$statistic <- "MM"
m_blok_diff$statistic <- "MM forskel\n(ref: Andet valg)"

m_blok <- rbind(m_blok_mm, m_blok_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m_blok, -outcome, -blok), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m_blok, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Andet valg)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, #c("MM"),
       y = NULL,
       colour = "Blok"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Rød blok" = c_ku_red,
                                 "Blå blok" = c_lightblue,
                                 "Midten" = c_purple,
                                 "Andet valg" = c_black,
                                 
                                 "Rød blok - Andet valg" = c_ku_red,
                                 "Blå blok - Andet valg" = c_lightblue,
                                 "Midten - Andet valg" = c_purple,
                                 "Andet valg - Andet valg" = c_black),
                      breaks = c("Rød blok", "Blå blok", "Midten", "Andet valg")) +
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

# Eksporteres
ggsave(filename = "m_blok_valg.pdf",
       #plot = , 
       width = 8, 
       height = 4.5,
       dpi = 500)

# EKSPLORATIV: Bloks betydning på valg af illiberal/liberal

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ partivalg)

# Interaktionsanalyse på tværs af partivalg
m_parti_mm <- cj(D_cj, 
                 model_illiberal,
                 estimate = "mm",
                 h0 = 0.5,
                 id = ~ ResponseId,
                 by = ~ partivalg
)


m_parti_mm$statistic <- "MM"

m_parti <- m_parti_mm %>% 
  mutate(statistic = "MM",
         feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m_parti, -outcome, -partivalg), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m_parti, aes(x = estimate, y = level, colour = reorder(fct_inorder(partivalg),
                                                              desc(partivalg)))) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .9),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .9)) +
  labs(x = NULL, 
       y = NULL,
       colour = NULL
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free") +
  guides(colour = guide_legend(reverse = T,
                               nrow=5, byrow=TRUE)
  ) +
  scale_colour_manual(values = c(
    "Ved ikke" = c_black,
    "Blank" = c_lightgrey,
    "Andet" = c_grey,
    "Konservative Folkeparti" = "#00571F", 
    "Radikale Venstre" = "#733280",
    "Socialistisk Folkeparti" = "#eb94d1",
    "Venstre" = "#01438E",
    "Liberal Alliance" = "#3FB2BE",
    "Enhedslisten" = "#F7660D",
    "Socialdemokratiet" = "#C82518",      
    "Dansk Folkeparti" = "#FCD03B",
    "Alternativet" = "#00FF00",
    "Danmarksdemokraterne" = "#1272C2",
    "Moderaterne" = "#B48CD2")
  ) +
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

# Eksporteres
ggsave(filename = "m_parti_valg.pdf",
       #plot = , 
       width = 8, 
       height = 10,
       dpi = 500)


#----------- TILSLUTNING TIL DE LIBERALE DEMOKRATISKE VÆRDIER -----------#

# Subgruppeanalyse med demokrati indeks split
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_abstrakt)

# Vigtigheden af demokratiet (abstrakt mål)
m4_a_mm <- cj(D_cj, 
              model_illiberal,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ demo_abstrakt
)
m4_a_diff <- cj(D_cj, cj_valgt ~ cj_illiberal_medier + cj_illiberal_dommere 
                + cj_illiberal_inger_mette
                ,
                estimate = "mm_diff",
                id = ~ ResponseId,
                by = ~ demo_abstrakt
)
m4_a_mm$statistic <- "MM"

m4_a_diff$statistic <- "MM forskel\n(ref: Lav)"

m4_a <- rbind(m4_a_mm, m4_a_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m4_a, -outcome, -demo_abstrakt), digits = 3),
      include.rownames = FALSE)


# Tilfredshed med demokratiet
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_tilfreds)

m4_t_mm <- cj(D_cj, 
              model_illiberal,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ demo_tilfreds
)
m4_t_diff <- cj(D_cj, 
                model_illiberal,
                estimate = "mm_diff",
                id = ~ ResponseId,
                by = ~ demo_tilfreds
)

m4_t_mm$statistic <- "MM"

m4_t_diff$statistic <- "MM forskel\n(ref: Lav)"

m4_t <- rbind(m4_t_mm, m4_t_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m4_t, -outcome, -demo_tilfreds), digits = 3),
      include.rownames = FALSE)

# Tilfredshed med demokratiet (valghandlingen)
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_valg)

m4_v_mm <- cj(D_cj, cj_valgt ~ cj_illiberal_medier + cj_illiberal_dommere 
              + cj_illiberal_inger_mette
              ,
              estimate = "mm",
              h0 = 0.5,
              id = ~ ResponseId,
              by = ~ demo_valg
)
m4_v_diff <- cj(D_cj, cj_valgt ~ cj_illiberal_medier + cj_illiberal_dommere 
                + cj_illiberal_inger_mette
                ,
                estimate = "mm_diff",
                id = ~ ResponseId,
                by = ~ demo_valg
)

m4_v_mm$statistic <- "MM"

m4_v_diff$statistic <- "MM forskel\n(ref: Lav)"

m4_v <- rbind(m4_v_mm, m4_v_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m4_v, -outcome, -demo_valg), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
# Abstrakt tilslutning til demokratiet
ggplot(m4_a, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Lav)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, #c("MM"),
       y = NULL,
       colour = "Abstrakt tilslutning til demokratiet"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Lav" = c_grey, 
                                 "Høj - Lav" = c_black),
                      breaks = c("Høj", "Lav")) +
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

# Eksporteres
ggsave(filename = "m4_a.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)

# Tilfredshed med demokratiet
ggplot(m4_t, aes(x = estimate, y = level, 
                 colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Lav)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, #c("MM"),
       y = NULL,
       colour = "Tilfredshed med demokratiet"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Middel" = c_grey,
                                 "Lav" = c_lightblue, 
                                 
                                 "Høj - Lav" = c_black,
                                 "Høj - Middel" = c_grey),
                      breaks = c("Høj", "Middel", "Lav")) +
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

# Eksporteres
ggsave(filename = "m4_t.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)

# Tilfredshed med valghandlingen
ggplot(m4_v, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Lav)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Opbakning til valghandlingen"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Middel" = c_grey,
                                 "Lav" = c_lightblue, 
                                 
                                 "Høj - Lav" = c_black,
                                 "Høj - Middel" = c_grey),
                      breaks = c("Høj", "Middel", "Lav")) +
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

# Eksporteres
ggsave(filename = "m4_v.pdf",
       width = 8, 
       height = 4,
       dpi = 500)


#----------- ALDERSGRUPPEEFFEKTER OG TILSLUTNING TIL DEMOKRATIET -----------#

# Test med ANOVA
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ aldersgruppe)

# Interaktionsanalyse på tværs af aldesrgrupper
m5_alder_mm <- cj(D_cj, 
                  model_illiberal,
                  estimate = "mm",
                  h0 = 0.5,
                  id = ~ ResponseId,
                  by = ~ aldersgruppe
)
m5_alder_diff <- cj(D_cj, 
                    model_illiberal,
                    estimate = "mm_diff",
                    id = ~ ResponseId,
                    by = ~ aldersgruppe
)

m5_alder_mm$statistic <- "MM"
m5_alder_diff$statistic <- "MM forskel\n(ref: 18-34 år)"

m5_alder <- rbind(m5_alder_mm, m5_alder_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m5_alder, -outcome, -aldersgruppe), digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m5_alder, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: 18-34 år)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Aldersgrupper"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("18-34 år" = c_grey, 
                                 "35-50 år" = c_black, 
                                 "51-65 år" = c_green, 
                                 "65+ år" = c_lightblue,
                                 
                                 "18-34 år - 18-34 år" = c_grey, 
                                 "35-50 år - 18-34 år" = c_black, 
                                 "51-65 år - 18-34 år" = c_green, 
                                 "65+ år - 18-34 år" = c_lightblue),
                      breaks = c("18-34 år", "35-50 år", "51-65 år","65+ år")) +
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

# Eksporteres
ggsave(filename = "m5_alder.pdf",
       #plot = , 
       width = 8, 
       height = 5,
       dpi = 500)


# ALDER OG HOLDNINGSKONGRUENS
cj_anova(subset(D_cj, cj_holdningskongruens_split == "Anden"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ aldersgruppe)
m5_k_mm_a <- cj(subset(D_cj, cj_holdningskongruens_split == "Anden"), 
                model_illiberal,
                estimate = "mm",
                h0 = 0.5,
                id = ~ ResponseId,
                by = ~ aldersgruppe
)

cj_anova(subset(D_cj, cj_holdningskongruens_split == "Samme"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ aldersgruppe)
m5_k_mm_s <- cj(subset(D_cj, cj_holdningskongruens_split == "Samme"), 
                model_illiberal,
                estimate = "mm",
                h0 = 0.5,
                id = ~ ResponseId,
                by = ~ aldersgruppe
)

m5_k_mm_a$statistic <- "MM"
m5_k_mm_s$statistic <- "MM"
m5_k_mm_a$kongruens <- "Inkongruent"
m5_k_mm_s$kongruens <- "Kongruent"

m5_k <- rbind(m5_k_mm_a, m5_k_mm_s) %>% 
  mutate(feature = factor(
    case_when(feature == "cj_illiberal_medier" ~ 
                "Mediefrihed",
              feature == "cj_illiberal_dommere" ~ 
                "Udpegning af\ndommere",
              feature == "cj_illiberal_inger_mette" ~
                "Politikeres brud\nmed loven",
              .default = feature),
    levels = c("Mediefrihed", "Udpegning af\ndommere","Politikeres brud\nmed loven")),
    
    level = str_trim(level)
  )

print(xtable(select(m5_k, -outcome, -aldersgruppe), digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m5_k, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, #c("MM"),
       y = NULL,
       colour = "Aldersgrupper"
  ) +
  facet_grid(cols = vars(kongruens),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("18-34 år" = c_grey, 
                                 "35-50 år" = c_black, 
                                 "51-65 år" = c_green, 
                                 "65+ år" = c_lightblue),
                      breaks = c("18-34 år", "35-50 år", "51-65 år","65+ år")) +
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

# Eksporteres
ggsave(filename = "m5_k.pdf",
       width = 8, 
       height = 5,
       dpi = 500)


# EKSPLORATIV: Aldersgrupper og demokratiindikatorer

# Vigtigheden af demokratiet (abstrakt mål)
cj_anova(subset(D_cj, aldersgruppe == "18-34 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_abstrakt)
m5_a_1 <- cj(subset(D_cj, aldersgruppe == "18-34 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_abstrakt
)
m5_a_1$alder <- "18-34 år"

cj_anova(subset(D_cj, aldersgruppe == "35-50 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_abstrakt)
m5_a_2 <- cj(subset(D_cj, aldersgruppe == "35-50 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_abstrakt
)
m5_a_2$alder <- "35-50 år"

cj_anova(subset(D_cj, aldersgruppe == "51-65 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_abstrakt)
m5_a_3 <- cj(subset(D_cj, aldersgruppe == "51-65 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_abstrakt
)
m5_a_3$alder <- "51-65 år"

cj_anova(subset(D_cj, aldersgruppe == "65+ år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_abstrakt)
m5_a_4 <- cj(subset(D_cj, aldersgruppe == "65+ år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_abstrakt
)
m5_a_4$alder <- "65+ år"


m5_a <- rbind(m5_a_1, m5_a_2, m5_a_3, m5_a_4)%>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )
m5_a$statistic <- "MM"

# Tilfredshed med demokratiet
cj_anova(subset(D_cj, aldersgruppe == "18-34 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_tilfreds)
m5_t_1 <- cj(subset(D_cj, aldersgruppe == "18-34 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_tilfreds
)
m5_t_1$alder <- "18-34 år"

cj_anova(subset(D_cj, aldersgruppe == "35-50 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_tilfreds)
m5_t_2 <- cj(subset(D_cj, aldersgruppe == "35-50 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_tilfreds
)
m5_t_2$alder <- "35-50 år"

cj_anova(subset(D_cj, aldersgruppe == "51-65 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_tilfreds)
m5_t_3 <- cj(subset(D_cj, aldersgruppe == "51-65 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_tilfreds
)
m5_t_3$alder <- "51-65 år"

cj_anova(subset(D_cj, aldersgruppe == "65+ år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_tilfreds)
m5_t_4 <- cj(subset(D_cj, aldersgruppe == "65+ år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_tilfreds
)
m5_t_4$alder <- "65+ år"


m5_t <- rbind(m5_t_1, m5_t_2, m5_t_3, m5_t_4)%>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )
m5_t$statistic <- "MM"

# Tilfredshed med valghandlingen
cj_anova(subset(D_cj, aldersgruppe == "18-34 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_valg)
m5_v_1 <- cj(subset(D_cj, aldersgruppe == "18-34 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_valg
)
m5_v_1$alder <- "18-34 år"

cj_anova(subset(D_cj, aldersgruppe == "35-50 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_valg)
m5_v_2 <- cj(subset(D_cj, aldersgruppe == "35-50 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_valg
)
m5_v_2$alder <- "35-50 år"

cj_anova(subset(D_cj, aldersgruppe == "51-65 år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_valg)
m5_v_3 <- cj(subset(D_cj, aldersgruppe == "51-65 år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_valg
)
m5_v_3$alder <- "51-65 år"

cj_anova(subset(D_cj, aldersgruppe == "65+ år"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ demo_valg)
m5_v_4 <- cj(subset(D_cj, aldersgruppe == "65+ år"), 
             model_illiberal
             ,
             estimate = "mm",
             h0 = 0.5,
             id = ~ ResponseId,
             by = ~ demo_valg
)
m5_v_4$alder <- "65+ år"

m5_v <- rbind(m5_v_1, m5_v_2, m5_v_3, m5_v_4)%>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )
m5_v$statistic <- "MM"


# Tabeller printes til bilag
print(xtable(select(m5_a, -outcome, -demo_abstrakt)[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], digits = 3),
      include.rownames = FALSE)
print(xtable(select(m5_t, -outcome, -demo_tilfreds)[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], digits = 3),
      include.rownames = FALSE)
print(xtable(select(m5_v, -outcome, -demo_valg)[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], digits = 3),
      include.rownames = FALSE)


# Visualiseres:
# Abstrakt tilslutning til demokratiet
ggplot(m5_a, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Abstrakt tilslutning til demokratiet"
  ) +
  facet_grid(cols = vars(alder),
             rows = vars(feature)) +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Lav" = c_grey),
                      breaks = c("Høj", "Lav")) +
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

# Eksporteres
ggsave(filename = "m5_a.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)

# Tilfredshed med demokratiet
ggplot(m5_t, aes(x = estimate, y = level, 
                 colour = fct_inorder(BY))) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Tilfredshed med demokratiet"
  ) +
  facet_grid(cols = vars(alder),
             rows = vars(feature)) +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Middel" = c_grey,
                                 "Lav" = c_lightblue),
                      breaks = c("Høj", "Middel", "Lav")) +
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

# Eksporteres
ggsave(filename = "m5_t.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)

# Tilfredshed med valghandlingen
ggplot(m5_v, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Opbakning til valghandlingen"
  ) +
  facet_grid(cols = vars(alder),
             rows = vars(feature)) +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Middel" = c_grey,
                                 "Lav" = c_lightblue),
                      breaks = c("Høj", "Middel", "Lav")) +
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

# Eksporteres
ggsave(filename = "m5_v.pdf",
       width = 8, 
       height = 4,
       dpi = 500)


#----------- POPULISME -----------#

# Interaktionsanalyse med populisme split
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ populisme_split)
m6_mm <- cj(D_cj, 
            model_illiberal,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
            by = ~ populisme_split
)
m6_diff <- cj(D_cj, 
              model_illiberal,
              estimate = "mm_diff",
              id = ~ ResponseId,
              by = ~ populisme_split
)

m6_mm$statistic <- "MM"
m6_diff$statistic <- "MM forskel\n(ref: Lav)"

m6 <- rbind(m6_mm, m6_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m6, -outcome, -populisme_split), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m6, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Lav)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Populisme"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Høj" = c_black, 
                                 "Lav" = c_grey, 
                                 "Høj - Lav" = c_black),
                      breaks = c("Høj", "Lav")) +
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

# Eksporteres
ggsave(filename = "m6.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)


#----------- KOMMUNEGRUPPER -----------#

D_cj_kommune <- D_cj %>% 
  mutate(kommunegruppe = factor(
    case_when(kommunegruppe == "Hovedstadskommuner" ~ "Hovedstad",
              kommunegruppe == "Storbykommuner" ~ "Storby",
              kommunegruppe == "Provinskommuner" ~ "Provins",
              kommunegruppe == "Oplandskommuner" ~ "Opland",
              kommunegruppe == "Landkommuner" ~ "Land"),
    levels = c("Land", "Opland", "Provins", "Storby", "Hovedstad"),
    ordered = T)
  )

# Subgruppeanalyse på tværs af kommuner
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ kommunegruppe)
m8_mm <- cj(D_cj_kommune, 
            model_illiberal,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
            by = ~ kommunegruppe
)
m8_diff <- cj(D_cj_kommune, 
              model_illiberal,
              estimate = "mm_diff",
              id = ~ ResponseId,
              by = ~ kommunegruppe
)

m8_mm$statistic <- "MM"
m8_diff$statistic <- "MM forskel\n(ref: Landkommuner)"

m8 <- rbind(m8_mm, m8_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m8, -outcome, -kommunegruppe), digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m8, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Landkommuner)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  labs(x = NULL,
       y = NULL,
       colour = "Kommunegruppe"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Hovedstad" = c_purple, 
                                 "Storby" = c_ku_darkred, 
                                 "Provins" = c_yellow, 
                                 "Opland" = c_blue, 
                                 "Land" = c_green,
                                 
                                 "Hovedstad - Land" = c_purple, 
                                 "Storby - Land" = c_ku_darkred, 
                                 "Provins - Land" = c_yellow, 
                                 "Opland - Land" = c_blue),
                      breaks = c("Land", "Opland", "Provins", "Storby", "Hovedstad")
  ) +
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

# Eksporteres
ggsave(filename = "m8_fuld.pdf",
       #plot = , 
       width = 8, 
       height = 7,
       dpi = 500)


#### EKSPLORATIV: REGIONER
# Subgruppeanalyse på tværs af regioner
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ region)
m8_reg_mm <- cj(D_cj, 
                model_illiberal,
                estimate = "mm",
                h0 = 0.5,
                id = ~ ResponseId,
                by = ~ region
)
m8_reg_diff <- cj(D_cj, 
                  model_illiberal,
                  estimate = "mm_diff",
                  id = ~ ResponseId,
                  by = ~ region
)

m8_reg_mm$statistic <- "MM"
m8_reg_diff$statistic <- "MM forskel\n(ref: Hovedstad)"

m8_reg <- rbind(m8_reg_mm, m8_reg_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         BY = factor(BY, 
                     levels = c("Region Hovedstaden", 
                                "Region Sjælland", 
                                "Region Nordjylland", 
                                "Region Syddanmark", 
                                "Region Midtjylland",
                                "Region Sjælland - Region Hovedstaden", 
                                "Region Nordjylland - Region Hovedstaden", 
                                "Region Syddanmark - Region Hovedstaden", 
                                "Region Midtjylland - Region Hovedstaden"),
                     ordered = T),
         
         level = str_trim(level)
  )

print(xtable(select(m8_reg, -outcome, -region), digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m8_reg, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Hovedstad)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  labs(x = NULL,
       y = NULL,
       colour = "Region"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(nrow = 2, reverse = T)) +
  scale_colour_manual(values = c("Region Hovedstaden" = c_purple, 
                                 "Region Sjælland" = c_ku_red, 
                                 "Region Nordjylland" = c_yellow, 
                                 "Region Syddanmark" = c_blue, 
                                 "Region Midtjylland" = c_green,
                                 
                                 "Region Sjælland - Region Hovedstaden" = c_ku_red, 
                                 "Region Nordjylland - Region Hovedstaden" = c_yellow, 
                                 "Region Syddanmark - Region Hovedstaden" = c_blue, 
                                 "Region Midtjylland - Region Hovedstaden" = c_green),
                      
                      breaks = c("Region Hovedstaden", "Region Sjælland", 
                                 "Region Nordjylland", "Region Syddanmark", 
                                 "Region Midtjylland")
  ) +
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

# Eksporteres
ggsave(filename = "m8_reg.pdf",
       #plot = , 
       width = 8, 
       height = 7,
       dpi = 500)


#----------- UDDANNELSE -----------#

D_cj_udd <- D_cj %>% 
  mutate(uddannelse = factor(
    case_when(uddannelse == "Grundskole" ~ "Grundskole",
              uddannelse == "Gymnasial uddannelse" ~ "Gymnasiel",
              uddannelse == "Erhvervsfaglig uddannelse" ~ "Erhvervsfaglig",
              uddannelse == "Kort videregående uddannelse" ~ "KVU",
              uddannelse == "Mellemlang videregående uddannelse" ~ "MVU",
              uddannelse == "Lang videregående uddannelse" ~ "LVU",
              uddannelse == "Forskeruddannelse" ~ "Forsker"),
    levels = c("Grundskole", "Gymnasiel", "Erhvervsfaglig", "KVU", "MVU", "LVU", "Forsker"),
    ordered = T)
  )

# Interaktionsanalyse på tværs af uddannelse
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse)
m7_mm <- cj(D_cj_udd, 
            model_illiberal,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
            by = ~ uddannelse
)
m7_diff <- cj(D_cj_udd, 
              model_illiberal,
              estimate = "mm_diff",
              id = ~ ResponseId,
              by = ~ uddannelse
)

m7_mm$statistic <- "MM"
m7_diff$statistic <- "MM forskel\n(ref: Grundskole)"

m7 <- rbind(m7_mm, m7_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m7, -outcome, -uddannelse), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m7, aes(x = estimate, y = level, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Grundskole)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  labs(x = NULL,
       y = NULL,
       colour = "Uddannelse"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Grundskole" = c_grey, 
                                 "Gymnasiel" = c_black, 
                                 "Erhvervsfaglig" = c_lightblue, 
                                 "KVU" = c_green, 
                                 "MVU" = c_blue,
                                 "LVU" = c_purple, 
                                 "Forsker" = c_ku_red,
                                 
                                 "Gymnasiel - Grundskole" = c_black, 
                                 "Erhvervsfaglig - Grundskole" = c_lightblue, 
                                 "KVU - Grundskole" = c_green, 
                                 "MVU - Grundskole" = c_blue,
                                 "LVU - Grundskole" = c_purple, 
                                 "Forsker - Grundskole" = c_ku_red),
                      breaks = c("Grundskole", "Gymnasiel", "Erhvervsfaglig",
                                 "KVU", "MVU", "LVU", "Forsker")) +
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

# Eksporteres
ggsave(filename = "m7_fuld.pdf",
       #plot = , 
       width = 8, 
       height = 9,
       dpi = 500)


# Uddannelse som dikotom variabel
cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_dik_mm <- cj(D_cj, 
                model_illiberal,
                estimate = "mm",
                h0 = 0.5,
                id = ~ ResponseId,
                by = ~ uddannelse_vu
)
m7_dik_diff <- cj(D_cj, 
                  model_illiberal,
                  estimate = "mm_diff",
                  id = ~ ResponseId,
                  by = ~ uddannelse_vu
)

m7_dik_mm$statistic <- "MM"
m7_dik_diff$statistic <- "MM forskel\n(ref: har en VU)"

m7_dik <- rbind(m7_dik_mm, m7_dik_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level),
         BY = relevel(BY, "Ja")
  )

print(xtable(select(m7_dik, -outcome, -uddannelse_vu), digits = 3),
      include.rownames = FALSE)


# Visualiseres:
ggplot(m7_dik, aes(x = estimate, y = level, colour = BY)) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: har en VU)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL, 
       y = NULL,
       colour = "Har en videregående uddannelse?"
  ) +
  facet_grid(cols = vars(statistic),
             rows = vars(feature),
             scales = "free_x") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Ja" = c_grey, 
                                 "Nej" = c_black,
                                 
                                 "Nej - Ja" = c_black),
                      breaks = c("Ja", "Nej")) +
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

# Eksporteres
ggsave(filename = "m7_dik.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)

# EKSPLORATIVT: Interaktion mellem uddannelse og kommune på valg af illiberal
cj_anova(subset(D_cj, kommunegruppe == "Hovedstadskommuner"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_h <- cj(subset(D_cj, kommunegruppe == "Hovedstadskommuner"), 
           model_illiberal,
           estimate = "mm",
           h0 = 0.5,
           id = ~ ResponseId,
           by = ~ uddannelse_vu
)

cj_anova(subset(D_cj, kommunegruppe == "Storbykommuner"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_s <- cj(subset(D_cj, kommunegruppe == "Storbykommuner"), 
           model_illiberal,
           estimate = "mm",
           h0 = 0.5,
           id = ~ ResponseId,
           by = ~ uddannelse_vu
)

cj_anova(subset(D_cj, kommunegruppe == "Provinskommuner"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_p <- cj(subset(D_cj, kommunegruppe == "Provinskommuner"), 
           model_illiberal,
           estimate = "mm",
           h0 = 0.5,
           id = ~ ResponseId,
           by = ~ uddannelse_vu
)

cj_anova(subset(D_cj, kommunegruppe == "Oplandskommuner"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_o <- cj(subset(D_cj, kommunegruppe == "Oplandskommuner"), 
           model_illiberal,
           estimate = "mm",
           h0 = 0.5,
           id = ~ ResponseId,
           by = ~ uddannelse_vu
)

cj_anova(subset(D_cj, kommunegruppe == "Landkommuner"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ uddannelse_vu)
m7_l <- cj(subset(D_cj, kommunegruppe == "Landkommuner"), 
           model_illiberal,
           estimate = "mm",
           h0 = 0.5,
           id = ~ ResponseId,
           by = ~ uddannelse_vu
)

m7_h$statistic <- "MM"
m7_s$statistic <- "MM"
m7_p$statistic <- "MM"
m7_o$statistic <- "MM"
m7_l$statistic <- "MM"

m7_h$gruppe <- "Hovedstad"
m7_s$gruppe <- "Storby"
m7_p$gruppe <- "Provins"
m7_o$gruppe <- "Opland"
m7_l$gruppe <- "Land"


m7_kg <- rbind(m7_h, m7_s, m7_p, m7_o, m7_l) %>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         level = str_trim(level),
         
         BY = factor(BY, levels = c("Ja", "Nej"),
                     ordered = T
         )
  )

print(xtable(select(m7_kg, -outcome, -uddannelse_vu)[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m7_kg, aes(x = estimate, y = level, group = BY, colour = fct_inorder(BY))) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .7),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .7)) +
  labs(x = NULL,
       y = NULL,
       colour = "Har en videregående uddannelse?"
  ) +
  scale_x_continuous(limits = c(0.28,0.72),
                     breaks = c(0.3, 0.5, 0.7))+
  facet_grid(cols = vars(fct_inorder(gruppe)),
             rows = vars(feature),
             scales = "fixed") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Nej" = c_black, 
                                 "Ja" = c_grey)
  ) +
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

# Eksporteres
ggsave(filename = "m7_kg.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)


#----------- ELITEAFSENDER -----------#

D_cj_elite <- D_cj

D_cj_elite$cj_ilib_medier_elite <- 
  interaction(D_cj$cj_elite, D_cj_elite$cj_illiberal_medier, sep = ":\n")
D_cj_elite$cj_ilib_dommere_elite <- 
  interaction(D_cj$cj_elite, D_cj_elite$cj_illiberal_dommere, sep = ":\n")
D_cj_elite$cj_ilib_politikere_elite <- 
  interaction(D_cj$cj_elite, D_cj_elite$cj_illiberal_inger_mette, sep = ":\n")
D_cj_elite$cj_illib_alle <- 
  interaction(D_cj$cj_elite, 
              D_cj_elite$cj_illiberal_medier,
              D_cj_elite$cj_illiberal_dommere,
              D_cj_elite$cj_illiberal_inger_mette,
              sep = ": ")
D_cj_elite$cj_illib <- 
  interaction(D_cj_elite$cj_illiberal_medier,
              D_cj_elite$cj_illiberal_dommere,
              D_cj_elite$cj_illiberal_inger_mette,
              sep = ": ")
attr(D_cj_elite$cj_illib_alle, "label") <- ""

cj_anova(D_cj_elite,
         cj_valgt ~ 
           + cj_illib,
         id = ~ ResponseId,
         by = ~ cj_elite)

m9_amce <- cj(D_cj_elite, 
              cj_valgt ~ 
                + cj_illib
              ,
              estimate = "amce",
              id = ~ ResponseId,
              by = ~ cj_elite
)

m9_amce_diff <- cj(D_cj_elite, 
                   cj_valgt ~ 
                     + cj_illib
                   ,
                   estimate = "amce_diff",
                   id = ~ ResponseId,
                   by = ~ cj_elite
)


m9_mm <- cj(D_cj_elite, 
            cj_valgt ~ 
              + cj_illib
            ,
            estimate = "mm",
            h0 = 0.5,
            id = ~ ResponseId,
            by = ~ cj_elite
)

m9_mm_diff <- cj(D_cj_elite, 
                 cj_valgt ~ cj_illib
                 ,
                 estimate = "mm_diff",
                 id = ~ ResponseId,
                 by = ~ cj_elite
)

m9_amce$statistic = "AMCE"
m9_amce_diff$statistic = "AMCE forskel\n(ref: Privat person)"
m9_mm$statistic = "MM"
m9_mm_diff$statistic = "MM forskel\n(ref: Privat person)"

m9 <- rbind(m9_amce, m9_amce_diff, m9_mm, m9_mm_diff)

m9[c("Medier", "Dommere", "Politikere")] <- str_replace_all(str_trim(str_split_fixed(m9$level, ": ", 3)), "eral", ".")
m9$Medier <- paste0(m9$Medier, "(M)")
m9$Dommere <- paste0(m9$Dommere, "(D)")
m9$Politikere <- paste0(m9$Politikere, "(P)")
m9$level2 <- paste0(m9$Medier,
                    "; ", m9$Dommere,
                    "; ", m9$Politikere)
m9$level2 <- factor(m9$level2, levels = unique(m9$level2))

print(xtable(select(m9, -outcome, -feature, - level, -level2, -BY)[, c(8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 7)], 
             digits = 3),
      include.rownames = FALSE)

# Visualiseres:
ggplot(m9, aes(x = estimate, y = level2, colour = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("AMCE", "AMCE forskel\n(ref: Privat person)", "MM", "MM forskel\n(ref: Privat person)"),
                               i = c(0, 0, 0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), #c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), 
             colour = "lightgrey", size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = "Afsender"
  ) +
  facet_grid(#rows = vars(level),
    cols = vars(statistic),
    scales = "free") +
  guides(colour = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("Politiker" = c_black, 
                                 "Privat person" = c_grey, 
                                 ".L - Privat person" = c_black,
                                 "Politiker - Privat person" = c_black),
                      breaks = c("Politiker", "Privat person")) +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

# Eksporteres
ggsave(filename = "m9_fuld.pdf",
       #plot = , 
       width = 8, 
       height = 5,
       dpi = 500)

cj_anova(D_cj,
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_elite)

m9_amce_2 <- cj(D_cj, 
                model_illiberal
                ,
                estimate = "amce",
                id = ~ ResponseId,
                by = ~ cj_elite
)
m9_amce_2$statistic <- "AMCE"

m9_amce_diff <- cj(D_cj, 
                   model_illiberal
                   ,
                   estimate = "amce_diff",
                   id = ~ ResponseId,
                   by = ~ cj_elite
)
m9_amce_diff$statistic <- "AMCE forskel\n(ref: Privat person)"
m9_amce_diff$BY <- "Politiker - Privat person"

m9_diff <- rbind(m9_amce_2, m9_amce_diff) %>% 
  mutate(feature = factor(case_when(feature == "Udpegning af dommere" ~ "Udpegning af\ndommere",
                                    feature == "Politikeres brud med loven" ~ "Politikeres brud\nmed loven",
                                    feature == "Mediefrihed" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m9_diff, -outcome, -cj_elite), digits = 3),
      include.rownames = FALSE)

ggplot(m9_diff, aes(x = estimate, y = level, colour = BY, group = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("AMCE", "AMCE forskel\n(ref: Privat person)"),
                               i = c(0, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = c_lightgrey, size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = NULL
  ) +
  scale_colour_manual(values = c("Privat person" = c_grey,
                                 "Politiker" = c_black, 
                                 "Politiker - Privat person" = c_black),
                      breaks = c("Privat person", "Politiker")) +
  facet_grid(rows = vars(feature), 
             cols = vars(statistic),
             scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

# Eksporteres
ggsave(filename = "m9_diff.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)


##### EKSPLORATIVT: POPULISME OG ELITEAFSNEDER
cj_anova(subset(D_cj, populisme_split == "Høj"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_elite)
m9_amce_popu_h <- cj(subset(D_cj, populisme_split == "Høj"), 
                     model_illiberal
                     ,
                     estimate = "amce",
                     id = ~ ResponseId,
                     by = ~ cj_elite
)
m9_amce_popu_h$statistic <- "AMCE"
m9_amce_popu_h$populisme <- "Høj"

cj_anova(subset(D_cj, populisme_split == "Lav"),
         model_illiberal,
         id = ~ ResponseId,
         by = ~ cj_elite)
m9_amce_popu_l <- cj(subset(D_cj, populisme_split == "Lav"), 
                     model_illiberal
                     ,
                     estimate = "amce",
                     id = ~ ResponseId,
                     by = ~ cj_elite
)
m9_amce_popu_l$statistic <- "AMCE"
m9_amce_popu_l$populisme <- "Lav"


m9_popu_amce <- rbind(m9_amce_popu_l, m9_amce_popu_h) %>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m9_popu_amce, -outcome, -cj_elite), digits = 3),
      include.rownames = FALSE)

ggplot(m9_popu_amce, aes(x = estimate, y = level, colour = BY, group = BY)) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = c_lightgrey, size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = NULL
  ) +
  scale_colour_manual(values = c("Privat person" = c_grey,
                                 "Politiker" = c_black, 
                                 "Politiker - Privat person" = c_black),
                      breaks = c("Privat person", "Politiker")) +
  facet_grid(rows = vars(feature), 
             cols = vars(populisme),
             scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

# Eksporteres
ggsave(filename = "m9_popu_amce.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)


## Marginal Means på populisme = høj
m9_mm_popu_h <- cj(subset(D_cj, populisme_split == "Høj"), 
                   model_illiberal
                   ,
                   estimate = "mm",
                   h0 = 0.5,
                   id = ~ ResponseId,
                   by = ~ cj_elite
)
m9_mm_popu_h$statistic <- "MM"
m9_mm_popu_h$populisme <- "Høj"

m9_mm_diff_popu_h <- cj(subset(D_cj, populisme_split == "Høj"), 
                        model_illiberal
                        ,
                        estimate = "mm_diff",
                        id = ~ ResponseId,
                        by = ~ cj_elite
)
m9_mm_diff_popu_h$statistic <- "MM forskel\n(ref: Privat person)"
m9_mm_diff_popu_h$populisme <- "Høj"


m9_popu_mm <- rbind(m9_mm_popu_h, m9_mm_diff_popu_h) %>% 
  mutate(feature = factor(case_when(feature == "cj_illiberal_dommere" ~ "Udpegning af\ndommere",
                                    feature == "cj_illiberal_inger_mette" ~ "Politikeres brud\nmed loven",
                                    feature == "cj_illiberal_medier" ~ "Mediefrihed"),
                          levels = c("Mediefrihed", 
                                     "Udpegning af\ndommere",
                                     "Politikeres brud\nmed loven")),
         
         level = str_trim(level)
  )

print(xtable(select(m9_popu_mm, -outcome, -cj_elite)[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)], digits = 3),
      include.rownames = FALSE)

ggplot(m9_popu_mm, aes(x = estimate, y = level, colour = BY, group = fct_inorder(BY))) +
  geom_vline(aes(xintercept = i),
             data = data.frame(statistic = c("MM", "MM forskel\n(ref: Privat person)"),
                               i = c(0.5, 0)),
             linetype = "longdash",
             color = c_black) +
  geom_hline(yintercept = 1.5, colour = c_lightgrey, size = .4) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = .5),
                 height = 0.5) +
  geom_point(position = position_dodge(width = .5)) +
  labs(x = NULL,
       y = NULL,
       colour = NULL
  ) +
  scale_colour_manual(values = c("Privat person" = c_grey,
                                 "Politiker" = c_black, 
                                 "Politiker - Privat person" = c_black),
                      breaks = c("Privat person", "Politiker")) +
  facet_grid(rows = vars(feature), 
             cols = vars(statistic),
             scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 14), 
    legend.position = "bottom",
    axis.text=element_text(colour="black",
                           margin=margin(0,0,3,0)),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

# Eksporteres
ggsave(filename = "m9_popu_hoej_mm.pdf",
       #plot = , 
       width = 8, 
       height = 4,
       dpi = 500)
