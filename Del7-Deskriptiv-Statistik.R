#----------------------------------------------------------
# DESKRIPTIV STATISTIK: VARIABELSAMMENSÆTNING
#----------------------------------------------------------

# Tomt dataframe til brug for tabel over deskriptiv statistik
T1 <- data.frame()

# Liste over variable til deskriptiv statistik
d_var_names <- c("ideologi", "politik_klima", "politik_dagpenge_ned", 
                 "politik_integration", "populisme_indeks", 
                 "demokrati_indeks_1", "demokrati_indeks_2", "demokrati_indeks_3",
                 "kvinde", "aarstal")


# Loop der beregner deskriptiv statistik for hver variabel
for (var_name in d_var_names) {
  # Hent data fra hoved-dataframe
  var_data <- D[[var_name]]
  # Beregn deskriptiv statistik
  var_mean <- mean(var_data, na.rm = T)
  var_median <- median(var_data, na.rm = T)
  var_sd <- sd(var_data, na.rm = T)
  var_min <- min(var_data, na.rm = T)
  var_max <- max(var_data, na.rm = T)
  var_q <- quantile(var_data, na.rm = T)
  
  # Gem data i dataframe
  T1 <- rbind(T1, data.frame(Variabel = var_name, Gns. = var_mean, SD = var_sd, 
                             Min = var_min, Q1 = var_q[[2]], 
                             Median = var_median, Q3 = var_q[[4]], 
                             Max = var_max))
}

# Omdøb variabelnavne
T1 <- T1 %>% 
  mutate(Variabel = case_when(Variabel == "ideologi" ~ "Ideologi",
                              Variabel == "politik_dagpenge_ned" ~ 
                                "Holdning, dagpenge" ,
                              Variabel == "politik_klima" ~ "Holdning, klima",
                              Variabel == "politik_integration" ~ 
                                "Holdning, udlændinge",
                              Variabel == "ideologi" ~ "Ideologi",
                              Variabel == "populisme_indeks" ~ 
                                "Populisme (indeks)",
                              Variabel == "demokrati_indeks_1" ~ 
                                "Demokratistøtte (abstrakt)",
                              Variabel == "demokrati_indeks_2" ~ 
                                "Demokratistøtte (tilfredshed)",
                              Variabel == "demokrati_indeks_3" ~ 
                                "Demokratistøtte (valghandling)",
                              Variabel == "aarstal" ~
                                "Fødselsår",
                              Variabel == "kvinde" ~
                                "Køn"
  )
  )

# Print til LaTeX
stargazer(T1,
          type = "latex",
          style = "apsr",
          label = "t_deskriptiv",
          title = "Deskriptiv statistik",
          rownames = F,
          summary = F)


#----------------------------------------------------------
# DESKRIPTIV STATISTIK: KOMMUNER OG REGIONER
#----------------------------------------------------------

# Stikprøvesammensætning: Geografi
table(D$kommune) 
table(D$kommunegruppe)

# datasæt over kommunerne og regionerne vores respondenter bor i sammenlignet 
# med andel på landspland
D_geo_kommune <- D %>% 
  group_by(kommune) %>% 
  summarise(Antal = n(),
            ideologi = mean(ideologi),
            indeks_demokrati = mean(indeks_demokrati),
            populisme_indeks = mean(populisme_indeks)) %>% 
  left_join(D_kommune, by = "kommune") %>% 
  mutate(pct = Antal/sum(Antal),
         pct_dk = antal_dk/sum(antal_dk),
         forskel = pct-pct_dk)

D_geo_region <- D_geo_kommune %>% 
  left_join(D_kommuner_og_regioner, by = "kommune") %>% 
  group_by(region) %>% 
  summarise(Antal = sum(Antal),
            antal_dk = sum(antal_dk)) %>% 
  mutate(pct = Antal/sum(Antal),
         pct_dk = antal_dk/sum(antal_dk),
         forskel = pct-pct_dk)

D_geo_grupper_temp <- D_geo_kommune %>% 
  left_join(D_kommunegrupper, by = "kommune") %>% 
  group_by(kommunegruppe) %>% 
  summarise(g_Antal = sum(Antal),
            g_antal_dk = sum(antal_dk),
            kommune) %>% 
  mutate(g_pct = g_Antal/sum(g_Antal),
         g_pct_dk = g_antal_dk/sum(g_antal_dk),
         g_forskel = g_pct-g_pct_dk)

D_geo_grupper <- D_geo_kommune %>% 
  left_join(D_geo_grupper_temp, by = "kommune")


#### KOMMUNER

# tabel over kommunegrupper
D_kommunegruppe_tabel <- D_kommunegrupper %>% 
  group_by(kommunegruppe) %>% 
  summarise(Antal = n()) %>% 
  rename(Kommunegruppe = kommunegruppe) %>% 
  filter(Kommunegruppe != "") %>% 
  mutate(Kommunegruppe = as.character(Kommunegruppe))

# print til LaTeX
stargazer(D_kommunegruppe_tabel,
          type = "latex",
          style = "apsr",
          label = "t_kommunegrupper",
          title = "Kommunegrupper",
          rownames = F,
          summary = F)

# tegn kommunekort over respondenterne
mapDK(data = D_geo_kommune,
      values = "Antal",
      id = "kommune") +
  scale_fill_gradient(low = c_lightblue,
                      high = c_blue,
                      na.value = c_black,
                      guide = "legend") +
  labs(fill = "Antal respondenter\npr. kommune") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# eksporter kort som .pdf
ggsave("kommunekort_total.pdf", device = "pdf", width = 9, height = 7)

# kommunekort over forskelle
mapDK(data = D_geo_kommune,
      values = "forskel",
      id = "kommune",
      detail = "municipal") +
  scale_fill_gradient2(low = c_red, 
                       midpoint = 0,
                       mid = c_green, 
                       high = c_blue, 
                       na.value = c_black,
                       labels = label_percent(accuracy = 1),
                       guide = "colourbar",
                       limits = c(-.03, .03),
  ) +
  labs(fill = "Forskel i andel\n(procentpoint)") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# eksporter kort som .pdf
ggsave("kommunekort_forskel_stor.pdf", device = "pdf", width = 7, height = 6)
ggsave("kommunekort_forskel_lille.pdf", device = "pdf", width = 5, height = 4)
#### REGIONER

# tegn regionskort over respondenterne
mapDK(data = D_geo_region,
      values = "Antal",
      id = "region",
      detail = "region") +
  scale_fill_gradient(low = c_lightblue,
                      high = c_blue,
                      na.value = c_black,
                      guide = "legend") +
  labs(fill = "Antal respondenter\npr. region") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# eksporter kort som .pdf
ggsave("regionskort_total.pdf", device = "pdf", width = 9, height = 7)

# regionskort over forskelle
mapDK(data = D_geo_region,
      values = "forskel",
      id = "region",
      detail = "region") +
  scale_fill_gradient2(low = c_red, 
                       midpoint = 0,
                       mid = c_green, 
                       high = c_blue, 
                       na.value = c_black,
                       labels = label_percent(),
                       guide = "legend",
                       limits = c(-.04, .04),
  ) +
  # rediger y-aksen
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                   #breaks = c(-0.2, .-.1, 0, .1, .2),
  #                   limits = c(-.2, .2)
  #) +
  labs(fill = "Forskel i andel\n(procentpoint)") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# eksporter kort som .pdf
ggsave("regionskort_forskel.pdf", device = "pdf", width = 9, height = 7)



### VARIABLE PÅ TVÆRS AF KOMMUNER

# Ideologi
mapDK(data = D_geo_kommune,
      values = "ideologi",
      id = "kommune") +
  scale_fill_gradient2(low = c_red,
                       high = c_blue,
                       midpoint = .5,
                       mid = c_lightgrey,
                       na.value = c_black,
                       guide = "legend") +
  labs(fill = "Ideologi") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# Populisme
mapDK(data = D_geo_kommune,
      values = "populisme_indeks",
      id = "kommune") +
  scale_fill_gradient(low = c_lightgrey,
                      high = c_blue,
                      na.value = c_black,
                      labels = label_percent(),
                      guide = "legend") +
  labs(fill = "Populisme") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

# Vigtigheden af demokratiet
mapDK(data = D_geo_kommune,
      values = "indeks_demokrati",
      id = "kommune") +
  scale_fill_gradient(low = c_lightgrey,
                      high = c_blue,
                      na.value = c_black,
                      labels = label_percent(),
                      guide = "legend") +
  labs(fill = "Vigtigheden af\ndemokratiet") +
  guides(fill = guide_legend(reverse = T)) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))


#----------------------------------------------------------
# DESKRIPTIV STATISTIK: KOMMUNEGRUPPER
#----------------------------------------------------------

# tegn kommunnegruppekort til operationalisering
mapDK(data = D_geo_grupper,
      values = "kommunegruppe",
      id = "kommune",
      detail = "municipal") +
  scale_fill_discrete() +
  scale_fill_manual(values = c(c_purple, c_ku_darkred, c_yellow, c_blue, c_green),
                    #na.translate = FALSE,
                    na.value = c_black,
                    breaks = c("Hovedstadskommuner", "Storbykommuner", 
                               "Provinskommuner", "Oplandskommuner",
                               "Landkommuner")
  ) +
  labs(fill = "Kommunegruppe") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        text = element_text(size = 12))

ggsave("gruppekort_operationalisering_lille.pdf", device = "pdf", width = 5, height = 4)
ggsave("gruppekort_operationalisering_stor.pdf", device = "pdf", width = 7, height = 6)


# sammenlignende bar-plot
D_grupper_barplot <- D_geo_grupper %>% 
  group_by(kommunegruppe) %>% 
  summarise(antal_dk = sum(antal_dk),
            antal = sum(Antal),
  ) %>% 
  mutate(antal_pct = antal/sum(antal),
         antal_pct_dk = antal_dk/sum(antal_dk)) %>% 
  select(-antal,
         -antal_dk) %>% 
  pivot_longer(-kommunegruppe)

# tegn plot
ggplot(D_grupper_barplot, 
       aes(x = forcats::fct_rev(kommunegruppe), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", colour = c_black) +
  # vandret graffen
  coord_flip() +
  # tilpas farver og labels
  scale_fill_manual(values = c(c_lightgrey, c_grey),
                    labels = c("Stikprøve", "Danmark")
  ) +
  # rediger legend
  labs(y = "Andel (%)",
       x = "", color = "", 
       fill = "") +
  # rediger y-aksen
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, .05, .1, .15, .2, .25, .3),
                     limits = c(0, .3)
  ) +
  # fiks retning i legend så den er konsistent
  guides(fill = guide_legend(reverse = T)) +
  # classic theme som grund-tema
  theme_classic() +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black"),
    # tilføj grid-linjer på x-aksen
    panel.grid.major.x = element_line(size=.1, color="grey"),
    # add margins around the plot
    #plot.margin = margin(15,15,15,15),
    
  ) 

# eksporter kort som .pdf
ggsave("kommunegrupper_forskel.pdf", device = "pdf", width = 7, height = 3)



#----------------------------------------------------------
# DESKRIPTIV STATISTIK: IDEOLOGI OG PARTIER
#----------------------------------------------------------

# Ideologisk fordeling på tværs af blokkene
# Rød blok
ggplot(D[D$blok=="Rød blok",], aes(x = ideologi)) +
  geom_histogram(binwidth = .1,
                 fill = c_lightgrey,
                 colour = c_black) +
  theme_classic() +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black")
  ) +
  xlab("Ideologi") +
  #scale_x_continuous(breaks = seq(0, 1, .25), lim = c(-.1, 1.1), name = "Ideologi") +
  scale_y_continuous(breaks = seq(0, 150, 25), lim = c(0, 150), name = "Antal") 


# Eksporter som .pdf
ggsave("ideologi_hist_roed.pdf", device = "pdf", width = 2.5, height = 2.5)

# Blå blok
ggplot(D[D$blok=="Blå blok",], aes(x = ideologi)) +
  geom_histogram(binwidth = .1,
                 fill = c_lightgrey,
                 colour = c_black) +
  theme_classic() +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black")
  ) +
  xlab("Ideologi") +
  #scale_x_continuous(breaks = seq(0, 1, .25), lim = c(-.1, 1.1), name = "Ideologi") +
  scale_y_continuous(breaks = seq(0, 150, 25), lim = c(0, 150), name = "Antal")

# Eksporter som .pdf
ggsave("ideologi_hist_blaa.pdf", device = "pdf", width = 2.5, height = 2.5)


# Midten (Moderaterne)
ggplot(D[D$blok=="Midten",], aes(x = ideologi)) +
  geom_histogram(binwidth = .1,
                 fill = c_lightgrey,
                 colour = c_black) +
  theme_classic() +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black")
  ) +
  xlab("Ideologi") +
  #scale_x_continuous(breaks = seq(0, 1, .25), lim = c(0, 1), name = "Ideologi") +
  scale_y_continuous(breaks = seq(0, 150, 25), lim = c(0, 150), name = "Antal")

# Eksporter som .pdf
ggsave("ideologi_hist_midten.pdf", device = "pdf", width = 2.5, height = 2.5)


#----------------------------------------------------------
# DESKRIPTIV STATISTIK: UDDANNELSE
#----------------------------------------------------------

# Data til sammenlignende bar-plot
D_uddannelse_barplot <- D %>% 
  group_by(uddannelse) %>% 
  summarise(antal = n()) %>% 
  mutate(pct = antal/sum(antal)) %>% 
  left_join(D_uddannelse, by = "uddannelse") %>% 
  select(-antal, -antal_dk) %>% 
  pivot_longer(-uddannelse)

# tegn plot
ggplot(D_uddannelse_barplot, 
       aes(x = forcats::fct_rev(uddannelse), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", colour = c_black) +
  # vandret graffen
  coord_flip() +
  # tilpas farver og labels
  scale_fill_manual(values = c(c_lightgrey, c_grey),
                    labels = c("Stikprøve", "Danmark*")
  ) +
  # rediger legend
  labs(y = "Andel (%)",
       x = "", color = "", 
       fill = "") +
  # rediger y-aksen
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4),
                     limits = c(0, .4)
  ) +
  scale_x_discrete(labels = c("Forsker",
                              "LVU",
                              "MVU",
                              "KVU",
                              "Erhvervsfaglig",
                              "Gymnasial",
                              "Grundskole")
  ) +
  # fiks retning i legend så den er konsistent
  guides(fill = guide_legend(reverse = T)) +
  # classic theme som grund-tema
  theme_classic() +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0)),
    axis.text.y=element_text(colour="black"),
    # tilføj grid-linjer på x-aksen
    panel.grid.major.x = element_line(size=.1, color="grey"),
    # add margins around the plot
    #plot.margin = margin(15,15,15,15),
    
  ) 

# eksporter kort som .pdf
ggsave("uddannelse_forskel.pdf", device = "pdf", width = 7, height = 4)



#----------------------------------------------------------
# DESKRIPTIV STATISTIK: FØDSELSÅR
#----------------------------------------------------------

ggplot(D_alder, aes(x = aarstal)) +
  geom_col(aes(y = pct_dk, fill = "Danmark"), alpha = 1, width = 1, 
           colour = c_black, size = .1) + 
  geom_col(aes(y = pct, fill = "Stikprøve"), alpha = .75, width = 1, 
           colour = c_black, size = .1) +
  theme_bw() +
  scale_y_continuous("Andel (%)", 
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous("Fødselsår",
                     limits = c(1920, 2006),
                     breaks = seq(1920, 2005, 5)) +
  scale_fill_manual(values = c(c_grey, c_lightgrey)) + 
  # fjern titel i legend
  guides(fill=guide_legend(title=NULL)) +
  # tilpas temaet
  theme(
    # generel teksstørrelse
    text = element_text(size = 12), 
    # sort tekstfarve på begge akser + større margener til akse-titler
    axis.text.x=element_text(colour="black", 
                             margin=margin(3,0,8,0),
                             # rotér akse-labels
                             angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black",
                             margin=margin(0,8,0,3))
    
  )

# eksporter graf som .pdf
ggsave("alder_forskel.pdf", device = "pdf", width = 7, height = 4)


