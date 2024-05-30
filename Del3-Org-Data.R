#----------------------------------------------------------
# INDLÆSNING AF ORGINALT DATASÆT
#----------------------------------------------------------

# Indlæs data
D_org <- read_csv("./data/eksperiment_data_anonymiseret.csv")

D_uddannelse <- read_csv("./data/uddannelse.csv",
                         col_names = F) %>%
  rename(uddannelse = X2) %>% 
  select(-X1) %>% 
  filter(uddannelse != "I alt", 
         uddannelse != "H90 Uoplyst mv.") %>% 
  mutate(antal_dk = rowSums(across(starts_with("X"))),
         uddannelse = case_when(uddannelse == "H10 Grundskole"
                                ~ "Grundskole",
                                uddannelse == "H20 Gymnasiale uddannelser"
                                ~ "Gymnasial uddannelse",
                                uddannelse %in% c("H30 Erhvervsfaglige uddannelser",
                                                  "H35 Adgangsgivende uddannelsesforl\xf8b") 
                                ~ "Erhvervsfaglig uddannelse",
                                uddannelse == "H40 Korte videreg\xe5ende uddannelser, KVU"
                                ~ "Kort videregående uddannelse",
                                uddannelse %in% c("H50 Mellemlange videreg\xe5ende uddannelser, MVU",
                                                  "H60 Bacheloruddannelser, BACH")
                                ~ "Mellemlang videregående uddannelse",
                                uddannelse == "H70 Lange videreg\xe5ende uddannelser, LVU"
                                ~ "Lang videregående uddannelse",
                                uddannelse == "H80 Ph.d. og forskeruddannelser"
                                ~ "Forskeruddannelse"),
         
         uddannelse = factor(uddannelse, c("Grundskole",
                                           "Gymnasial uddannelse",
                                           "Erhvervsfaglig uddannelse",
                                           "Kort videregående uddannelse",
                                           "Mellemlang videregående uddannelse",
                                           "Lang videregående uddannelse",
                                           "Forskeruddannelse")),
         
         pct_dk = antal_dk/sum(antal_dk)
  ) %>% 
  group_by(uddannelse) %>% 
  summarise(antal_dk = sum(antal_dk),
            pct_dk = sum(pct_dk))

D_kommune <- read_csv("./data/kommuner_data.csv", 
                      locale=locale(encoding="latin1"),
                      col_names = F) %>% 
  rename(kommune = X1,
         antal_dk = X2)

D_kommunegrupper <- read_csv2("./data/kommunegrupper.csv") %>% 
  rename(kommune = TITEL) %>% 
  mutate(kommunegruppe = "",
         kommunegruppe = case_when(row_number() %in% 2:25 ~ "Hovedstadskommuner",
                                   row_number() %in% 27:29 ~ "Storbykommuner",
                                   row_number() %in% 31:46 ~ "Provinskommuner",
                                   row_number() %in% 48:71 ~ "Oplandskommuner",
                                   row_number() %in% 73:103 ~ "Landkommuner"
         ),
         kommunegruppe = factor(kommunegruppe, levels = c("Hovedstadskommuner",
                                                          "Storbykommuner", 
                                                          "Provinskommuner", 
                                                          "Oplandskommuner", 
                                                          "Landkommuner")),
         kommunegruppe_split = as.factor(case_when(
           kommunegruppe %in% c("Hovedstadskommuner", "Storbykommuner") 
           ~ "Bykommune",
           kommunegruppe %in% c("Landkommuner", "Oplandskommuner") 
           ~ "Landkommune",
           kommunegruppe == "Provinskommuner" ~ "Provinskommune",
           .default = NA))
  ) %>% 
  select(kommune, kommunegruppe, kommunegruppe_split)

# Tilføje data over regioner
D_kommuner_og_regioner <- tibble(
  kommune = c(
    "Brønderslev", "Frederikshavn", "Hjørring", "Jammerbugt", "Læsø", 
    "Mariagerfjord", "Morsø", "Rebild", "Thisted", "Vesthimmerlands", "Aalborg",
    "Favrskov", "Hedensted", "Herning", "Holstebro", "Horsens", "Ikast-Brande", 
    "Lemvig", "Norddjurs", "Odder", "Randers", "Ringkøbing-Skjern", "Samsø", 
    "Silkeborg", "Skanderborg", "Skive", "Struer", "Syddjurs", "Viborg", 
    "Aarhus", "Assens", "Billund", "Esbjerg", "Fanø", "Fredericia", 
    "Faaborg-Midtfyn", "Haderslev", "Kerteminde", "Kolding", "Langeland", 
    "Middelfart", "Nordfyns", "Nyborg", "Odense", "Svendborg", "Sønderborg", 
    "Tønder", "Varde", "Vejen", "Vejle", "Ærø", "Aabenraa", "Faxe", "Greve", 
    "Guldborgsund", "Holbæk", "Kalundborg", "Køge", "Lejre", "Lolland", 
    "Næstved", "Odsherred", "Ringsted", "Roskilde", "Slagelse", "Solrød", 
    "Sorø", "Stevns", "Vordingborg", "Albertslund", "Allerød", "Ballerup", 
    "Bornholm", "Brøndby", "Dragør", "Egedal", "Fredensborg", "Frederiksberg", 
    "Frederikssund", "Furesø", "Gentofte", "Gladsaxe", "Glostrup", "Gribskov", 
    "Halsnæs", "Helsingør", "Herlev", "Hillerød", "Hvidovre", "Høje-Taastrup", 
    "Hørsholm", "Ishøj", "København", "Lyngby-Taarbæk", "Rudersdal", "Rødovre", 
    "Tårnby", "Vallensbæk"
  ),
  region = c(
    rep("Region Nordjylland", 11), rep("Region Midtjylland", 19), 
    rep("Region Syddanmark", 22), rep("Region Sjælland", 17), 
    rep("Region Hovedstaden", 29)
  )
)

# Rens data
D <- D_org %>%
  rename(kommune = kommune_1,
         aarstal = alder_1,
         ideologi = ideologi_1,
         sekunder = `Duration (in seconds)`,
         politik_dagpenge_ned = politiske_emner_1,
         politik_klima = politiske_emner_2,
         politik_integration = politiske_emner_3,
  ) %>% 
  mutate(across(c("profil_slide1_1", "profil_slide1_2",
                  "profil_slide2_1", "profil_slide2_2",
                  "profil_slide3_1", "profil_slide3_2",
                  "profil_slide4_1", "profil_slide4_2",
                  "profil_slide5_1", "profil_slide5_2",
                  "ideologi", "aarstal", "sekunder"), 
                as.numeric),
         
         blok = case_when(partivalg %in% c("Enhedslisten", "Alternativet", 
                                           "Socialistisk Folkeparti",
                                           "Radikale Venstre", 
                                           "Socialdemokratiet") ~ "Rød blok",
                          partivalg == "Moderaterne" ~ "Midten",
                          partivalg %in% c("Venstre", "Konservative Folkeparti",
                                           "Liberal Alliance", 
                                           "Danmarks Demokraterne", 
                                           "Dansk Folkeparti") ~ "Blå blok",
                          .default = "Andet valg"
         ),
         
         
         partivalg = case_when(partivalg == "Danmarks Demokraterne" ~ "Danmarksdemokraterne",
                               .default = partivalg),
         
         orientering = case_when(ideologi <= 3 ~ "Venstreorienteret",
                                 ideologi %in% 4:6 ~ "Centrumorienteret",
                                 ideologi >= 7 ~ "Højreorienteret"),
         
         uddannelse = case_when(uddannelse %in% c("Ingen uddannelse","Folkeskole/grundskole")
                                ~ "Grundskole",
                                uddannelse == "Kort videregående uddannelse (under 3 år, fx EUD, SOSU, handel og kontor)"
                                ~ "Kort videregående uddannelse",
                                uddannelse == "Mellemlang videregående uddannelse (3-4 år, fx bachelor, HD, sygeplejerske, lærer)"
                                ~ "Mellemlang videregående uddannelse",
                                uddannelse == "Lang videregående uddannelse (5 år eller mere, fx kandidatgrad eller MBA)"
                                ~ "Lang videregående uddannelse",
                                uddannelse == "Forskeruddannelse (fx ph.d.)" 
                                ~ "Forskeruddannelse",
                                .default = uddannelse),
         uddannelse = factor(uddannelse, c("Grundskole",
                                           "Gymnasial uddannelse",
                                           "Erhvervsfaglig uddannelse",
                                           "Kort videregående uddannelse",
                                           "Mellemlang videregående uddannelse",
                                           "Lang videregående uddannelse",
                                           "Forskeruddannelse")),
         
         uddannelse_vu = ifelse(uddannelse=="Kort videregående uddannelse" |
                                  uddannelse=="Mellemlang videregående" |
                                  uddannelse=="Mellemlang videregående uddannelse" |
                                  uddannelse=="Lang videregående uddannelse" |
                                  uddannelse=="Forskeruddannelse", 1, 0),
         
         across(c("politik_dagpenge_ned", "politik_klima", "politik_integration",
                  "demokrati_indeks_1", "demokrati_indeks_2", 
                  "demokrati_indeks_3", "populisme_indeks1_1", 
                  "populisme_indeks1_2", "populisme_indeks1_3", 
                  "populisme_indeks1_4", "populisme_indeks2_1", 
                  "populisme_indeks2_2", "populisme_indeks2_3", 
                  "populisme_indeks2_4",
         ), ~ recode(.x,
                     "Meget uenig" = 0,
                     "Lidt uenig" = 0.25,
                     "Hverken enig eller uenig" = 0.5,
                     "Lidt enig" = 0.75,
                     "Meget enig" = 1)),
         
         indeks_demokrati = (demokrati_indeks_1+demokrati_indeks_2+
                               demokrati_indeks_3)/3,
         
         demo_abstrakt = factor(case_when(demokrati_indeks_1 == 1 ~ "Høj",
                                          demokrati_indeks_1 < 1 ~ "Lav"),
                                levels = c("Lav", "Høj"),
                                ordered = T
         ),
         
         demo_tilfreds = factor(case_when(demokrati_indeks_2 > 0.5 ~ "Høj",
                                          demokrati_indeks_2 == 0.5 ~ "Middel",
                                          demokrati_indeks_2 < 0.5 ~ "Lav"),
                                levels = c("Lav", "Middel", "Høj"),
                                ordered = T
         ),
         
         
         demo_valg = factor(case_when(demokrati_indeks_3 > 0.5 ~ "Høj",
                                      demokrati_indeks_3 == 0.5 ~ "Middel",
                                      demokrati_indeks_3 < 0.5 ~ "Lav"),
                            levels = c("Lav", "Middel", "Høj"),
                            ordered = T
         ),
         
         # koen
         kvinde = (case_when(koen == "Kvinde" ~ 1,
                             koen == "Mand" ~ 0,
                             .default = NA)),
         
         # skalér ideologi 0-1
         ideologi = ideologi/10,
         
         # aldersgrupper
         aldersgruppe = case_when(aarstal %in% 2006:1990 ~ "18-34 år",
                                  aarstal %in% 1989:1975 ~ "35-50 år",
                                  aarstal %in% 1976:1960 ~ "51-65 år",
                                  aarstal < 1960 ~ "65+ år")
  ) %>% 
  filter(aarstal <= 2006,
         uddannelse != "",
         sekunder >= 240) %>% 
  left_join(D_kommuner_og_regioner, by = "kommune") %>% 
  left_join(D_kommunegrupper, by = "kommune")

# Tilføj split variabel på tilslutningen til demokratiet
D$demokrati_split <- ifelse(D$indeks_demokrati >= median(D$indeks_demokrati),
                            "Høj", "Lav")
# Antal observationer
nrow(D)

# Antal observation med køn: "Anden kønsidentitet"
nrow(D[D$koen=="Anden kønsidentitet",]) #n=8

# Antal observation med partivalg: "Andet"
nrow(D[D$partivalg=="Andet",]) #n=16

# Alders dataframe sammenlignet med populationen
D_alder <- read_csv("./data/alder.csv", 
                    locale=locale(encoding="latin1"),
                    col_names = F) %>% 
  select(-X1) %>% 
  rename(alder_dk = X2,
         antal_dk = X3,
         maend = X4,
         kvinder = X5) %>% 
  mutate(alder_dk = str_split_i(alder_dk, " ", 1),
         across(everything(), ~ as.numeric(.x)),
         aarstal = 2024-alder_dk
  ) %>% 
  left_join(D %>% 
              group_by(aarstal) %>% 
              summarize(antal = n()),
            by = "aarstal") %>% 
  mutate(pct_dk = antal_dk/sum(antal_dk),
         pct = antal/sum(antal, na.rm = T))
