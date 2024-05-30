#----------------------------------------------------------
# CONJOINT DATASÆT
#----------------------------------------------------------

# liste over conjointopgaver til loop
cj_list <- c("1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b", "5a", "5b")

# liste over kolonnenavne med hver conjointopgave
cj_var <- c("__js_traits1a", "__js_traits1b",
            "__js_traits2a", "__js_traits2b",
            "__js_traits3a", "__js_traits3b",
            "__js_traits4a", "__js_traits4b",
            "__js_traits5a", "__js_traits5b")

# liste over variable tilknyttet hver conjointopgave
cj_kol <- c("alder", "koen", "elite", "blok", "pol_dagpenge", "pol_klima",
            "pol_integration", "illiberal_medier", "illiberal_dommere",
            "illiberal_inger_mette")

# dataset til cj
D2 <- D %>% 
  select(ResponseId, sekunder,
         ideologi, orientering, partivalg, blok, aarstal, aldersgruppe, koen, kvinde, 
         uddannelse, uddannelse_vu, kommune, kommunegruppe, kommunegruppe_split, 
         region, politik_dagpenge_ned, politik_klima, politik_integration,
         indeks_demokrati, demokrati_split, populisme_indeks, populisme_split,
         demo_abstrakt, demo_tilfreds, demo_valg,
         profil_valg1_1,
         profil_valg2_1,
         profil_valg3_1,
         profil_valg4_1,
         profil_valg5_1,
         profil_slide1_1, profil_slide1_2,
         profil_slide2_1, profil_slide2_2,
         profil_slide3_1, profil_slide3_2,
         profil_slide4_1, profil_slide4_2,
         profil_slide5_1, profil_slide5_2,
         `__js_traits1a`, `__js_traits1b`,
         `__js_traits2a`, `__js_traits2b`,
         `__js_traits3a`, `__js_traits3b`,
         `__js_traits4a`, `__js_traits4b`,
         `__js_traits5a`, `__js_traits5b`,) %>% 
  rename(`profil_slide_a1` = profil_slide1_1,
         `profil_slide_b1` = profil_slide1_2,
         `profil_slide_a2` = profil_slide2_1, 
         `profil_slide_b2` = profil_slide2_2,
         `profil_slide_a3` = profil_slide3_1, 
         `profil_slide_b3` = profil_slide3_2,
         `profil_slide_a4` = profil_slide4_1, 
         `profil_slide_b4` = profil_slide4_2,
         `profil_slide_a5` = profil_slide5_1, 
         `profil_slide_b5` = profil_slide5_2,
         `profil_valg1` = profil_valg1_1,
         `profil_valg2` = profil_valg2_1, 
         `profil_valg3` = profil_valg3_1, 
         `profil_valg4` = profil_valg4_1, 
         `profil_valg5` = profil_valg5_1,
         koen_respondent = koen)

# loop der splitter hver cnojointopgave-kolonne i kolonner med 1 attribut i hver
for (cj_n in cj_list) {
  
  # midlertidigt dataframe for hver conjointopgave
  D_temp <- do.call(rbind.data.frame,
                    strsplit(D2[[paste("__js_traits", cj_n, sep ="")]],
                             "\\|")
  )
  
  # tilpas navn tilknyttet hver conjointopgave uden prefix
  colnames(D_temp) <- cj_kol
  
  # wrangle data; omkod til factor med meningsfulde labels og værdier
  D_temp <- D_temp %>% 
    mutate(
      illiberal_medier = ifelse(illiberal_medier == 
                                  "Medierne skal være uafhængige af staten.",
                                "Liberal", "Illiberal"),
      illiberal_dommere = ifelse(illiberal_dommere ==
                                   "Dommere skal udpeges uden politisk indblanding.",
                                 " Liberal", " Illiberal"),
      illiberal_inger_mette = ifelse(illiberal_inger_mette ==
                                       "Minksagen med Mette Frederiksen og Barnebrudssagen med Inger Støjberg skal undersøges til bunds.",
                                     "  Liberal", "  Illiberal"),
      
      across(everything(), ~ as.factor(.x))
    )
  
  # tilpas navn tilknyttet hver conjointopgave med korrekt suffix
  colnames(D_temp) <- paste(cj_kol, cj_n, sep = "_")
  
  # tilknyt dataframe
  D2 <- cbind(D2, D_temp)
}

# fjern oprindelige kolonner med samlet conjoint-data
D2 <- select(D2, -starts_with(c("__js_")))

# liste over variable i conjointprofilerne
cj_list1 <- list(
  cj_alder = list(
    ~ alder_1a + alder_2a + alder_3a + alder_4a + alder_5a, 
    ~ alder_1b + alder_2b + alder_3b + alder_4b + alder_5b
  ),
  cj_koen = list(
    ~ koen_1a + koen_2a + koen_3a + koen_4a + koen_5a,
    ~ koen_1b + koen_2b + koen_3b + koen_4b + koen_5b
  ),
  cj_elite = list(
    ~ elite_1a + elite_2a + elite_3a + elite_4a + elite_5a,
    ~ elite_1b + elite_2b + elite_3b + elite_4b + elite_5b
  ),
  cj_blok = list(
    ~ blok_1a + blok_2a + blok_3a + blok_4a + blok_5a,
    ~ blok_1b + blok_2b + blok_3b + blok_4b + blok_5b
  ),
  cj_pol_dagpenge = list(
    ~ pol_dagpenge_1a + pol_dagpenge_2a + pol_dagpenge_3a + pol_dagpenge_4a + pol_dagpenge_5a,
    ~ pol_dagpenge_1b + pol_dagpenge_2b + pol_dagpenge_3b + pol_dagpenge_4b + pol_dagpenge_5b
  ),
  cj_pol_klima = list(
    ~ pol_klima_1a + pol_klima_2a + pol_klima_3a + pol_klima_4a + pol_klima_5a,
    ~ pol_klima_1b + pol_klima_2b + pol_klima_3b + pol_klima_4b + pol_klima_5b
  ),
  cj_pol_integration = list(
    ~ pol_integration_1a + pol_integration_2a + pol_integration_3a + pol_integration_4a + pol_integration_5a,
    ~ pol_integration_1b + pol_integration_2b + pol_integration_3b + pol_integration_4b + pol_integration_5b
  ),
  cj_illiberal_medier = list(
    ~ illiberal_medier_1a + illiberal_medier_2a + illiberal_medier_3a + illiberal_medier_4a + illiberal_medier_5a,
    ~ illiberal_medier_1b + illiberal_medier_2b + illiberal_medier_3b + illiberal_medier_4b + illiberal_medier_5b
  ),
  cj_illiberal_dommere = list(
    ~ illiberal_dommere_1a + illiberal_dommere_2a + illiberal_dommere_3a + illiberal_dommere_4a + illiberal_dommere_5a,
    ~ illiberal_dommere_1b + illiberal_dommere_2b + illiberal_dommere_3b + illiberal_dommere_4b + illiberal_dommere_5b
  ),
  cj_illiberal_inger_mette = list(
    ~ illiberal_inger_mette_1a + illiberal_inger_mette_2a + illiberal_inger_mette_3a + illiberal_inger_mette_4a + illiberal_inger_mette_5a,
    ~ illiberal_inger_mette_1b + illiberal_inger_mette_2b + illiberal_inger_mette_3b + illiberal_inger_mette_4b + illiberal_inger_mette_5b
  ),
  cj_slider = list(
    ~ profil_slide_a1 + profil_slide_a2 + profil_slide_a3 + profil_slide_a4 + profil_slide_a5,
    ~ profil_slide_b1 + profil_slide_b2 + profil_slide_b3 + profil_slide_b4 + profil_slide_b5
  )
)

# liste over task variable
cj_list2 <- list(cj_valg = ~ profil_valg1 + profil_valg2 + profil_valg3 + profil_valg4 + profil_valg5)

# reshape data med cregg pakken
D3 <- cj_tidy(D2,
              profile_variables = cj_list1,
              task_variables = cj_list2,
              id = ~ ResponseId) %>% 
  rename(cj_profile = profile,
         cj_task = task,
         cj_pair = pair) 

D_cj <- as.data.frame(D3) %>% 
  mutate(cj_valgt = ifelse(((cj_valg == "Person A" & cj_profile == "A") |
                              (cj_valg == "Person B" & cj_profile == "B")), 
                           1, 0),
         cj_blok_samme = as.factor(ifelse(blok == cj_blok, 
                                          "Samme blok", "Anden blok")),
         
         kvinde_factor = as.factor(ifelse(kvinde == 1, "Kvinde", "Mand")),
         
         cj_elite = factor(cj_elite, 
                           levels = c("Privat person", "Politiker")),
         
         cj_blok = relevel(cj_blok, "Hverken rød eller blå blok"),
         
         partivalg = factor(partivalg,
                            levels = c("Socialdemokratiet", 
                                       "Radikale Venstre", 
                                       "Konservative Folkeparti", 
                                       "Socialistisk Folkeparti", 
                                       "Moderaterne", 
                                       "Liberal Alliance", 
                                       "Venstre", 
                                       "Dansk Folkeparti", 
                                       "Enhedslisten", 
                                       "Danmarksdemokraterne", 
                                       "Alternativet",
                                       "Andet",
                                       "Blank",
                                       "Ved ikke"),
                            ordered = T),
         
         cj_dagpenge_forskel = as.factor(
           case_when(
             (politik_dagpenge_ned == 0.2 & cj_pol_dagpenge == "Hverken enig eller uenig")
             ~ "Ens holdning",
             (politik_dagpenge_ned <= 0.25 & cj_pol_dagpenge == "Uenig")
             ~ "Ens holdning",
             (politik_dagpenge_ned >= 0.75 & cj_pol_dagpenge == "Enig")
             ~ "Ens holdning",
             .default = "Forskellig holdning")),
         
         cj_klima_forskel = as.factor(
           case_when(
             (politik_klima == 0.2 & cj_pol_klima == "Hverken enig eller uenig")
             ~ " Ens holdning",
             (politik_klima <= 0.25 & cj_pol_klima == "Uenig")
             ~ " Ens holdning",
             (politik_klima >= 0.75 & cj_pol_klima == "Enig")
             ~ " Ens holdning",
             .default = " Forskellig holdning")),
         
         cj_integration_forskel = factor(
           case_when(
             (politik_integration == 0.2 & cj_pol_integration == "Hverken enig eller uenig")
             ~ "  Ens holdning",
             (politik_integration <= 0.25 & cj_pol_integration == "Uenig")
             ~ "  Ens holdning",
             (politik_integration >= 0.75 & cj_pol_integration == "Enig")
             ~ "  Ens holdning",
             .default = "  Forskellig holdning"),
         ),
         
         across(c(cj_dagpenge_forskel, cj_klima_forskel, cj_integration_forskel),
                ~ fct_rev(.x)),
         
         cj_pol_dagpenge = case_when(cj_pol_dagpenge == "Hverken enig eller uenig" 
                                     ~ "Hverken enig eller uenig",
                                     cj_pol_dagpenge == "Uenig" 
                                     ~ "Uenig",
                                     cj_pol_dagpenge == "Enig" 
                                     ~ "Enig"),
         cj_pol_klima = case_when(cj_pol_klima == "Hverken enig eller uenig"
                                  ~ " Hverken enig eller uenig",
                                  cj_pol_klima == "Uenig" 
                                  ~ " Uenig",
                                  cj_pol_klima == "Enig" 
                                  ~ " Enig"),
         cj_pol_integration = case_when(cj_pol_integration == "Hverken enig eller uenig" 
                                        ~ "  Hverken enig eller uenig",
                                        cj_pol_integration == "Uenig" 
                                        ~ "  Uenig",
                                        cj_pol_integration == "Enig"
                                        ~ "  Enig"),
         
         holdningskongruens_indeks = as.numeric(
           ((as.numeric(cj_dagpenge_forskel)-1) +
              (as.numeric(cj_klima_forskel)-1) +
              (as.numeric(cj_integration_forskel)-1)
           )/3),
         
         cj_holdningskongruens_factor = factor(
           case_when(
             holdningskongruens_indeks == 0
             ~ "Helt forskellig",
             holdningskongruens_indeks == 1/3
             ~ "Lidt forskellig",
             holdningskongruens_indeks == 2/3
             ~ "Lidt ens",
             holdningskongruens_indeks == 1
             ~ "Helt ens"),
           levels = c("Helt forskellig", "Lidt forskellig", "Lidt ens", "Helt ens"),
           ordered = T
         ),
         
         cj_holdningskongruens_split = factor(
           case_when(
             cj_holdningskongruens_factor %in% c("Helt ens", "Lidt ens") 
             ~ "Samme",
             cj_holdningskongruens_factor %in% c("Helt forskellig", "Lidt forskellig") 
             ~ "Anden"),
           #levels = c("Ens", "Forskellig"),
           #ordered = T
         ),
         
         uddannelse_vu_num = uddannelse_vu,
         uddannelse_vu = factor((ifelse(uddannelse_vu == 1,
                                        "Ja",
                                        "Nej"))),
         
         cj_task = paste0("Opgave ", cj_task),
         
         across(c(cj_pol_dagpenge, cj_pol_klima, cj_pol_integration, 
                  koen_respondent, cj_task, cj_holdningskongruens_factor,
                  cj_holdningskongruens_split),
                ~ as.factor(.x)),
  ) %>% 
  filter(
    # Fjern respondenter med anden kønsidentitet end mand/kvinde
    koen_respondent != "Anden kønsidentitet"
  )



## get mode of all vars
var_mode <- sapply(D_cj, mode)
## get class of all vars
var_class <- sapply(D_cj, class)
## identify columns that needs be coerced to factors
ind1 <- which(var_mode %in% c("logical", "character"))
## coerce logical / character to factor with `as.factor`
D_cj[ind1] <- lapply(D_cj[ind1], as.factor)
## index of factor columns
fctr <- which(sapply(D_cj, is.factor))
## factor variables that have skipped explicit conversion in step 2
## don't simply do `ind2 <- fctr[-ind1]`; buggy if `ind1` is `integer(0)`
ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
## drop unused levels
D_cj[ind2] <- lapply(D_cj[ind2], droplevels)
## export factor levels actually used by `lm` and `glm`
lev <- lapply(D_cj[fctr], levels)
## count number of levels
nl <- lengths(lev)

# Ændring af referencekategori for enkelte conjointattributter
D_cj$cjblok <- relevel(D_cj$cj_blok, ref = "Hverken rød eller blå blok")
D_cj$cj_illiberal_medier <- relevel(D_cj$cj_illiberal_medier, ref = "Liberal")
D_cj$cj_illiberal_dommere <- relevel(D_cj$cj_illiberal_dommere, ref = " Liberal")
D_cj$cj_illiberal_inger_mette <- relevel(D_cj$cj_illiberal_inger_mette, ref = "  Liberal")
D_cj$cj_pol_dagpenge <- relevel(D_cj$cj_pol_dagpenge, ref = "Hverken enig eller uenig")
D_cj$cj_pol_klima <- relevel(D_cj$cj_pol_klima, ref = " Hverken enig eller uenig")
D_cj$cj_pol_integration <- relevel(D_cj$cj_pol_integration, ref = "  Hverken enig eller uenig")

# labels på attributer
attr(D_cj$cj_alder, "label") <- "Alder"
attr(D_cj$cj_koen, "label") <- "Køn"
attr(D_cj$koen_respondent, "label") <- "Køn (respondent)"
attr(D_cj$cj_elite, "label") <- "Elite"
attr(D_cj$cj_blok_samme, "label") <- "Blok (relativ)"
attr(D_cj$cj_blok, "label") <- "Blok"
attr(D_cj$cj_dagpenge_forskel, "label") <- "Holdning til dagpenge (relativ)"
attr(D_cj$cj_pol_dagpenge, "label") <- "Dagpenge perioden skal sættes ned"
attr(D_cj$cj_klima_forskel, "label") <- "Holdning til klima (relativ)"
attr(D_cj$cj_pol_klima, "label") <- "Klimapolitikken er ikke ambitiøs nok"
attr(D_cj$cj_integration_forskel, "label") <- "Holdning til udlændinge (relativ)"
attr(D_cj$cj_pol_integration, "label") <- "Udlændinge- og integrationspolitikken skal være strammere"
attr(D_cj$cj_holdningskongruens_factor, "label") <- "Holdningskongruens"
attr(D_cj$cj_holdningskongruens_split, "label") <- "Holdningskongruens split"
attr(D_cj$cj_illiberal_medier, "label") <- "Mediefrihed"
attr(D_cj$cj_illiberal_dommere, "label") <- "Udpegning af dommere"
attr(D_cj$cj_illiberal_inger_mette, "label") <- "Politikeres brud med loven"
attr(D_cj$cj_task, "label") <- "Conjointopgave"
attr(D_cj$demo_abstrakt, "label") <- "Vigtigheden af demokratiet"
attr(D_cj$demo_tilfreds, "label") <- "Tilfredshed med demokratiet"
attr(D_cj$demo_valg, "label") <- "Tilslutning til valghandlingen i demokratiet"
attr(D_cj$uddannelse_vu, "label") <- "Har en videregående uddanelse?"

