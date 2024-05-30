#----------------------------------------------------------
# DISKUSSION
#----------------------------------------------------------

# Datasæt
D_disk <- D[D$koen != "Anden kønsidentitet",]

# Sammensætning af aldersgrupper og uddannelse
# Frekvens:
alder_udd_freq <- table(D_disk$uddannelse, D_disk$aldersgruppe)
# Eksporteres til LaTeX
xtable(alder_udd_freq)
# Beregner totaler (indsættes manuelt i LaTeX)
colSums(alder_udd_freq)
rowSums(alder_udd_freq)
round(prop.table(rowSums(alder_udd_freq))*100, digits = 1)
# Beregner procent
alder_udd_pct <- round(prop.table(alder_udd_freq, margin = 2)*100, digits = 1)
# Indættes manuelt LaTeX tabellen 
alder_udd_pct

# Andelen af respondenter med en videregående uddannelse
round(mean(D_disk$uddannelse_vu, na.rm = T)*100, digits = 1)
table(D_disk$uddannelse_vu, D_disk$aldersgruppe)
prop.table(table(D_disk$uddannelse_vu, D_disk$aldersgruppe), margin = 2)

# Sammensætning af køn
round(mean(D$kvinde, na.rm = T)*100, digits = 1)
