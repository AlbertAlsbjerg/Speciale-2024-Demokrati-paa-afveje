#----------------------------------------------------------
# ROBUSTHEDSTEST AF INDEKSKONSTRUKTIONER
#----------------------------------------------------------
# Til at beregne parvise korrelationer bruges følgende metode fundet fra:
# http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#----------
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(ifelse(p < .001, "$^{***}$ ", ifelse(p < .01, "$^{**}$  ", ifelse(p < .05, "$^{*}$   ", "    "))))
  
  ## trunctuate the correlation matrix to three decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex", sanitize.text.function = function(x) x) 
  }
} 
#----------

# Pearson's r korrelationstest af populisme indekset
D_popu <- D %>% 
  select("populisme_indeks1_1", 
         "populisme_indeks1_2", "populisme_indeks1_3", 
         "populisme_indeks1_4", "populisme_indeks2_1", 
         "populisme_indeks2_2", "populisme_indeks2_3", 
         "populisme_indeks2_4") %>% 
  mutate(populisme_indeks1_3 = (populisme_indeks1_3*-1)+5)
ggpairs(D_popu)

# Populisme-indeks: Pearson's r korelation 
popu_res <- rcorr(as.matrix(D_popu),
                  type = "pearson") 
popu_res

# Tester Pearson's r igen og printer til LaTeX tabel
corstars(D_popu, method = "pearson", removeTriangle = "upper", result = "latex")

# Chronbach's Alpha test af populismeindekset
cronbach.alpha(D_popu)
# 0.694
# Det er næsten tilstrækkeligt, men Pearsons R er ikke god nok.

# Lever samlet ikke op til kriterierne. Vi fjerner de sidste 3 indikatorer


ggpairs(D_popu[, c("populisme_indeks1_1", 
                   "populisme_indeks1_2", "populisme_indeks1_3", 
                   "populisme_indeks1_4", "populisme_indeks2_1")])
# Er nu tilstrækkelig

# Nyt indeks
D_popu2 <- D_popu[, c("populisme_indeks1_1", 
                      "populisme_indeks1_2", "populisme_indeks1_3", 
                      "populisme_indeks1_4", "populisme_indeks2_1")] %>% 
  rename(`$P_1$` = populisme_indeks1_1,
         `$P_2$` = populisme_indeks1_2,
         `$P_3$` = populisme_indeks1_3,
         `$P_4$` = populisme_indeks1_4,
         `$P_5$` = populisme_indeks2_1,)

# Tester Pearson's r igen og printer til LaTeX tabel
corstars(D_popu2, method = "pearson", removeTriangle = "upper", result = "latex")

# Chronbach's Alpha test af populismeindekset
cronbach.alpha(D_popu2)
# 0.747
# Det er tilstrækkeligt

# Tilføjer indeks til datasæt
D <- D %>% 
  mutate(populisme_indeks = (populisme_indeks1_1+populisme_indeks1_2+
                               populisme_indeks1_3+(populisme_indeks1_3*-1)+5+
                               populisme_indeks1_4+populisme_indeks2_1)/24) 
D <- D %>% 
  mutate(populisme_split = factor(ifelse(populisme_indeks < median(D$populisme_indeks, na.rm = T),
                                         "Lav", "Høj"),
                                  levels = c("Lav", "Høj")
  )
  )

# Opbakning til demokratiet, indeks:
D_demo1 <- select(D, demokrati_indeks_1, demokrati_indeks_2, demokrati_indeks_3)
D_demo2 <- select(D, demokrati_indeks_1, demokrati_indeks_3)

corstars(D_demo1, "pearson", "upper", "latex")
rcorr(as.matrix(D_demo2), type = "pearson") 

cronbach.alpha(D_demo1)
cronbach.alpha(D_demo2)

