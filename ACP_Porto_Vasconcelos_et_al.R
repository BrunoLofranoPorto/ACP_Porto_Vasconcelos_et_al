library(MASS)
library(ade4)
library(vegan)
library(car)
library(bbmle)
library(arm)
library(MuMIn)
library(dplyr)
library(ade4)
library("lme4")
library(factoextra)
library(effects)
library(vctrs)

str(PCA_Bruno)
tab<-PCA_Bruno
tab


###PCA

data.frame(tab$possui_CNH,tab$alfabetizados, tab$lotes_regularizados, tab$renda_pct, tab$Ciclovia, tab$rua_arborizada, tab$jardim_ou_parque, tab$praca, tab$academia, tab$quadra_esportiva, tab$calcada_otima)->tab.ord
head(tab.ord)
colnames(tab.ord)<-substr(colnames(tab.ord),5,15)
head(tab.ord)

decostand(tab.ord,"standardize")->tabeladeco
pca<-dudi.pca(tabeladeco, scan=F,scale=F)


##Contribui??o dos eixos
res.pcaB <- prcomp(tabeladeco, scale = T)
summary(res.pcaB)



##Contribui??o das vari?veis
fviz_contrib(res.pcaB, choice = "var", axes = 1:2)
fviz_contrib(res.pcaB, choice = "var", axes = 1:3)
fviz_contrib(res.pcaB, choice = "var") 
fviz_contrib(res.pcaB, choice = "var", axes = 2)

# Calcule a contribuição das variáveis
windows()
(contri_var <- fviz_contrib(res.pcaB, choice = "var"))
(contri_var_12 <- fviz_contrib(res.pcaB, choice = "var", axes = 1:2))
(contri_var_all <- fviz_contrib(res.pcaB, choice = "var", axes = 1:3))
(contri_var_2 <- fviz_contrib(res.pcaB, choice = "var", axes = 2))


# Printar na tela a contribuição relativa para os eixos 1 ou 2 e 1 e 2
# O print dá o valor numérico de contribuição de cada variável
print(contri_var$data)
print(contri_var_12$data)
print(contri_var_all$data)
print(contri_var_2$data)


## Gr?fico - Compartimento

fviz_pca_biplot(res.pcaB, label=c("var"), habillage=tab$Nivel_Renda,
                addEllipses=T, ellipse.level=0.85, geom = "point",
                geom.var = c("arrow","text"), col.var = "black", title = "")

##Help
?fviz_pca_biplot
?fviz_contrib

install.packages("vctrs")
