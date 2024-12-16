library(plm)
library(pder)
library(sandwich)
library(lmtest)
library(texreg)

# Modelo de Baltagi - Demanda de Gasolina
data(Gasoline)
#data("Gasoline", package = "plm")

pGas <- pdata.frame(Gasoline, index = c("country", "year"))
pdim(pGas)

Gas_Pooling <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas,model = "pooling")

# Regresión Pooling con errores estándar no robustos
summary(Gas_Pooling)

#Efectos Fijos Within
Gas_Within <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas,model ="within")
summary(Gas_Within)

#Efectos temporales
Gas_Time <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas,
                effect = "time")
summary(Gas_Time)

#Two ways
Gas_Twoways <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas,
                   effect = "twoways",model ="within")
summary(Gas_Twoways)

#GLS
Gas_amemiya <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "amemiya")  
summary(Gas_amemiya)

Gas_amemiya2 <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "amemiya",effect = "twoways")  
summary(Gas_amemiya2)

Gas_walhus <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "walhus")  
summary(Gas_walhus)

Gas_swar <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "swar")  
summary(Gas_swar)

Gas_swar2 <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "swar",effect = "twoways")  
summary(Gas_swar2)

Gas_nerlove <- plm(lgaspcar~ lincomep + lrpmg + lcarpcap, data=pGas, model = "random", random.method = "nerlove")  
summary(Gas_nerlove)


# Tests de efectos individuales - F and LMtests
# Compara within one way con Pooling  
pFtest(Gas_Within, Gas_Pooling)

# compara two ways con Pooling:
pFtest(Gas_Twoways, Gas_Pooling)

# Breusch and Pagan (1980) test  
# El primer argumento es el modelo pooling,  
# El segundo el tipo de efecto testeado 
# effect = c("individual", "time", "twoways"), 
# El tercero el tipo de test (BP, Honda, etc.) 
# type = c("honda", "bp", "ghm", "kw"),  
plmtest(Gas_Pooling, effect="individual", type="bp") 
plmtest(Gas_Pooling, effect="time", type="bp") 
plmtest(Gas_Pooling, effect="twoways", type="bp") 

#Honda rechaza para los efectos individual y ambos, pero no para el efecto tiempo
plmtest(Gas_Pooling, effect="individual", type="honda")
plmtest(Gas_Pooling, effect="time", type="honda")
plmtest(Gas_Pooling, effect="twoways", type="honda")

#Kw rechaza para los efectos individual y ambos, pero no para el efecto tiempo
plmtest(Gas_Pooling, effect="individual", type="kw")
plmtest(Gas_Pooling, effect="time", type="kw")
plmtest(Gas_Pooling, effect="twoways", type="kw")

# ghm sólo para two ways, no rechaza la hipotesis nula
plmtest(Gas_Pooling, effect="twoways", type="ghm")

#Mediante el test de Hausman decidir entre el modelo de FE (Efectos Fijos
#- Within) y el modelo RE (Efectos Aleatorios - GLS)  
#Tests por efectos individuales correlacionados con las explicativas
# Hausman test
phtest(Gas_Within, Gas_swar)

# Tests de correlación serial
# Wooldridge Diseñado para detectar efectos individuales no nulos
# también rechaza por autocorrelación
pwtest(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas, effect="individual")

pwtest(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas, effect="time")

# Test BSY El for corre los 3 tests
bsy.LM <- matrix(ncol=3, nrow = 2)
tests <- c("J", "RE", "AR")
dimnames(bsy.LM) <- list(c("LM test", "p-value"), tests)
for(i in tests) {
  mytest <- pbsytest(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas, test = i)
  bsy.LM[1:2, i] <- c(mytest$statistic, mytest$p.value)
}
round(bsy.LM, 6)

# Test de Baltagi y LI
pbltest(lgaspcar~ lincomep + lrpmg + lcarpcap,data=pGas, alternative = "onesided")

# En función de los resultados del test defino el uso o no de una 
# matriz robusta de varianzas y covarianzas 
# para obtener los errores estándar de los coeficientes estimados. 

coeftest(Gas_amemiya, vcov = vcovHC)
coeftest(Gas_walhus, vcov = vcovHC)
coeftest(Gas_swar, vcov = vcovHC)
coeftest(Gas_nerlove, vcov = vcovHC)

# Datos en una tabla

screenreg(list(Gas_Pooling, Gas_Within, Gas_Time, Gas_Twoways,Gas_swar),
          custom.model.names = c("MCO", "Ef. ind", "Ef. Temp","Ambos ef.","Efectos Aleat"),
          caption = "Consumo Gasolina", label = "table:gasoline",
          #custom.gof.names = c("R$ ^ 2$", "Adj. R$ ^ 2$","Num. Obs"),
          digits=5,
          scriptsize = FALSE)
