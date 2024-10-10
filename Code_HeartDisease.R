#####
library(corrplot) 
library(ggplot2)
library(gridExtra)
library(heplots) 
library(MASS)
library(tidyverse) 
library(car)
library(class)
library(caret)
library(ROCR)
library(readxl)
#####

data <- read.csv("heart.csv",header=T) 

########### FASE 1. ANALISI PRELIMINARE ############

### VERIFICA DELLA TIPOLOGIA DI VARIABILI

str(data)
data$Sex <- as.factor(data$Sex)
data$ChestPainType <- as.factor(data$ChestPainType)
data$FastingBS <- as.factor(data$FastingBS)
data$RestingECG <- as.factor(data$RestingECG)
data$ExerciseAngina <- as.factor(data$ExerciseAngina)
data$ST_Slope <- as.factor(data$ST_Slope)
data$HeartDisease <- as.factor(data$HeartDisease)
str(data)


### DIVIDIAMO IN TRAINING, VALIDATION E TEST

train_size <- floor(0.80*nrow(data))
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = train_size)
training <- data[train_index, ]
test <- data[-train_index, ]

subtrain_size <- floor(0.65*nrow(training))
subtrain_index <- sample(seq_len(nrow(training)), size = subtrain_size)
sub_training <- training[subtrain_index, ]
sub_training_original <- sub_training
validation <- training[-subtrain_index, ]
validation_original <- validation


### IL PROBLEMA E' BILANCIATO?

# Vediamo se le proporzioni tra le due classi coincidono nei vari dataset
round(prop.table(table(sub_training$HeartDisease)), 2)
round(prop.table(table(validation$HeartDisease)), 2)
round(prop.table(table(test$HeartDisease)), 2)
# bilanciato


############# FASE 2. ANALISI ESPLORATIVE ################

### VERIFICA MISSING 

# Controllo le principali statistiche per le variabili quantitative. 
# Questo mi permetterà di verificare anche la presenza di missing value.
summary(sub_training[, c(1:11)])

# Dal summary vediamo che non ci sono missing 


### ANALISI CORRELAZIONE E DISTRIBUZIONI MARGINALI

# Calcolo correlazione x verificare effetti di multicollinearita' tra gli input 
# Solo per le var quantitative
correlazione <- cor(sub_training[,c(1,4,5,8,10)])
round(correlazione,2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlazione, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
# Le variabili (quantitative) non presentano correlazioni elevate, 
# di conseguenza decidiamo di tenerle tutte nel nostro modello

### VERIFICA DELLA PRESENZA DI VALORI ANOMALI


par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(1,2))
boxplot(sub_training$Cholesterol, main="Cholesterol")
hist(sub_training$Cholesterol, main="Cholesterol")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(sub_training$RestingBP, main="RestingBP")
hist(sub_training$RestingBP, main="RestingBP")
par(mfrow=c(1,1))

# Dall'analisi dei boxplot notiamo che Colesterol, RestingBP e Oldpeak presentano
# più valori anomali.
# Decidiamo di trasfromare con il logaritmo Colesterol e RestingBP, essendo entrambe positive.

# Per i valori pari a 0, decidiamo di imputare al posto di log(0) il log del
# valore minimo assunto da quella variabile

min_nozero <- function(x) {
  min(x[x!=0])
}
lsub_training <- sub_training

for (i in 1:dim(sub_training)[1]) {
  for (j in c(4,5)) {
    if (sub_training[i,j]!=0) {
      lsub_training[i,j] <- log(sub_training[i,j])
    }
    else {
      lsub_training[i,j] <- log(min_nozero(sub_training[,j]))
    }
  }
}

sub_training <- lsub_training

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(1,2))
boxplot(sub_training$Cholesterol, main="Cholesterol")
hist(sub_training$Cholesterol, main="Cholesterol")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(sub_training$RestingBP, main="RestingBP")
hist(sub_training$RestingBP, main="RestingBP")
par(mfrow=c(1,1))

summary(sub_training)


### STANDARDIZZAZIONE
# Le variabili hanno intervalli di variazione diversi tra di loro -> vogliamo renderle 
# confrontabili 

# Variabile di supporto
matrix_indicators <- matrix(0, nrow=(dim(sub_training)[2]-1), ncol=2) 
colnames(matrix_indicators) <- c("mean", "stdev")

# Calcolo media e dev. st.
for (i in c(1,4,5,8,10)){
  matrix_indicators[i, "mean"] <- mean(sub_training[,i])
  matrix_indicators[i, "stdev"] <- sd(sub_training[,i])
}

# Standardizzazione
for (i in c(1,4,5,8,10)){
  sub_training[, i] <- (sub_training[, i] - matrix_indicators[i, "mean"])/matrix_indicators[i, "stdev"]
}

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  boxplot(sub_training[,i],main=colnames(sub_training)[i])
}

par(mfrow=c(3,2))
for (i in c(1,4,5,8,10)) {
  hist(sub_training[,i],main=colnames(sub_training)[i])
}


############ FASE 3. VERIFICA DELLE ASSUNZIONI ##################
# Verifica delle assunzioni per l'applicabilita' della LDA e QDA
# Solo per le var quantitative continue

### 1. NORMALITA'
# Verifichiamo la normalita' condizionata alla classe

# Classe: 1 

variables <- colnames(sub_training)[c(1,4,5,8,10)]

par(mfrow = c(3, 2))
for(i in variables) {
  qqnorm(sub_training[sub_training$HeartDisease == 1, i], main = i); qqline(sub_training[sub_training$HeartDisease == 1, i], col = 2)
}

# Per conferma della non normalità è necessario quindi un test di normalita': 
# test di Shapiro Wilk

# Variabili di supporto
pvalue_shapiro <- matrix(0, nrow = 5, ncol = 2)
rownames(pvalue_shapiro) = colnames(sub_training)[c(1,4,5,8,10)]
colnames(pvalue_shapiro) = c("HeartDisease", "Normal")

# Test Shapiro e costruzione di una matrice riassuntiva con i p-value condizionati alla classe
for (i in colnames(sub_training)[c(1,4,5,8,10)]){
  pvalue_shapiro[i, "HeartDisease"] <- shapiro.test(sub_training[sub_training$HeartDisease == 1, i])$p.value
  pvalue_shapiro[i, "Normal"] <- shapiro.test(sub_training[sub_training$HeartDisease == 0, i])$p.value
}
round(pvalue_shapiro, 5)

# i p-value portano a rifiutare l'ipotesi nulla di normalita'
# Non possiamo applicare LDA e QDA


############# FASE 4. CLASSIFICAZIONE #################

### MODELLI - TRAINING

### REGRESSIONE LOGISTICA

# Modello logistica e applicazione di una stepwise selection
model_logit <- glm(HeartDisease ~., data = sub_training, family = binomial)
summary(model_logit) # AIC = 326.86

step.model <- stepAIC(model_logit, direction = "both", trace = FALSE)
summary(step.model) # AIC = 318.25

# Il modello contiene le variabili: Age, Sex, ChestPainType, Cholesterol, FastingBS,
# Oldpeak, ST_Slope

# Testiamo ora la presenza di punti influenti che potrebbero influenzare il nostro modello.

# Analisi dei punti influenti
par(mfrow=c(1,1))
influencePlot(step.model)

which(row.names(sub_training)==376) #16
which(row.names(sub_training)==786) #29
which(row.names(sub_training)==323) #163
which(row.names(sub_training)==563) #219
which(row.names(sub_training)==326) #270
which(row.names(sub_training)==894) #444

sub_training_pt <- sub_training[-c(16,29,163,219,270,444),]

model_logit2 <- glm(HeartDisease ~., data = sub_training_pt, family = binomial)
summary(model_logit2) # AIC = 300.65
step.model <- stepAIC(model_logit2, direction = "both", trace = FALSE)
summary(step.model) # AIC = 291.89
# = modello migliore con AIC piu' basso

# Il modello contiene le variabili: Age, Sex, ChestPainType, Cholesterol, FastingBS,
# Oldpeak, ST_Slope

# Verifichiamo ora se le variabili esplicative hanno una relazione lineare con log(p1/(1-p1)).

# Calcolo fitted values
probabilities <- predict(step.model, type = "response")
predictors <- c("Age","Cholesterol","Oldpeak")

# Costruzione delle log(p1/(1-p1))
supp <- sub_training_pt[, c(1, 5, 10)]
supp <- supp %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Costruzione dei grafici
ggplot(supp, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


### KNN
# Dopo


### VALUTAZIONE CAPACITA' DI CLASSIFICAZIONE
### VALIDATION

# Prima di tutto dobbiamo applicare le diverse operazioni di cleaning e sistemazione dei dati decise durante il training.

# Trasformazione logaritmica
lvalidation <- validation

for (i in 1:dim(validation)[1]) {
  for (j in c(4,5)) {
    if (validation[i,j]!=0) {
      lvalidation[i,j] <- log(validation[i,j])
    }
    else {
      lvalidation[i,j] <- log(min_nozero(validation[,j]))
    }
  }
}

validation <- lvalidation

# Standardizzazione
for (i in c(1,4,5,8,10)){
  validation[, i] <- (validation[, i] - matrix_indicators[i, "mean"])/matrix_indicators[i, "stdev"]
}


# KNN
# Possiamo utilizzare tutte le variabili numeriche e le qualitative non sconnesse, ovvero
# ChestPainType, RestingECG, ST_Slope

# Definizione dei valori di K da testare
K <- seq(1:100)
accuracy_knn_models <- NULL

sub_training_k <- sub_training[c(1,3,4,5,7,8,10,11,12)]
validation_k <- validation[c(1,3,4,5,7,8,10,11,12)]
sub_training_k$ChestPainType <- as.numeric(sub_training_k$ChestPainType)
sub_training_k$RestingECG <- as.numeric(sub_training_k$RestingECG)
sub_training_k$ST_Slope <- as.numeric(sub_training_k$ST_Slope)
validation_k$ChestPainType <- as.numeric(validation_k$ChestPainType)
validation_k$RestingECG <- as.numeric(validation_k$RestingECG)
validation_k$ST_Slope <- as.numeric(validation_k$ST_Slope)

# Calcolo accuratezza per ogni valore di K
for (k in K){
  model <- knn(sub_training_k[, -9], validation_k[, -9], cl=sub_training_k[, 9], k=k)
  accuracy <-   confusionMatrix(factor(validation_k[, 9]), model)$overall["Accuracy"]
  accuracy_knn_models <- c(accuracy_knn_models, accuracy)
}  

plot(K, accuracy_knn_models, type = "l")

which.max(accuracy_knn_models) # k = 39
max(accuracy_knn_models) # 0.848249

# Modello conclusivo KNN con K = 39
model <- knn(sub_training_k[, -9], validation_k[, -9], cl=sub_training_k[, 9], k=39, prob=TRUE)
confusionMatrix(factor(validation_k[, 9]), model)
# Accuracy = 0.8482
# Sensitivity = 

# Regressione logistica - calcolo delle probabilità a posteriori
pred_logit <- predict(step.model, validation[, -12], type = "response")
# Trasformazioni probabilità in classe con sogli 0.5
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
confusionMatrix(factor(validation[, 12]), factor(as.vector(pred_logit_class)))
# Accuracy = 0.8405 
# Sensitivity = 0.8739
# Specificity = 0.8151

# Valutiamo ora le ROC curves e calcoliamo l’AUC.

pred_roclogit <- prediction(pred_logit, validation[, 12])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit # 0.8955251

prob_knn <- attributes(model)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(model == "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, validation[, 12])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn # 0.8946156

par(mfrow = c(1, 2))
plot(perf_logit, colorize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "39-NN")


### MODELLI - TRAINING + VALIDATION

# Training finale
training <- rbind(sub_training_original, validation_original)
summary(training)

# Trasformazione logaritmica
ltraining <- training

for (i in 1:dim(training)[1]) {
  for (j in c(4,5)) {
    if (training[i,j]!=0) {
      ltraining[i,j] <- log(training[i,j])
    }
    else {
      ltraining[i,j] <- log(min_nozero(training[,j]))
    }
  }
}

training <- ltraining

# Standardizzazione
matrix_indicators <- matrix(0, nrow=(dim(training)[2]-1), ncol=2) 
colnames(matrix_indicators) <- c("mean", "stdev")
for (i in c(1,4,5,8,10)){
  matrix_indicators[i, "mean"] <- mean(training[,i])
  matrix_indicators[i, "stdev"] <- sd(training[,i])
}

for (i in c(1,4,5,8,10)){
  training[, i] <- (training[, i] - matrix_indicators[i, "mean"])/matrix_indicators[i, "stdev"]
}

# Modello logistico
model_logit_final <- glm(HeartDisease ~ Age+Sex+ChestPainType+Cholesterol+FastingBS+Oldpeak+ST_Slope, family = binomial, data = training)
summary(model_logit_final) # AIC = 517.8


# Individuazione valori anomali
par(mfrow=c(1,1))
influencePlot(step.model)

which(row.names(training)==821) #5
which(row.names(training)==703) #133
which(row.names(training)==20) #160
which(row.names(training)==884) #173
which(row.names(training)==316) #400
which(row.names(training)==618) #468

# Valori influenti: 112, 351
training_pt <- training[-c(5,133,160,173,400,468), ]

# Modello finale
model_logit_final <- glm(HeartDisease ~ Age+Sex+ChestPainType+Cholesterol+FastingBS+Oldpeak+ST_Slope, family = binomial, data = training_pt)
summary(model_logit_final) # AIC = 498.85

# L’algoritmo KNN possiamo valutarlo direttamente nella fase successiva.


### MODELLI - TEST

# Sistemiamo il test come da analisi.

# Trasformazione logaritmica
ltest <- test

for (i in 1:dim(test)[1]) {
  for (j in c(4,5)) {
    if (test[i,j]!=0) {
      ltest[i,j] <- log(test[i,j])
    }
    else {
      ltest[i,j] <- log(min_nozero(test[,j]))
    }
  }
}

test <- ltest

# Standardizzazione
for (i in c(1,4,5,8,10)){
  test[, i] <- (test[, i] - matrix_indicators[i, "mean"])/matrix_indicators[i, "stdev"]
}

# Regressione logistica
pred_logit <- predict(model_logit_final, test[, -12], type = "response")
pred_logit_class <- ifelse (pred_logit > 0.5, 1, 0) 
confusionMatrix(factor(test[, 12]), factor(as.vector(pred_logit_class)))
# accuracy 0.8967 
# Sensitivity 0.9136 
# Specificity 0.8835

# KNN

training_k <- training[c(1,3,4,5,7,8,10,11,12)]
training_k$ChestPainType <- as.numeric(training_k$ChestPainType)
training_k$RestingECG <- as.numeric(training_k$RestingECG)
training_k$ST_Slope <- as.numeric(training_k$ST_Slope)
test_k <- test[c(1,3,4,5,7,8,10,11,12)]
test_k$ChestPainType <- as.numeric(test_k$ChestPainType)
test_k$RestingECG <- as.numeric(test_k$RestingECG)
test_k$ST_Slope <- as.numeric(test_k$ST_Slope)

model <- knn(training_k[, -9], test_k[, -9], cl=training_k[, 9], k=39, prob=TRUE)
confusionMatrix(factor(test_k[, 9]), model)
# Accuracy = 0.8315
# Sensitivity = 0.7895
# Specificity = 0.8764


pred_roclogit <- prediction(pred_logit, test_k[, 9])
perf_logit <- performance(pred_roclogit,"tpr","fpr")
auc_logit <- performance(pred_roclogit, measure = "auc")@y.values
auc_logit # 0.9304699

prob_knn <- attributes(model)$prob
# Trasformazione delle proporzioni (proporzione di elementi su K che appartengono alla classe 0) che il KNN resitutisce in probabilità
prob_knn <- 2*ifelse(model == "0", 1-prob_knn, prob_knn) - 1
pred_rocknn <- prediction(prob_knn, test_knn[, 22])
perf_knn<- performance(pred_rocknn,"tpr","fpr")
auc_knn <- performance(pred_rocknn, measure = "auc")@y.values
auc_knn # 0.8946156

par(mfrow = c(1, 2))
plot(perf_logit, colorize = TRUE, main = "Regressione Logistica")
plot(perf_knn, colorize = TRUE, main = "39-NN")


