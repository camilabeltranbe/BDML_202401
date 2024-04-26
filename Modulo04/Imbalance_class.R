require("pacman")

p_load("tidyverse",
       "glmnet",
       "caret")
# cargar datos
fraud <- read_csv('https://storage.googleapis.com/download.tensorflow.org/data/creditcard.csv')

# Veamos cómo se distribuye la variable de interés.
fraud<- fraud %>% mutate(Fraude = factor(Class,levels=c(0,1),labels=c("Negativo","Positivo")))
table(fraud$Fraude)
data<- fraud  %>% group_by(Fraude) %>% tally()

ggplot(data,aes(x = Fraude, y = n, fill = Fraude)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Positivo" = "orange", "Negativo" = "blue")) + # Colors can be changed
  labs(x = "", y = "count") # Customize axis labels if needed

# Eliminar las columnas 'Class' 'Time'
fraud<- fraud  %>% select(-Class,-Time)

# Convertir 'Amount' a  logs
fraud<- fraud  %>% mutate(Log_Amount = log(Amount + 0.001)) %>% select(-Amount)

# Dividir los datos en conjuntos de entrenamiento (train) y prueba (test)
set.seed(123) # Para reproducibilidad

train_indices <- as.integer(createDataPartition(fraud$Fraude, p = 0.8, list = FALSE))
train <- fraud[train_indices, ]
test <- fraud[-train_indices, ]

dim(train)

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#logit
set.seed(1410)
fraude_logit_orig <- train(Fraude~., 
                           data = train, 
                           method = "glm",
                           trControl = ctrl,
                           family = "binomial")

fraude_logit_orig

p_load("smotefamily")

predictors<-colnames(train  %>% select(-Fraude))
head( train[predictors])

smote_output <- SMOTE(X = train[predictors],
                      target = train$Fraude)
smote_data <- smote_output$data

table(train$Fraude)
table(smote_data$class)

set.seed(1410)

fraude_logit_smote <- train(class~., 
                            data = smote_data, 
                            method = "glm",
                            trControl = ctrl,
                            family = "binomial")

fraude_logit_smote
