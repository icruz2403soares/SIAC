
# nolint start

library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(tidymodels, warn.conflicts = FALSE)
library(themis, warn.conflicts = FALSE)

Dados <- read.csv2('Teste\\Dados\\Primeiro.csv', sep = ',', dec = '.') %>%

    mutate(Y = as.factor(Y))

Grafico <- Dados %>% ggplot() +

    geom_point(aes(x = A, y = B, color = Y)) +

    theme(legend.position = 'bottom') +

    labs(title = 'Nível de raridade = 0.2')

saveRDS(Grafico, 'Teste\\Gráficos\\Primeiro.RDS')

Dados <- Dados %>%

    mutate(A = A^2, B = B^2)

Particao <- initial_split(Dados, prop = 0.8, strata = Y)

### Conjuntos de Treino e Teste ###

## Padrão ##

Treino_Padrao <- training(Particao)

Teste_Padrao <- testing(Particao)

## SMOTE ##

Treino_SMOTE <- recipe(Y ~ ., data = Treino_Padrao) %>%

    step_smote(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

Teste_SMOTE <- recipe(Y ~ ., data = Teste_Padrao) %>%

    step_smote(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

## BoderlineSMOTE ##

Treino_BSMOTE <- recipe(Y ~ ., data = Treino_Padrao) %>%

    step_bsmote(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

Teste_BSMOTE <- recipe(Y ~ ., data = Teste_Padrao) %>%

    step_bsmote(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

## ADASYN ##

Treino_ADASYN <- recipe(Y ~ ., data = Treino_Padrao) %>%

    step_adasyn(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

Teste_ADASYN <- recipe(Y ~ ., data = Teste_Padrao) %>%

    step_adasyn(Y) %>% prep() %>% 
    
    bake(new_data = NULL)

### Ajusta Modelo ###

## Padrão ##

CLF_Padrao <- glm(

    formula = Y ~ ., 
    
    family = binomial(link = 'logit'), 
    
    data = Treino_Padrao

)

## SMOTE ##

CLF_SMOTE <- glm(

    formula = Y ~ ., 
    
    family = binomial(link = 'logit'), 
    
    data = Treino_SMOTE

)

## BoderlineSMOTE ##

CLF_BSMOTE <- glm(

    formula = Y ~ ., 
    
    family = binomial(link = 'logit'), 
    
    data = Treino_BSMOTE

)

## ADASYN ##

CLF_ADASYN <- glm(

    formula = Y ~ ., 
    
    family = binomial(link = 'logit'), 
    
    data = Treino_ADASYN

)

### Dados Previsão ###

## Padrão ##

Pred_Padrao <- data.frame(Teste = Teste_Padrao %>% pull(Y)) %>% 

    mutate(Pred_Prob = CLF_Padrao %>% predict(Teste_Padrao, type = 'response')) %>%

    mutate(Ponto_Padrao = if_else(Pred_Prob > 0.5, 1, 0) %>% as.factor())

## SMOTE ##

Pred_SMOTE <- data.frame(Teste = Teste_SMOTE %>% pull(Y)) %>% 

    mutate(Pred_Prob = CLF_SMOTE %>% predict(Teste_SMOTE, type = 'response')) %>%

    mutate(Ponto_Padrao = if_else(Pred_Prob > 0.5, 1, 0) %>% as.factor())

## BoderlineSMOTE ##

Pred_BSMOTE <- data.frame(Teste = Teste_BSMOTE %>% pull(Y)) %>% 

    mutate(Pred_Prob = CLF_BSMOTE %>% predict(Teste_BSMOTE, type = 'response')) %>%

    mutate(Ponto_Padrao = if_else(Pred_Prob > 0.5, 1, 0) %>% as.factor())

## ADASYN ##

Pred_ADASYN <- data.frame(Teste = Teste_ADASYN %>% pull(Y)) %>%

    mutate(Pred_Prob = CLF_ADASYN %>% predict(Teste_ADASYN, type = 'response')) %>%

    mutate(Ponto_Padrao = if_else(Pred_Prob > 0.5, 1, 0) %>% as.factor())

### Sensibilidade ###

Sensibilidade <- recall(Pred_Padrao, Teste, Ponto_Padrao, event_level = 'second') %>%

    mutate(Estimador = 'Padrão') %>% rbind(

        recall(Pred_SMOTE, Teste, Ponto_Padrao, event_level = 'second') %>%

            mutate(Estimador = 'SMOTE')
 
    ) %>% rbind(

        recall(Pred_BSMOTE, Teste, Ponto_Padrao, event_level = 'second') %>%

            mutate(Estimador = 'BorderlineSMOTE')

    ) %>% rbind(

        recall(Pred_ADASYN, Teste, Ponto_Padrao, event_level = 'second') %>%

            mutate(Estimador = 'ADASYN')

    ) %>% select(Estimador, .estimate)

write.csv2(Sensibilidade, 'Teste\\Tabelas\\Sensibilidade_P.csv', row.names = FALSE)

### Especificidade ###

Especificidade <- recall(Pred_Padrao, Teste, Ponto_Padrao) %>%

    mutate(Estimador = 'Padrão') %>% rbind(

        recall(Pred_SMOTE, Teste, Ponto_Padrao) %>%

            mutate(Estimador = 'SMOTE')
 
    ) %>% rbind(

        recall(Pred_BSMOTE, Teste, Ponto_Padrao) %>%

            mutate(Estimador = 'BorderlineSMOTE')

    ) %>% rbind(

        recall(Pred_ADASYN, Teste, Ponto_Padrao) %>%

            mutate(Estimador = 'ADASYN')

    ) %>% select(Estimador, .estimate)

write.csv2(Especificidade, 'Teste\\Tabelas\\Especificidade_P.csv', row.names = FALSE)

### Curvas ROC ###

## Padrão ##

Curva_ROC_Padrao <- roc_curve(Pred_Padrao, Teste, Pred_Prob, event_level = 'second') %>%

    mutate(Estimador = 'Padrão')

## SMOTE ##

Curva_ROC_SMOTE <- roc_curve(Pred_SMOTE, Teste, Pred_Prob, event_level = 'second') %>%

    mutate(Estimador = 'SMOTE')

## BoderlineSMOTE ##

Curva_ROC_BSMOTE <- roc_curve(Pred_BSMOTE, Teste, Pred_Prob, event_level = 'second') %>%

    mutate(Estimador = 'BoderlineSMOTE')

## ADASYN ##

Curva_ROC_ADASYN <- roc_curve(Pred_ADASYN, Teste, Pred_Prob, event_level = 'second') %>%

    mutate(Estimador = 'ADASYN')

### Gráficos ###

## SMOTE ##

Grafico_SMOTE <- Curva_ROC_Padrao %>%

    rbind(Curva_ROC_SMOTE) %>% ggplot() +

    geom_path(aes(x = 1 - specificity, y = sensitivity, color = Estimador), linewidth = 0.8) +

    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), linetype = 'dashed', linewidth = 0.8) +

    labs(x = 'TFP', y = 'TVP', title = 'Nível de raridade = 0.2') +

    theme(legend.position = 'bottom')

Grafico_SMOTE %>%

    saveRDS('Teste\\Curva ROC\\SMOTE_P.RDS')

## BoderlineSMOTE ##

Grafico_BSMOTE <- Curva_ROC_Padrao %>%

    rbind(Curva_ROC_BSMOTE) %>% ggplot() +

    geom_path(aes(x = 1 - specificity, y = sensitivity, color = Estimador), linewidth = 0.8) +

    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), linetype = 'dashed', linewidth = 0.8) +

    labs(x = 'TFP', y = 'TVP', title = 'Nível de raridade = 0.2') +

    theme(legend.position = 'bottom')

Grafico_BSMOTE %>%

    saveRDS('Teste\\Curva ROC\\BSMOTE_P.RDS')

## ADASYN ##

Grafico_ADASYN <- Curva_ROC_Padrao %>%

    rbind(Curva_ROC_ADASYN) %>% ggplot() +

    geom_path(aes(x = 1 - specificity, y = sensitivity, color = Estimador), linewidth = 0.8) +

    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), linetype = 'dashed', linewidth = 0.8) +

    labs(x = 'TFP', y = 'TVP', title = 'Nível de raridade = 0.2') +

    theme(legend.position = 'bottom')

Grafico_ADASYN %>%

    saveRDS('Teste\\Curva ROC\\ADASYN_P.RDS')

# nolint end
