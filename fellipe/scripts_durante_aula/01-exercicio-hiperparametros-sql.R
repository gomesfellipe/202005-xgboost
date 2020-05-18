library(tidymodels)
library(tidyverse)
library(tidypredict)

# dados ----------------------------------------------------
data <- tribble(
  ~dose, ~eficacia,
  2, -6,
  8, 4,
  12, 5,
  16, -5
)

# especificacao do modelo ---------------------------------
# mapa dos hiperparâmetros:
#
# tree_depth = tree_depth
# loss_reduction = gamma
# trees = trees
# learn_rate = eta

xgb_model <- boost_tree(
  # Parametros comuns a todos os algoritmos de boost
  # Parametros gerais
  mode = "regression", 
  mtry = 1, # sorteio de colunas
  sample_size = 1, # sorteio de linhas
  min_n = 1, # número mínimo de obs dentro da folha pra considerar fazer a quebra
  
  # -----------------------------------
  # PREENCHA AQUI (solução tem 4 linhas!)
  tree_depth = 2, # profundidade da arvore
  loss_reduction = 0, # gamma (quanto maior o gama arvores mais simples) (gama = zero criar arvores "caprichadas", porem tendem a overfit) (xgboost usa o gain para fazer a poda) (gama = 0  deixa a arvore solta, gama = 1 arvore fica um cotoco)
  learn_rate = 0.3, # quanto menor, mais cautela em incluir novas arvores (inicia com uma learning rate alta pq se n demora muito)
  trees = 2 # n de arores (so para de ajustar quando terminar o n de arvores)
  #-------------------------------------
) %>%
  set_engine("xgboost", # pacote R para ajustar o modelo
             # Parametros especificos para xgboost
             lambda = 0  # mesma coisa do ridge / lasso, quanto maior o lambda mais insensivel  sera o modelo, e poda mais facilmente, apeta o gain, cada vez mais dificil abrir a arvore. labda pequeno arvore muito complexa, labda grande arvore vira um toco
             # uma cautela mais especifica, inclusive levando em conta o n de cada ramo, se o n for baixo e o modelo da um passo mais devagar
  ) 

xgb_fit <- fit(xgb_model, eficacia ~ dose, data = data)
xgb_fit

data %>% mutate(
  pred = predict(xgb_fit, data)$.pred
)

# bonus: SQL ------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "meu_sqlite_db.db")
tidypredict_sql(xgb_fit$fit, con)

copy_to(con, data, "data", overwrite = TRUE)

db_list_tables(con)

data_sql <- tbl(con, "data") %>%
  mutate(
    pred = !!tidypredict_sql(xgb_fit$fit, con)
  )

# resultado
data_sql

# SQL por trás dos panos
show_query(data_sql)


# plot ----------------------------------------------------------------------------------------

library(xgboost)
library(DiagrammeR)
xgboost::xgb.plot.multi.trees(model = xgb_fit$fit)
xgboost::xgb.plot.tree(model = xgb_fit$fit)
