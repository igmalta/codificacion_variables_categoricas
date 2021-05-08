# ==============================================================================
# ONE-HOT-ENCODING
# ==============================================================================

# Train
# ------------------------------------------------------------------------------
dummy_tr <- function(dataset, kcols_cat){
  # One-Hot-Encoding en dataset de entrenamiento
  setDT(dummy_cols(dataset))
  # Se eliminan variables originales
  return(dataset[, c(kcols_cat) := NULL])
}

# Test
# ------------------------------------------------------------------------------
dummy_te <- function(dataset, knames_dummy_tr, kcols_cat){
  # One-Hot-Encoding en dataset de validación/test
  setDT(dummy_cols(dataset))
  # Se eliminan variables originales
  dataset[, c(kcols_cat) := NULL]
  # Restantes variables en train que no están en test
  dif_dummy <- setdiff(knames_dummy_tr, names(dataset))
  # Si dataset train tiene otras variables se agregan a test con "0"
  if(length(dif_dummy) > 0){
    dataset[, c(dif_dummy) := 0]
  } else {
    # Test debe tener las mismas columnas que train
    dataset <- dataset[, knames_dummy_tr, with=F]
  }
  return(dataset)
}

# ==============================================================================
# CODIFICACIÓN DE FRECUENCIA
# ==============================================================================

enc_freq_tr <- function(dataset, kcols_cat){
  # Función contar frecuencia
  contar <- function(x){
    t = data.frame(table(x))
    t$Freq[match(x, t[, 1])] / length(x)
  }
  # Reemplazar originales
  enc_freq <- dataset[, lapply(.SD, contar), .SDcols = kcols_cat]
  
  return(enc_freq)
}

# ==============================================================================
# CODIFICACIÓN DE FRECUENCIA ORDINAL
# ==============================================================================
enc_freq_ord_tr <- function(dataset, kcols_cat){
  # Función contar frecuencia
  # Mayor frecuencia otorga mayor número ordinal
  contar_orden <- function(x){
    working_df      <- data.table(name = x)
    working_df$name <- as.factor(working_df$name)
    summary_count   <- table(x) %>% data.frame()
    colnames(summary_count) <- c("name","count")
    summary_count$name <- as.factor(summary_count$name)
    summary_count <- summary_count %>% arrange(count) %>% 
      mutate(encoded = row_number()) 
    working_df <- working_df %>% left_join(summary_count, by = "name")
    return(working_df$encoded)
  }
  # Reemplazar originales
  enc_freq_ord <- dataset[, paste0(kcols_cat, "_freq_ord") := 
                            lapply(.SD, contar_orden), .SDcols = kcols_cat]
  
  return(enc_freq_ord[, -kcols_cat, with = FALSE])
}

# ============================================================================
# TARGET ENCODING CON SMOOTHING Y CV 
# ============================================================================
#' Se Divide train en folds, se calcula la media en kn-folds y se matchea medias 
#' al n-fold. Después se calcula la media por cada categoría (se unifican los 
#' valores en cada categoría por su media) y esos valores se matchean a test.
#' 
#' Ejemplo:
#' Train (resultado del cv):
#'       a | 1
#'       a | 0
#'       b | 1
#' Train (se unifica en media)
#'       a | 0.5
#'       a | 0.5
#'       b | 1
#' Test (se machea de train, ver "aplicar_codificacion")
#'       a | 0.5
#'       b | 1
#'       c | 0 (primero se crean las columnas con 0 y después se machea)
enc_tgt_tr <- function(dataset, target, kcols_cat, kcv, km){
  #' km = 0 es un target encoding tradicional sin smoothing
  #' km: 1% - 2% de nrow parece razonable
  target_encoding <- function(x) {
    # Join columna con target
    dt <- data.table(x, target)
    # Se crea columna con numeración de folds
    set.seed(123)
    dt[, cv := createFolds(dt[, factor(copy(target))], k = kcv, list = FALSE)]
    # Encoding
    for (i in 1:kcv){
      # Se computa la media global
      mean_general_cv = mean(dt[cv != i, mean(target)])
      # Media y conteo por categorias
      agg_cv <- dt[cv != i, .(count=.N, mean=mean(target)), by=x] 
      # Se computa la media suave
      agg_cv[, smooth:= (count * mean + km * mean_general_cv) / (count + km)]
      # Se agrega columna para machear
      agg_cv [, cv := i]
      # Se pasan resultados a "dt"
      dt[agg_cv, mean := i.smooth, on = .(x, cv)]
    }
    # Se agrupan resultados de pliegues y se calcula la media general por cada categoría
    mean_tr <- dt[, mean(mean,  na.rm=TRUE), by=x]
    setnames(mean_tr, c("x", "mean"))
    # Si hay nulos le corresponde valor 0
    mean_tr[is.na(mean_tr),] <- NA
    # Se pasan resultados a "dt"
    dt[mean_tr, mean := i.mean, on = .(x)]
    
    return(dt[, mean])
  }
  # Codificación
  tgt_enc <- dataset[, lapply(.SD, target_encoding), .SDcols = kcols_cat]
  
  tgt_enc[is.na(tgt_enc),] <- 0
  
  return(tgt_enc)
}

# ============================================================================
# KFOLD TARGET ENCODING 
# ============================================================================
#' A diferencia del anterior no unifica kfolds con una media por categoría, pero
#' se deja el smooth. Valores nulos se imputan con media general de train.


enc_tekcv_tr <- function(dataset, target, kcols_cat, kcv, km, tipo){
  target_encoding <- function(x) {
    # Join columna con target
    dt <- data.table(x, target)
    # Se crea columna con numeración de folds
    set.seed(123)
    dt[, cv := createFolds(dt[, factor(copy(target))], k = kcv, list = FALSE)]
    # Encoding
    for (i in 1:kcv){
      # Se computa la media en los pliegues
      mean_general_cv = mean(dt[cv != i, mean(target)])
      # Media y conteo por categorias
      agg_cv <- dt[cv != i, .(count=.N, mean=mean(target)), by=x] 
      # Se computa la  media suave
      agg_cv[, smooth:= (count * mean + km * mean_general_cv) / (count + km)]
      # Se agrega columna para machear
      agg_cv [, cv := i]
      # Se pasan resultados a "dt"
      dt[agg_cv, mean := i.smooth, on = .(x, cv)]
    }
    # Media general
    mean_tr_gral <- dt[, mean(mean,  na.rm=TRUE)]
    # Si hay nulos corresponde valor de la media general
    dt[is.na(dt),] <- mean_tr_gral 
    
    return(dt[, mean])
  }
  # Codificación
  tgt_enc <- dataset[, paste0(kcols_cat, "_tekcv") := lapply(.SD, target_encoding), 
                     .SDcols = kcols_cat]
  
  return(tgt_enc[, -kcols_cat, with = FALSE])
}

# ============================================================================
# APLICAR CODIFICACIÓN
# ============================================================================
crear_codificacion <- function(dataset, ktipo){
  
  # Se pasa a data.frame para poder dividirlo
  tmp_freq <- data.frame(dataset)#data.frame(dataset[, kcols_freq_ord, with = FALSE])
  # Se divide cada dos columnas y se guarda en una lista
  list_freq <-lapply(1:(ncol(tmp_freq)/2), function(x) tmp_freq[, c(2 * x - 1, 2 * x)])
  rm(tmp_freq); gc()
  # Valores únicos
  list_freq <- lapply(list_freq, unique)
  # Se renombran columnas en cada lista
  list_freq <- lapply(list_freq,  setNames, nm =c("int", "valor"))
  # Se convierte la lista en un data.frame
  codificacion <- do.call(rbind.data.frame, list_freq)
  
  return(codificacion)
}

# Función de match
# ------------------------------------------------------------------------------
match_enc <- function(valores_cat){
  match_codificacion <- function(x){
    valores_cat[chmatch(as.character(x), as.character(valores_cat[, int])), valor]
  }
}
