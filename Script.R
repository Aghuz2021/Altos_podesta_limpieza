library("dplyr")
library("lubridate")

datos <- read.csv("C://Users//Usuario//Documents//GitHub//Altos_podesta_limpieza//insumo//CC_AP_Download_10-03-2025.csv")


# Eliminar columnas
datos <- datos[, !names(datos) %in% c("mora", 
                                      "tarea",
                                      "obra", "ajuste", 
                                      "cantcuotas", 
                                      "debcred", 
                                      "codadm", 
                                      "cat", 
                                      "valuacion", 
                                      "nromora", 
                                      "impudef",
                                      "recibo","factualiza","marca","caja_are",
                                      "importe_declarado","sistema","fpago_are")]

# Convierte a caracter fpago y fvto

datos$fpago <- as.character(datos$fpago)
datos$fvto <- as.character(datos$fvto)


# Convertir las columnas fpago y fvto a formato Date
datos$fpago <- as.Date(datos$fpago, format="%Y%m%d")
datos$fvto <- as.Date(datos$fvto, format="%Y%m%d")

datos$importe_are <-substr(datos$importe_are, 1, nchar(datos$importe_are) - 2)
datos$importe <-substr(datos$importe, 1, nchar(datos$importe) - 2)
#datos <- datos %>%
#filter(fvto <= Sys.Date())#



View(datos)

library("lubridate")

fechas_invalidas <- datos %>%
  filter(is.na(fvto))

# Definir un rango razonable de fechas
fecha_inicio <- as.Date(datos$fvto,"2000-01-01")
fecha_fin <- as.Date(datos$fvto,"2035-12-31")

# Filtrar fechas fuera del rango
fechas_fuera_rango <- datos %>%
  filter(fvto < fecha_inicio | fvto > fecha_fin)

View(fechas_invalidas)
datos <- datos %>%
  mutate(fvto = if_else(is.na(fvto), as.Date(fpago), fvto),
         fvto = as.Date(paste0(year(fvto), "-", month(fvto), "-15")))


View(datos)

# Crear la nueva columna 'estado'
datos <- datos %>%
  mutate(estado = case_when(
    fpago <= fvto ~ "Pagó a tiempo",
    is.na(fpago) & fvto > Sys.Date() ~ "Aun no pago",
    fpago > fvto ~ "Pago con intereses",
    is.na(fpago) & fvto < Sys.Date() ~ "Mora",
    TRUE ~ NA_character_  # Caso por defecto si ninguna condición se cumple
  ))


datos <- datos %>%
  mutate(importe_are = as.numeric(importe_are))
datos <- datos %>%
  mutate(importe = as.numeric(importe))

# Aplicar la condición y la operación
datos <- datos %>%
  mutate(Pago_con_interes = case_when(
    estado == "Pago con intereses" ~ round(importe * 1.0404 * 1.0404 * 1.0404 * 1.0404),
    estado == "Pagó a tiempo" ~ round(importe_are),
    estado == "Mora" & (Sys.Date() > fvto) ~ round(importe * 1.0404),
    TRUE ~ NA_real_
  ))



# Mostrar las primeras filas del dataframe actualizado




write.csv(datos, "C://Users//Usuario//Documents//GitHub//Altos_podesta_limpieza//resultado//datos_exportados(MARZO_2025).csv", row.names = FALSE)
