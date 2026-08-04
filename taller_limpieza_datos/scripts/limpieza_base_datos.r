# -----------------------------------------------------------------------------#
# Taller de GitHub y limpieza básica de datos con dplyr
# Archivo: limpieza_base_datos.R
# -----------------------------------------------------------------------------
#
# Objetivo:
# Observar una base con errores y corregir cada problema de manera explícita
# usando mutate() y recode().
#
# No es necesario construir funciones automáticas.
# -----------------------------------------------------------------------------#

# Ejecute esta línea una sola vez si no tiene instalado tidyverse:
# install.packages("tidyverse")
library(tidyverse)

ruta_entrada <- "../datos/base_sucia_encuesta.txt"
ruta_salida  <- "../resultados/base_limpia.csv"


# 0. Inspección inicial --------------------------------------------------------

# Lea unas pocas líneas sin modificar el archivo.
# ¿Se ven correctamente las tildes, la ñ y los signos especiales?

lineas_iniciales <- readLines(
  ruta_entrada,
  n = 5,
  warn = FALSE
)

print(lineas_iniciales)

# TODO 1:
# Identifique:
# a) el delimitador: ';'
# b) la codificación del archivo: "UTF-8"
# c) las cadenas que representan valores perdidos: "N/D", "-", ""


# 1. Importar la base ----------------------------------------------------------


# TODO 2:
# Complete la importación.
#
# Pistas:
# - La función read_delim() permite indicar el delimitador.
# - locale(encoding = "...") permite indicar la codificación.
# - El argumento na permite definir varias formas de representar faltantes.
# - Conviene importar inicialmente todas las columnas como texto para evitar
#   conversiones automáticas incorrectas.

base <- read_delim(
  file = ruta_entrada,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  na = c("N/D", "-", ""),
  col_types = cols(.default = col_character()),
  trim_ws = FALSE,
  show_col_types = FALSE
)

glimpse(base)
print(base)


# 2. Corregir los nombres ------------------------------------------------------

# Para limpieza del texto debe:
# - eliminar espacios al inicio y al final;
# - estandarizar nombres y ciudades con mayúscula inicial;
# - estandarizar trabaja como "Sí" o "No".
#
# Pistas:
# - str_squish()
# - str_to_title()
# - str_to_lower()
# - case_when()

unique(base$nombre)

# mutate() modifica o crea columnas.
# recode() reemplaza valores específicos por otros valores.

base <- base %>%
  mutate(
    nombre = recode(
      nombre,
      " Ana María López " = "Ana María López",
      "JOSE MUÑOZ"        = "José Muñoz",
      "Lucía Pérez"       = "Lucía Pérez",
      "Andrés Niño"       = "Andrés Niño",
      "María José Gómez"  = "María José Gómez",
      "Camilo Rojas"      = "Camilo Rojas",
      "Sofía León"        = "Sofía León"
    )
  )


# 3. Corregir las ciudades -----------------------------------------------------

unique(base$ciudad)

base <- base %>%
  mutate(
    ciudad = recode(
      ciudad,
      "Bogotá "     = "Bogotá",
      "medellín"    = "Medellín",
      "CALI"        = "Cali",
      "Barranquilla"= "Barranquilla",
      " bogotá"     = "Bogotá",
      "Cartagena"   = "Cartagena",
      "Pereira"     = "Pereira"
    )
  )


# 4. Corregir las fechas -------------------------------------------------------

unique(base$fecha_encuesta)

# Primero, todas las fechas deben quedar escritas como AAAA-MM-DD.

base <- base %>%
  mutate(
    fecha_encuesta = recode(
      fecha_encuesta,
      "03/08/2026"    = "2026-08-03",
      "2026-08-04"    = "2026-08-04",
      "5 agosto 2026" = "2026-08-05",
      "06-08-26"      = "2026-08-06",
      "2026/08/07"    = "2026-08-07",
      "08.08.2026"    = "2026-08-08",
      "08/13/2026"    = "2026-08-13"
    )
  )

# Después, convierta la columna de texto al tipo fecha.

base <- base %>%
  mutate(
    fecha_encuesta = as.Date(
      fecha_encuesta,
      format = "%Y-%m-%d"
    )
  )


# 5. Corregir el ingreso mensual ----------------------------------------------

unique(base$ingreso_mensual)

# Quite manualmente los separadores de miles.
# Use punto únicamente para separar los decimales.

base <- base %>%
  mutate(
    ingreso_mensual = recode(
      ingreso_mensual,
      "1.250.000,50" = "1250000.50",
      "950000.75"    = "950000.75",
      "1,100,000.00" = "1100000.00",
      "875.500,00"   = "875500.00",
      "1 050 000,25" = "1050000.25",
      "725000"       = "725000.00"
    )
  )

# Convertir la columna de texto a número.

base <- base %>%
  mutate(
    ingreso_mensual = as.numeric(ingreso_mensual)
  )


# 6. Corregir la nota promedio -------------------------------------------------

unique(base$nota_promedio)

# Todas las notas deben usar punto como separador decimal.

base <- base %>%
  mutate(
    nota_promedio = recode(
      nota_promedio,
      "4,2" = "4.2",
      "3.8" = "3.8",
      "4,0" = "4.0",
      "3,5" = "3.5",
      "4.5" = "4.5",
      "4,1" = "4.1"
    )
  )

# Convertir la columna de texto a número.

base <- base %>%
  mutate(
    nota_promedio = as.numeric(nota_promedio)
  )


# 7. Corregir la variable trabaja ---------------------------------------------

unique(base$trabaja)

# Todos los valores deben quedar exactamente como "Sí" o "No".

base <- base %>%
  mutate(
    trabaja = recode(
      trabaja,
      "Sí"  = "Sí",
      "si " = "Sí",
      "sí"  = "Sí",
      "No"  = "No",
      "NO"  = "No",
      "no"  = "No"
    )
  )


# 8. Convertir el identificador ------------------------------------------------

base <- base %>%
  mutate(
    id = as.integer(id)
  )


# 9. Revisar el resultado ------------------------------------------------------

print(base)
glimpse(base)
summary(base)


# 10. Comprobaciones automáticas ----------------------------------------------
#
# Estas líneas fueron preparadas por el profesor.
# No es necesario modificarlas.
# Si el trabajo está completo, se ejecutarán sin mostrar errores.

stopifnot(nrow(base) == 7)
stopifnot(length(unique(base$id)) == 7)
stopifnot(inherits(base$fecha_encuesta, "Date"))
stopifnot(is.numeric(base$ingreso_mensual))
stopifnot(is.numeric(base$nota_promedio))
stopifnot(sum(is.na(base$ingreso_mensual)) == 1)
stopifnot(sum(is.na(base$nota_promedio)) == 1)
stopifnot(all(na.omit(base$trabaja) %in% c("Sí", "No")))


# 11. Exportar la base ---------------------------------------------------------

dir.create(dirname(ruta_salida), showWarnings = FALSE)

write_csv(
  base,
  ruta_salida,
  na = ""
)

print(paste("La base limpia fue guardada en", ruta_salida))
