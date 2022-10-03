############################################################################################################
#                                                                                                          #
#                                                                                                          #
#                      Tarea 4: Tema 4 --- Transformación de los datos --                                  #
#                                                                                                          #
#                                                                                                          #   
############################################################################################################

#############
# Librerías #
#############

suppressMessages(library(readxl))
suppressMessages(library(stringi))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(writexl))
suppressWarnings(library(magrittr))
suppressWarnings(library(dplyr))
suppressWarnings(library(readxl))
suppressWarnings(library(tidyr))
suppressWarnings(library(DT))
suppressWarnings(library(kableExtra))
suppressWarnings(library(formattable))
suppressWarnings(library(knitr))
suppressWarnings(library(kableExtra))
suppressWarnings(library(rpivotTable))
suppressWarnings(library(gt))
suppressWarnings(library(reactable))
suppressWarnings(library(flextable))
suppressWarnings(library(rhandsontable))

install.packages("kableExtra")
devtools::install_github("haozhu233/kableExtra")

################
#  Directorio  #
################

setwd("C:/Users/oscar/Desktop/R --- SAF/Tema 4")

#################
#  Importación  #
#################

SM_2019 <-  suppressWarnings(read_excel("SIGAF_MENSUAL_2019_TRANSFORMADO.xlsx"))



#########################
#        Tabla  1       #
#########################

# Tabla del archivo de datos: SIGAF_MENSUAL_2019_TRANSFORMADO


#####
# A # 
#####

# Mediante el DT


#datatable(SM_2019)

#####
# B # 
#####

# Mediante el reactable

reactable(SM_2019)

# Esta opción es bastante más económica en términos recursos informáticos.

#########################
#        Tabla  2       #
#########################

#  Total de transferencias por mes en el 2019.

T2 <- SM_2019 %>% 
  dplyr::group_by(mes, mes.cod) %>% 
  dplyr::summarise(total = n()) %>% 
  arrange(mes.cod)

#####
# A # 
#####

# Fomarttable 

library(formattable)
formattable(T2)


#####
# B # 
#####

# kabble + kabble Extra

T2 %>% 
  kable %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F)

# T2 %>%  
#      kbl( booktabs = T, caption = "Tabla 2: Total de transferencias por mes en el 2019.",format = "html" ) %>%
#  kable_styling(bootstrap_options = "striped", 
#                full_width = F) %>%
#  footnote(c("SIGAF", "Ministerio de Hacienda")) 

# ver: https://www.rdocumentation.org/packages/kableExtra/versions/1.2.1  
  
#########################
#        Tabla  3       #
#########################

#  Lista de todas las instituciones a nivel de  titulo y programa.

T3.2 <- SM_2019 %>%
  dplyr::group_by(Titulo, Programa) %>%
  dplyr::summarise(`Titulos - Programa` = n()) %>%
  dplyr::arrange(desc(`Titulos - Programa`))

#####
# A # 
#####

# librería gt

T3.2 %>%
  gt() %>%
  tab_header(
    title = "Lista de todas las instituciones:",
    subtitle = "titulo y programa"
  ) %>%
  tab_source_note(
    source_note = " SIGAF"
  ) %>%
  tab_source_note(
    source_note = md("Ministerio de Hacienda")
  )

#####
# B # 
#####

# flextable


T3.2 %>%
  flextable() %>%
  autofit 
 


#########################
#        Tabla  4       #
#########################

# Gasto mensual (devengado) y % de variación del gasto mensual.


T4 <-  SM_2019 %>% 
  dplyr::group_by(mes, mes.cod) %>%  
  dplyr::summarise(`Gasto.mensual` = sum(`G-Devengado`,na.rm=TRUE)) %>%
  dplyr::arrange(mes.cod)

T4 <- as.data.frame(T4)

T4 <- T4 %>% 
  dplyr::mutate( 
    `var.gasto` = (`Gasto.mensual`/lag(`Gasto.mensual`)-1)*100
  )



#####
# A # 
#####

# rhandsontable

T4 %>%
  rhandsontable()

#####
# B # 
#####

# gt

T4 %>%
  gt()%>%
  tab_header(
    title = "Gasto mensual (devengado) y % de variación del gasto mensual."
  ) %>%
  tab_source_note(
    source_note = " SIGAF."
  ) %>%
  tab_source_note(
    source_note = md("Ministerio de Hacienda.")
  )


#########################
#        Tabla  5       #
#########################

#  Gasto total (devengado) de las instituciones a nivel de  titulo y programa.


T5.2 <- SM_2019 %>%
  dplyr::group_by(Titulo, Programa) %>%
  dplyr::summarise( `Gastos totales por instituciones (Programa)`= sum(`G-Devengado`,na.rm=TRUE)) %>%
  dplyr::arrange(desc(`Gastos totales por instituciones (Programa)`))


#####
# A # 
#####

T5.2 %>%
  datatable()

#####
# B # 
#####

T5.2 %>%
  reactable()

