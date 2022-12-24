rm(list = ls())
pacman::p_load(covidmx, lubridate, tidyverse, cowplot, ggtext, glue, 
               MetBrewer, EpiEstim, ggplot2, rtweet)

datos_covid <- descarga_datos_abiertos(show_warnings = FALSE)

variantes   <- descarga_datos_variantes_GISAID("nacional")

#Create dirs
if (!dir.exists("pdf_reports")){
  dir.create("pdf_reports")
}

if (!dir.exists("images")){
  dir.create("images")
}

#____________________________________________________________________________________
#Casos con RT----
#____________________________________________________________________________________

datos_covid <- datos_covid |>
  casos(tipo_clasificacion = c("Sospechosos", "Confirmados COVID")) |>
  estima_rt(min_date           = today() - years(1) - months(1),
            tipo_clasificacion = c("Sospechosos", "Confirmados COVID"),
            method             = "parametric_si",   
            config = EpiEstim::make_config(
              list(
                mean_si = 3.5, 
                std_si = 1.5   
              )
            ))

#Limpiamos del estima_rt los últimas dos semanas porque se cae
datos_covid$estima_rt <- datos_covid$estima_rt |>
  dplyr::filter(FECHA_SINTOMAS <= today() - weeks(2)) |>
  dplyr::filter(FECHA_SINTOMAS >= today() - years(1))

#Filtramos las fechas para coincidir con el RT
datos_covid$casos <- datos_covid$casos |>
  dplyr::filter(FECHA_SINTOMAS >= today() - years(1)) 

#Asignamos semana epidemiológica y año para la coloración y colapsamos por semana
datos_covid$casos <- datos_covid$casos |>
  dplyr::mutate(SEMANA_EPI = epiweek(FECHA_SINTOMAS)) |>
  dplyr::mutate(ANIO_EPI   = epiyear(FECHA_SINTOMAS)) |>
  dplyr::group_by(SEMANA_EPI, ANIO_EPI, ENTIDAD_FEDERATIVA, ABREVIATURA) |>
  dplyr::summarise(n = sum(n), .groups = "keep")

#Unimos la información de variantes
datos_covid$casos <- datos_covid$casos |>
  dplyr::rename(casos_covid = n) |>
  dplyr::left_join(variantes, 
                   by = c("SEMANA_EPI" = "semana", "ANIO_EPI" = "ano")) |>
  dplyr::left_join(
    tibble::tibble(fecha = seq(today() - years(1) - weeks(2), today(), by = "1 day")) |>
      dplyr::mutate(SEMANA_EPI = epiweek(fecha)) |>
      dplyr::mutate(ANIO_EPI =   epiyear(fecha)) |>
      distinct(SEMANA_EPI, ANIO_EPI, .keep_all = T), 
    by = c("SEMANA_EPI", "ANIO_EPI")
  )

univar      <- datos_covid$casos |> ungroup() |> filter(!is.na(variant)) |> distinct(variant) |> unlist()
color_cases <- met.brewer("Cross", n = length(univar))
names(color_cases) <- univar
color_cases <- c(color_cases, "Sin información" = "gray40")

datos_covid$casos <- datos_covid$casos |> 
  mutate(freq    = if_else(is.na(variant), 1.0, freq)) |>
  mutate(n = if_else(is.na(variant), as.double(casos_covid), as.double(n))) |>
  mutate(variant = if_else(is.na(variant), "Sin información", variant)) 
  
  
entidades   <- unique(datos_covid$casos$ENTIDAD_FEDERATIVA) 

#____________________________________________________________________________________
#Ocupación hospitalaria-----
#____________________________________________________________________________________

ocupacion_estado <- descarga_datos_red_irag("Estatal")
ocupacion_UM     <- descarga_datos_red_irag("Unidad Médica")

ocupacion_UM <- ocupacion_UM |> 
  dplyr::filter(Fecha == max(Fecha))

#Obtenemos la ocupación por estado
ocupacion_EDO <- ocupacion_estado |>
  dplyr::mutate(`Hospitalizados (%)` = 
                  dplyr::if_else(`Hospitalizados (%)` > 100 | `Hospitalizados (%)` < 0, 
                                 NA_real_, `Hospitalizados (%)`)) |>
  mutate(Estado = toupper(Estado))

datos_covid <- datos_covid |> 
  casos(
    group_by_entidad = TRUE,
    fecha_tipo       = "Ingreso",
    tipo_paciente    = "HOSPITALIZADO",
    list_name        = "hospitalizados"
  )

#Pegamos en la misma base
datos_covid$hospitalizados <- datos_covid$hospitalizados |>
  filter(FECHA_INGRESO >= today() - years(1)) |>
  dplyr::left_join(ocupacion_EDO, by = c("FECHA_INGRESO" = "Fecha", "ENTIDAD_FEDERATIVA" = "Estado")) 

#Obtenemos los colores
colores     <- met.brewer("Cross")

#____________________________________________________________________________________
#Positividad-----
#____________________________________________________________________________________

datos_covid <- datos_covid |>
  #Calculamos también la positividad
  positividad(
    tipo_prueba          = c("Antigeno", "PCR"),
    group_by_tipo_prueba = TRUE
  )

datos_covid$positividad <- datos_covid$positividad |>
  dplyr::filter(FECHA_SINTOMAS >= today() - years(1))

#____________________________________________________________________________________
#Mortalidad-----
#____________________________________________________________________________________

datos_covid <- datos_covid |>
  casos(
    defunciones      = TRUE,
    fecha_tipo       = "Defunción",
    edad_cut         = c(0, 20, 40, 60, Inf),
    list_name        = "defunciones"
  )

datos_covid$defunciones <- datos_covid$defunciones |>
  dplyr::filter(FECHA_DEF >= today() - years(1))

#ELABORACIÓN DE LOS PLOTS POR ENTIDAD

for (entidad in entidades){

  #CASOS
  #........................................................................................
  mu <- datos_covid$casos |> 
    ungroup() |>
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    summarise(mu = max(casos_covid, na.rm = T)/2) |>
    unlist()
  
  plot_casos <- ggplot() +
    geom_col(aes(x = as.Date(fecha), y = as.numeric(casos_covid)*freq, fill = variant), 
             data = datos_covid$casos |> filter(ENTIDAD_FEDERATIVA == entidad)) +
    #Se multiplica por 40,000 para andar cerca de la media de casos
    geom_line(aes(x = as.Date(FECHA_SINTOMAS), mu[1]*`Median(R)`), 
              data = datos_covid$estima_rt |> filter(ENTIDAD_FEDERATIVA == entidad),
              linetype = "solid", linewidth = 0.7, color = "gray30") + 
    scale_fill_manual("Variante", values = color_cases) +
    labs(
      x = "Fecha de inicio de síntomas",
      y = "Casos sospechosos y confirmados de COVID-19",
      title = "",
      caption = "**Fuente:** GISAID EpiCoV y Github: @RodrigoZepeda/VariantesCovid"
    ) +
    scale_y_continuous(labels = scales::label_comma(),  
                       sec.axis = sec_axis(~ . / mu[1], 
                                           name = "Número efectivo de reproducción (RT)")) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.caption    = element_markdown()
    ) +
    geom_hline(aes(yintercept = mu), linetype = "dashed", color = "gray25") +
    coord_cartesian(xlim = c(min(datos_covid$casos$fecha), max(datos_covid$casos$fecha))) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y", expand = c(0,0))
  

  #OCUPACION HOSP
  #........................................................................................
  #Obtenemos el máximo de pacientes para el reescalamiento
  m_pacientes <- datos_covid$hospitalizados |> 
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    summarise(max = max(n, na.rm = T)) |>
    unlist()
  
  #Reescalamos el porcentaje para que aparezca
  plot_hospitalizados <- datos_covid$hospitalizados |> 
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    ggplot() +
    geom_area(aes(x = as.Date(FECHA_INGRESO), y = `Hospitalizados (%)`/100*m_pacientes),
              fill = colores[8], alpha = 0.25) +
    geom_line(aes(x = as.Date(FECHA_INGRESO), y = as.double(n)), color = colores[1]) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y", expand = c(0,0)) +
    labs(
      x = "", 
      y = "Hospitalizados",
      title = glue("<span style='color:{colores[1]}'>PACIENTES HOSPITALIZADOS</span> Y ",
                   "<span style='color:{colores[8]}'>% DE OCUPACIÓN</span>"),
      caption = glue("**Fuente:** _Github ", 
                     "@RodrigoZepeda/CapacidadHospitalariaMX_")
    ) +
    theme(
      axis.text.x  = element_text(angle = 90, hjust = 1),
      plot.title   = element_markdown(),
      plot.caption = element_markdown()
    ) +
    scale_y_continuous(labels = scales::label_comma(),  
                       sec.axis = sec_axis(~ . / m_pacientes, 
                                           labels  = scales::label_percent(),
                                           name    = "Ocupación de la Red IRAG (%)"))
  
  #OCUPACION UM
  #........................................................................................
  ocupacion_UM_edo <- ocupacion_UM |>
    filter(toupper(Estado) == entidad) |>
    arrange(desc(`Hospitalizados (%)`)) |>
    distinct(`Unidad médica`, .keep_all = T) |>
    mutate(order = 1:n()) |>
    mutate(`Unidad médica` = factor(`Unidad médica`, 
                                    levels = `Unidad médica`[order],
                                    ordered = TRUE)) |>
    dplyr::filter(`Hospitalizados (%)` > 0)
  
  #Reescalamos el porcentaje para que aparezca
  plot_ocupacion <- ggplot(ocupacion_UM_edo) +
    geom_col(aes(x = `Unidad médica`, y = `Hospitalizados (%)`/100, fill = `Hospitalizados (%)`)) +
    labs(
      x = "", 
      y = "Ocupación de la Unidad Médica (%)",
      caption = "**Nota** Se excluyen Unidades Médicas con ocupación del 0% o sin reporte."
    ) + 
    theme_minimal() + 
    scale_fill_gradientn(colours =  met.brewer("Cross", direction = -1)) + 
    coord_flip() +
    theme(
      legend.position = "none",
      axis.text.y     = element_text(size = -4*nrow(ocupacion_UM_edo)/19 + 164/19),
      plot.caption    = element_markdown() ) +
    scale_y_continuous(labels = scales::label_percent())
  
  plot_hosp <- plot_grid(plot_hospitalizados, ggplot() + theme_void(), plot_ocupacion, ncol = 3,
                         rel_widths = c(1, 0.1, 1))
  
  #POSITIVIDAD
  #........................................................................................
  mpruebas         <- datos_covid$positividad |> 
    ungroup() |>
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    summarise(max = max(n_pruebas, na.rm = T)) |>
    unlist()
  
  positividad_plot <- 
    datos_covid$positividad |>
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    ggplot() +
    geom_col(aes(x = as.Date(FECHA_SINTOMAS), 
                 y = as.numeric(n_pruebas), 
                 fill = TIPO_PRUEBA), alpha = 0.25) +
    geom_line(aes(x = as.Date(FECHA_SINTOMAS), 
                  y = Positividad*mpruebas, color = TIPO_PRUEBA)) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y", expand = c(0,0)) +
    labs(
      x = "", 
      y = "Número de pruebas realizadas",
      title = glue("POSITIVIDAD EN ", 
                   "<span style='color:{colores[3]}'>ANTIGENO</span> Y ", 
                   "<span style='color:{colores[6]}'>PCR</span>")
    ) +
    scale_y_continuous(labels = scales::label_comma(),  
                       sec.axis = sec_axis(~ . / mpruebas, 
                                           labels  = scales::label_percent(),
                                           name    = "Positividad (%)")) +
    scale_fill_manual("Tipo de prueba", values = c(colores[3], colores[6])) +
    scale_color_manual("Tipo de prueba", values = c(colores[3], colores[6])) +
    theme(
      axis.text.x     = element_text(angle = 90, hjust = 1),
      legend.position = "none",
      plot.title      = element_markdown()
    )
  
  #DEFUNCIONES
  #........................................................................................
  plot_defunciones <- 
    datos_covid$defunciones |>
    filter(ENTIDAD_FEDERATIVA == entidad) |>
    ggplot() +
    geom_col(aes(x = as.Date(FECHA_DEF), y = as.numeric(n), fill = EDAD_CAT)) +
    facet_wrap(~EDAD_CAT, scales = "free_y") +
    theme_minimal() +
    labs(
      x = "",
      y = "Defunciones",
      title = "DEFUNCIONES"
    ) +
    scale_fill_manual(values = met.brewer("Cross", 4)) +
    scale_y_continuous(labels = scales::label_comma(accuracy = 0.1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y", expand = c(0,0)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "none"
    )
  
  plot_defun_pos <-  plot_grid(positividad_plot, ggplot() + theme_void(), plot_defunciones,
                               rel_widths = c(1, 0.1, 1), ncol = 3)
  
  #Juntamos los plots
  gráfica_sin_titulo <- plot_grid(plot_casos, plot_hosp, plot_defun_pos, ncol = 1)
  
  #Agregamos título
  plot_title <- ggdraw() + 
    draw_label(entidad, fontface='bold', size = 35) +
    draw_label(glue("Actualización {format(today(),'%d/%m/%Y')} | Elaborado en #rstats con el paquete 'covidmx'"), size = 14, y = 0.15,
               color = "gray25")
  
  grafica_completa <- plot_grid(plot_title, gráfica_sin_titulo, ncol = 1, rel_heights = c(0.1, 1))
  ggsave(file.path("pdf_reports", glue("{entidad}.pdf")), grafica_completa, width = 11, height = 14)
  ggsave(file.path("images", glue("{entidad}.png")), grafica_completa, width = 11, height = 14, dpi = 150, bg = "white")
}

datos_covid$disconnect()

#Paste pdf plots
if (require(ghostpdf)){
  fnames <- list.files("pdf_reports", pattern = "*.pdf", full.names = TRUE)
  for (fname in fnames){
    file.rename(fname, str_replace_all(fname," ", "_"))
  }
  pdf_merge(list.files("pdf_reports", pattern = "*.pdf", full.names = TRUE), 
            output_file = "reporte_estatal_covidmx.pdf")
  unlink("pdf_reports/", recursive = TRUE)
}

rtweet::rtweet_bot(
  api_key       =  Sys.getenv("RBOT_TWITTER_CONSUMER_API_KEY"),
  api_secret    =  Sys.getenv("RBOT_TWITTER_CONSUMER_API_SECRET"),
  access_token  =  Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =  Sys.getenv("TWITTER_ACCESS_SECRET")
)

alt_text <- function(x){
  glue("Imagen de la evolución del COVID-19 en {x}. El primer panel
        muestra los casos reportados y la proporción inferida de variantes.
       El segundo panel muestra la ocupación hospitalaria del estado y
       la cantidad de hospitalizados. El tercero las unidades médicas con
       sus respectivas ocupaciones. Finalmente se muestra la positividad
       y las defunciones por grupo de edad.")
}

#Tweet-----
rtweet::post_tweet(status = glue("#covidmx en #{str_remove_all(entidades[1],' ')}"), 
                   display_coordinates = FALSE, 
                   media_alt_text =  alt_text(entidades[1]),
                   media = file.path("images", glue("{entidades[1]}.png")))
cat(entidades[1])
Sys.sleep(5)

my_timeline <- rtweet::get_my_timeline()
reply_id    <- my_timeline$id_str[1]

for (entidad in entidades[2:length(entidades)]){

  Sys.sleep(16*60) #16 minutos
  
  rtweet::post_tweet(status = glue("#covidmx en #{str_remove_all(entidad,' ')}"), 
                     display_coordinates = FALSE, 
                     in_reply_to_status_id = reply_id,
                     media_alt_text =  alt_text(entidad),
                     media = file.path("images", glue("{entidad}.png")))
  cat(entidad)
  
  my_timeline <- rtweet::get_my_timeline()
  reply_id    <- my_timeline$id_str[1]

}


Sys.sleep(10)

rtweet::post_tweet(status = glue("Descarga el pdf con mayor resolución de 
                                 https://raw.githubusercontent.com/RodrigoZepeda/covidmx_bot/main/reporte_estatal_covidmx.pdf"), 
                   display_coordinates = FALSE, 
                   in_reply_to_status_id = reply_id)

