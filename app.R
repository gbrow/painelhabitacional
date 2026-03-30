library("shiny")
library("bslib")
library("ggplot2")
library("sf")
library("DT")
library("shinycssloaders")
library("geobr")
library("scales") 
library('readxl') #Para carregar arquivo xlsx

#carregando dados de tabela e fazendo download do shapefile
#mapdata <- read.csv("../data/processed/mun_ibge_pop_dpu_ivcad.csv")

#fazendo download dos dados shapefile do ibge
#opção pelo download para deixar os zip dos arquivos mais leve
#mun_ibge_shp_2024 <- read_municipality(year=2024)
#mas o shapefile dos municípios pode ser colocado localmente na pasta ../data/processed
#nesse caso usar comando abaixo
#mun_ibge_shp_2024 <- st_read("../data/processed/mun_ibge_2024.shp")

#agregando dados e shapefile
#mapdata <- left_join(mun_ibge_shp_2024[ , !names(mun_ibge_shp_2024) %in% c("name_muni", "code_state", "abbrev_state", "name_state","code_region", "name_region")], mapdata, by = "code_muni")

mapdata <- st_read("./data/processed/mun_ibge_2024_dom.shp")

print("Colunas disponíveis no shapefile:")
print(names(mapdata))

# Verificar se as colunas necessárias existem
colunas_necessarias <- c("dcmp_pr", "dimp_pr", "dmstd_p", "dfvmp_p", "domtotl", "name_mn")
colunas_faltando <- colunas_necessarias[!colunas_necessarias %in% names(mapdata)]

if(length(colunas_faltando) > 0) {
  print(paste("ATENÇÃO: Colunas faltando:", paste(colunas_faltando, collapse = ", ")))
}

#criando dicionário para título de legenda de acordo com a variável
titles <- c("domtotl" = "Domicílios", 
            "dcomp" = "Domicílios Compostos",
            "dcmp_pr" = "Domicílios Compostos (%)",
            "domestd" = "Domicílios com estrutura degradada",
            "dmstd_p" = "Domicílios com estrutura degradada (%)",
            "dfavimp" = "Domicílios improvidados em favelas",
            "dfvmp_p" = "Domicílios improvidados em favelas (%)",
            "dimp" = "Domicílios improvisados",
            "dimp_pr" = "Domicílios improvisados (%)"
            )
#criando dicionário para cores de valores mínimos  de acordo com a variável para os mapas
lowColors <- c("domtotl" = "lightblue", 
               "dcomp" = "lightblue",
               "dcmp_pr" = "lightblue",
               "domestd" = "lightblue",
               "dmstd_p" = "lightblue",
               "dfavimp" = "lightblue",
               "dfvmp_p" = "lightblue",
               "dimp" = "lightblue",
               "dimp_pr" = "lightblue"
)

# lowColors <- c("pop" = "lightyellow", 
#                "Famílias" = "lightcyan",
#                "Pessoas" = "lightcyan",
#                "per_pop" = "#ddddFF",
#                "IVCAD" = "lightblue",
#                "NC" = "#ffe7b8",
#                "DPI" = "#fffbe6",
#                "DCA" = "#daf5ed",
#                "TQA" = "#ebdaf2",
#                "DR" = "#dbf0ff",
#                "CH" = "#ffebf3"
#                )
#criando dicionário para cores de valores máximos  de acordo com a variável para os mapas


highColors <- c("domtotl" = "darkblue", 
                "dcomp" = "darkblue",
                "dcmp_pr" = "darkblue",
                "domestd" = "darkblue",
                "dmstd_p" = "darkblue",
                "dfavimp" = "darkblue",
                "dfvmp_p" = "darkblue",
                "dimp" = "darkblue",
                "dimp_pr" = "darkblue"
)
# 
# highColors <- c("pop" = "darkred",
#                 "Famílias" = "darkcyan",
#                 "Pessoas" = "darkcyan",
#                 "per_pop" = "#4422bb",
#                 "IVCAD" = "darkblue",
#                 "NC" = "#8a4c01",
#                 "DPI" = "#c4a502",
#                 "DCA" = "#005e50",
#                 "TQA" = "#460263",
#                 "DR" = "#004478",
#                 "CH" = "#63012a"
#                 )
#dicrionário de ajuste de rótulos de legenda de acordo com a variável
accLabel <- c("domtotl" = 1,
                "dcomp" = 1,
                "dcmp_pr" = 0.1,
                "domestd" = 1,
                "dmstd_p" = 0.1,
                "dfavimp" = 1,
                "dfvmp_p" = 0.1,
                "dimp" = 1,
                "dimp_pr" = 0.1
)
#criando layout da página do painel Shiny
ui <- page_fluid(
  #Título
  tags$h2("Painel de prioridades de políticas para questão habitacional"),
  #criando colunas para organizar layout
  layout_columns(
    #seção com mapa e opçao de variáveis e camadas a serem mostradas
    card( card_header(tags$b("Mapemaento e espacialização das variáveis")),
      layout_columns(
        #caixa de seleção das variáveis
      selectInput( 
        "select", 
        "Selecione a varável do mapa:",  
        c("Domicílios" = "domtotl", 
          "Dom. Compostos" = "dcomp",
          "Dom. Comp. (%)" = "dcmp_pr",
          "Dom. com estrut. degr." = "domestd",
          "Dom. com estrut. degr. (%)" = "dmstd_p",
          "Dom. improv. em favelas" = "dfavimp",
          "Dom. improv. em favelas (%)" = "dfvmp_p",
          "Dom. improv." = "dimp",
          "Dom. improv. (%)" = "dimp_pr"
        )
      ),
      #checkbox para mostrar camada de atuação da DPU
        # card(
        #   checkboxInput("checkbox", "Programa Minha Casa, Minha Vida", FALSE)
        #   ),
      #checkbox para mostrar camada dos municípios filtrados
        card(
          checkboxInput("mostrarfiltrados", "Mostrar municípios filtrados", FALSE),
          checkboxInput("mostrarnomes", "Mostrar nomes", FALSE)
          )
        ),
      #plotagem do mapa
      card(
        shinycssloaders::withSpinner(plotOutput("plot"), type = 1)
      )
    ),
    #check box de aplicação dos filtros de prioridade
    card(card_header(tags$b("Filtros de prioridade")),
      checkboxInput("filtro_dcomp", "Domicílios Compostos", TRUE),
      checkboxInput("filtro_dimp", "Domicílios Improvisados", TRUE),
      checkboxInput("filtro_destd", "Estrutura Degradada", TRUE),
      checkboxInput("filtro_dimpfav", "Domicílios Improvisados em Favelas", FALSE),
      checkboxInput("filtro_sel_5", "Cinco municípios com mais domicílios", TRUE)
    ), col_widths = c(10, 2),
  ),
  #tabela com os municípios filtrados
  dataTableOutput("table")
)

#execução do aplicativo
server <- function(input, output) {
  #observeEvent(input$go, {
    #withProgress(message = 'Loading data...', {
      
      # Your long data processing/loading code here
      #mun_ibge_shp_2024 <- read_municipality(year=2024)
      #incProgress(0.5, detail = "Reading file")
      #agregando dados e shapefile
      #mapdata <- left_join(mapdata, mun_ibge_shp_2024[ , !names(mun_ibge_shp_2024) %in% c("name_muni", "code_state", "abbrev_state", "name_state","code_region", "name_region")], by = "code_muni")
      #incProgress(0.5, detail = "Processing data")
    #})
  #})
  
  
  #dados dos municípios com varável reativa, varia de acordo com os filtros
  filtered_data <- reactiveValues(data = mapdata)
  
  #construuindo a tabela de visualização
  output$table <- renderDT({
    # Verificar se filtered_data$data existe e tem dados
    req(filtered_data$data)
    
    # Remover geometria para exibir na tabela
    if (nrow(filtered_data$data) > 0) {
      st_drop_geometry(filtered_data$data)
    } else {
      data.frame(Mensagem = "Nenhum dado encontrado com os filtros selecionados")
    }
  })
  #configurando função observe que atualiza os dados filtrados
  observe({
    #fazendo uma cópia dos dados originais
    filtermap <- mapdata
    
    # APLICAR FILTROS SOMENTE SE AS COLUNAS EXISTIREM
    # Domicílios Compostos
    if ("dcmp_pr" %in% names(filtermap)) {
      if (input$filtro_dcomp) {
        filtermap <- filtermap %>% filter(dcmp_pr > 1.48)
      } else {
        filtermap <- filtermap %>% filter(dcmp_pr < 100)
      }
    } else {
      print("AVISO: Coluna 'dcmp_pr' não encontrada")
    }
    
    # Domicílios Improvisados
    if ("dimp_pr" %in% names(filtermap)) {
      if (input$filtro_dimp) {
        filtermap <- filtermap %>% filter(dimp_pr > 0.222)
      } else {
        filtermap <- filtermap %>% filter(dimp_pr < 100)
      }
    } else {
      print("AVISO: Coluna 'dimp_pr' não encontrada")
    }
    
    # Estrutura Degradada
    if ("dmstd_p" %in% names(filtermap)) {
      if (input$filtro_destd) {
        filtermap <- filtermap %>% filter(dmstd_p > 0.07)
      } else {
        filtermap <- filtermap %>% filter(dmstd_p < 100)
      }
    } else {
      print("AVISO: Coluna 'dmstd_p' não encontrada")
    }
    
    # Domicílios Improvisados em Favelas
    if ("dfvmp_p" %in% names(filtermap)) {
      if (input$filtro_dimpfav) {
        filtermap <- filtermap %>% filter(dfvmp_p > 0.26)
      } else {
        filtermap <- filtermap %>% filter(dfvmp_p < 100)
      }
    } else {
      print("AVISO: Coluna 'dfvmp_p' não encontrada")
    }
    
    #selecionando filtro dos 5 municípios mais populosos
    if (input$filtro_sel_5) {
      if ("domtotl" %in% names(filtermap)) {
        filtered_data$data <- filtermap %>%
          arrange(desc(domtotl)) %>%
          slice_head(n = 5)
      } else {
        filtered_data$data <- filtermap
        print("AVISO: Coluna 'domtotl' não encontrada para ordenar")
      }
    } else {
      filtered_data$data <- filtermap
    }
  })
  #construção do mapa
  output$plot <- renderPlot( 
    { 
      
      while(is(mapdata,"sf") == FALSE) {
        
      }
      #inicialização do gráfico
      p<- ggplot() 
      
      #verifica a marcação dos checkbox, mostrando a variável selecionada
      if (!input$mostrarfiltrados) {
        #Se nenhuma checkbox estiver marcada carrega somente a variável
        p <- p + #configurando camada da variável
          #adicionando camada de acordo com a variável selecionada
          geom_sf(data = mapdata, aes( fill = as.numeric(.data[[input$select]])),  colour = "#00000010" ) +
          #colorindo a camada de acordo com a avariável e os dicionários
          scale_fill_gradient(low=lowColors[input$select],
                              high = highColors[input$select],
                              trans = "log",
                              name=titles[input$select],
                              labels = label_number(accuracy = as.numeric(accLabel[input$select]), big.mark = ".",decimal.mark = ","))
        
      # } else if (input$checkbox && !input$mostrarfiltrados) {
      #   #com apenas a check box da dpu marcada
      #   p <- p + #configurando a camada
      #     #adicionando a camada de acordo com a variável
      #     geom_sf(data = mapdata, aes( fill = as.numeric(.data[[input$select]])),  colour = "#00000010" ) +
      #     #aplicando as cores de acordo com os dicionários
      #     scale_fill_gradient(low=lowColors[input$select],
      #                         high = highColors[input$select],
      #                         trans = "log",
      #                         name=titles[input$select],#aplicando o título de acordo com o dicionário
      #                         labels = label_number(accuracy = 1, big.mark = ".",decimal.mark = ","))
      #     #adicionando camada dos municípios com atuação da DPU
      #     #geom_sf(data = filter(mapdata, Atuacao_DPU == "Sim"), fill = "#ff000010",  colour = "#ff000060")
       } else if (input$mostrarfiltrados) {
        #se apenas a checkbox dos municípios filtrados estiver selecionada
        p <- p + #configurando a camada
          #adicionando a camada de acrodo com a variável
          geom_sf(data = mapdata, aes( fill = as.numeric(.data[[input$select]])),  colour = "#00000010" ) +
          #aplicando as cores de acordo com os dicionários
          scale_fill_gradient(low=lowColors[input$select],
                              high = highColors[input$select],
                              trans = "log",
                              name=titles[input$select],#configurando o título de acordo com os dicionários
                              labels = label_number(accuracy = 1, big.mark = ".",decimal.mark = ","))+
          #adicionando os municípios filtrados em formato de ponto
          geom_sf(data=st_centroid(filtered_data$data), color="white", stroke = 1.5, shape = 21, fill="black")
        #adcionando o nome dos municípios
        if (input$mostrarnomes == TRUE) {
          p <- p + geom_sf_label(data = filtered_data$data, aes(label=name_mn), size = 2.5, label.padding = unit(0.2, "mm"),border.colour = NA, fill = "#ffffff40", position = position_jitter(width = -4, height = 3))
        }
      } else {
        #se estiverem marcadas a atuação da DPU e os municípios filtrados
        p <- p + #configurando a camada
          #adicionando a camada de acordo com a variável
          geom_sf(data = mapdata, aes( fill = as.numeric(.data[[input$select]])),  colour = "#00000010" ) +
          #aplicando as cores de acordo com os dicionários
          scale_fill_gradient(low=lowColors[input$select],
                              high = highColors[input$select],
                              trans = "log",
                              name=titles[input$select],#configurando o rótulo de acordo com os dicionários
                              labels = label_number(accuracy = 1, big.mark = ".",decimal.mark = ","))+
          #adicionando camada da atuação da DPU
          #geom_sf(data = filter(mapdata, Atuacao_DPU == "Sim"), fill = "#ff000010",  colour = "#ff000060")+
          #adicionando camada dos municípios filtrados
          geom_sf(data=st_centroid(filtered_data$data), color="white", stroke = 1.5, shape = 21, fill="black")
        #adicionando nomes dos municípios filtrados
        if (input$mostrarnomes == TRUE) {
          p <- p+ geom_sf_label(data = filtered_data$data, aes(label=name_mn), size = 2.5, label.padding = unit(0.2, "mm"),border.colour = NA, fill = "#ffffff40", position = position_jitter(width = -4, height = 3))
        }
        
        
      }
      #retornando o mapa configurado
      print(p)
    }
  )

}

#comando de execução do Shiny
shinyApp(ui = ui, server = server)
