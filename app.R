library(shiny)
library(bslib)
library(shinyjs)
library(waiter)
library(shinyalert)
library(dplyr)
library(tidyr)
library(purrr)
library(colourpicker)
library(reactable)
library(ggplot2)

# Sourcing file structure and dependencies
source("R/data_parser.R")
source("R/module_question_block.R")
source("R/module_export_builder.R")

# Definindo um tema global da aplicação
app_theme <- bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#0d6efd"
)

# ===== Interface do Usuário (Esqueleto Vazio) =====
ui <- page_navbar(
    title = "Gerador Automático de Relatórios PPTX",
    id = "main_nav",
    theme = app_theme,
    sidebar = NULL,

    # Inicialização de bibliotecas javascript e loaders
    header = tagList(
        useShinyjs(),
        useWaiter()
    ),

    nav_spacer(),
    nav_item(actionButton(
        "btn_open_save_load",
        "Sessão (.rds)",
        icon = icon("save"),
        class = "btn-info btn-sm mt-2"
    )),

    # --- Página 1: Base de Dados ---
    nav_panel(
        title = "Base de Dados",
        value = "page_base",
        div(
            class = "container mt-3",
            h3("Upload da Base de Dados"),
            p(
                "Por favor, selecione seu arquivo (.xlsx ou .csv). O formato exigido consiste em 3 linhas de cabeçalho antes dos dados reais."
            ),
            fileInput(
                "file_upload",
                "Escolher arquivo de dados",
                accept = c(".csv", ".xlsx", ".xls")
            ),
            hr(),
            # Usaremos DT (que não precisa de import adicional como o reactable) ou reactable
            # Mas o ideal é reactable se ele quiser. Vamos deixar DT como placeholder para a tabela.
            # Vou adicionar reactable no import
            reactable::reactableOutput("table_data_preview")
        )
    ),

    # --- Página 2: Personalização (Cores) ---
    nav_panel(
        title = "Personalização (Cores)",
        value = "page_cores",
        div(
            class = "container mt-3",
            id = "content_cores", # Para uso com shinyjs
            h3("Paleta de Cores e Gráficos de Exemplo"),
            p("Selecione a paleta principal de cores para a sua empresa."),
            # Usar colourpicker (requer adicionar o pacote)
            layout_columns(
                col_widths = c(4, 8),
                card(
                    card_header("Definir Cores"),
                    colourpicker::colourInput(
                        "color_primary",
                        "Cor Primária",
                        value = "#0d6efd"
                    ),
                    colourpicker::colourInput(
                        "color_secondary",
                        "Cor Secundária",
                        value = "#6c757d"
                    ),
                    colourpicker::colourInput(
                        "color_tertiary",
                        "Cor Terciária",
                        value = "#198754"
                    ),
                    colourpicker::colourInput(
                        "color_quaternary",
                        "Cor Quartenária",
                        value = "#dc3545"
                    ),
                    actionButton(
                        "btn_confirm_colors",
                        "Confirmar e Prosseguir Visualizações",
                        class = "btn-success mt-3",
                        width = "100%"
                    )
                ),
                card(
                    card_header("Gráficos de Exemplo Reativos"),
                    plotOutput("plot_example_bar"),
                    plotOutput("plot_example_pie")
                )
            )
        )
    ),

    # --- Página 3: Configuração de Visualizações ---
    nav_panel(
        title = "Configuração de Gráficos/Tabelas",
        value = "page_viz",
        div(
            class = "container mt-3",
            id = "content_viz", # Para uso com shinyjs
            h3("Montagem Dinâmica de Visualizações por Questão"),
            p(
                "Adicione blocos abaixo para cada gráfico ou tabela que deseja incluir no relatório. Você poderá configurar a variável de análise, os dados filtrados e a formatação individual."
            ),
            actionButton(
                "btn_add_block",
                "Adicionar Novo Bloco de Informação",
                class = "btn-primary",
                icon = icon("plus")
            ),
            hr(),
            div(id = "ui_blocks_container")
        )
    ),

    # --- Página 4: Exportação (Ordem e Layout) ---
    nav_panel(
        title = "Exportação de PPTX",
        value = "page_export",
        div(
            class = "container mt-3",
            id = "content_export", # Para uso com shinyjs
            h3("Gestão de Lâminas e Geração de PPTX"),
            p(
                "Aqui você define a ordem em que os gráficos e tabelas aparecerão no PowerPoint final."
            ),

            exportBuilderUI("export_tab")
        )
    )
)

# ===== Servidor =====
server <- function(input, output, session) {
    # --- Variáveis Reativas Globais ---
    rv <- reactiveValues(
        metadata = NULL,
        data = NULL,
        colors_confirmed = FALSE,
        colors = NULL, # Armazena paleta de cores global
        blocos_ativos = character(0),
        blocos_dados = list(),
        slides_arranjos = NULL
    )

    # --- Controle de Sessão (Save/Load) ---
    observeEvent(input$btn_open_save_load, {
        showModal(
            modalDialog(
                title = "Gerenciar Projeto (Sessão)",
                p(
                    "Atenção: A paleta de cores padrão será ativada via código posteriormente. (A fazer)"
                ),
                hr(),
                h5("Salvar Projeto Atual"),
                downloadButton(
                    "btn_save_session",
                    "Baixar Sessão (.rds)",
                    class = "btn-primary w-100 mb-3"
                ),
                hr(),
                h5("Carregar Projeto Salvo"),
                fileInput(
                    "file_load_session",
                    "Selecione o arquivo .rds",
                    accept = ".rds",
                    width = "100%"
                ),
                easyClose = TRUE,
                footer = modalButton("Fechar")
            )
        )
    })

    # Action: Salvar Arquivo
    output$btn_save_session <- downloadHandler(
        filename = function() {
            paste0("Projeto_PPTX_", format(Sys.Date(), "%d-%m-%Y"), ".rds")
        },
        content = function(file) {
            estado_salvar <- list(
                metadata = isolate(rv$metadata),
                data = isolate(rv$data),
                colors_confirmed = isolate(rv$colors_confirmed),
                qtd_slides = isolate(rv$qtd_slides),
                blocos_ativos = isolate(rv$blocos_ativos)
            )

            # Coleta o estado dos blocos (inputs selecionados pelo user em cada módulo)
            estado_salvar$blocos_dados_salvos <- lapply(
                isolate(rv$blocos_ativos),
                function(id) {
                    isolate(rv$blocos_dados[[id]]())
                }
            )
            names(estado_salvar$blocos_dados_salvos) <- isolate(
                rv$blocos_ativos
            )

            # Capturar o layout dos slides da Aba Exportação
            estado_salvar$slides_arranjos <- lapply(
                seq_len(isolate(rv$qtd_slides)),
                function(i) {
                    isolate(input[[paste0("export_tab-slide_", i)]])
                }
            )

            saveRDS(estado_salvar, file = file)
        }
    )

    # Action: Carregar Arquivo
    observeEvent(input$file_load_session, {
        req(input$file_load_session)
        ext <- tools::file_ext(input$file_load_session$name)
        if (ext != "rds") {
            shinyalert::shinyalert(
                "Erro",
                "Arquivo inválido, precisa ser .rds",
                type = "error"
            )
            return()
        }

        tryCatch(
            {
                er <- readRDS(input$file_load_session$datapath)

                # Restaura Variáveis Core
                rv$metadata <- er$metadata
                rv$data <- er$data
                rv$colors_confirmed <- er$colors_confirmed
                rv$qtd_slides <- er$qtd_slides

                # Limpa qualquer módulo que estivesse na tela atualmente
                for (m_id in rv$blocos_ativos) {
                    removeUI(selector = paste0("#", ns("card_block_", m_id))) # Fallback, mas o ideal é remover o parent div
                }
                removeUI(selector = "#ui_blocks_container > *", multiple = TRUE)
                rv$blocos_ativos <- character(0)
                rv$blocos_dados <- list()

                # Restaura slides para injeção posterior na module_export
                if (!is.null(er$slides_arranjos)) {
                    rv$slides_arranjos <- er$slides_arranjos
                }

                # Reconstrói dinamicamente todos os blocos na UI e inicializa seus servers
                if (length(er$blocos_ativos) > 0) {
                    numeros <- as.numeric(gsub("bloco_", "", er$blocos_ativos))
                    blocos_contador(max(numeros))

                    opcoes_disponiveis <- er$metadata$id_questao
                    names(opcoes_disponiveis) <- paste(
                        er$metadata$id_questao,
                        "-",
                        substring(er$metadata$enunciado, 1, 30),
                        "..."
                    )

                    for (m_id in er$blocos_ativos) {
                        rv$blocos_ativos <- c(rv$blocos_ativos, m_id)

                        insertUI(
                            selector = "#ui_blocks_container",
                            where = "beforeEnd",
                            ui = questionBlockUI(
                                m_id,
                                dic_choices = opcoes_disponiveis
                            )
                        )

                        estado_inic <- er$blocos_dados_salvos[[m_id]]
                        rv$blocos_dados[[m_id]] <- questionBlockServer(
                            m_id,
                            rv,
                            remove_callback = function(modulo_id) {
                                rv$blocos_ativos <- setdiff(
                                    rv$blocos_ativos,
                                    modulo_id
                                )
                                rv$blocos_dados[[modulo_id]] <- NULL
                            },
                            estado_inicial = estado_inic
                        )
                    }
                }

                removeModal()
                shinyalert::shinyalert(
                    "Sessão Restaurada!",
                    "Dados, módulos visuais e páginas foram recuperados com sucesso!",
                    type = "success"
                )
            },
            error = function(e) {
                shinyalert::shinyalert(
                    "Erro na leitura",
                    paste(
                        "O formato do arquivo pode estar incorreto. Requer 3 linhas de cabeçalho. Detalhe:",
                        e$message
                    ),
                    type = "error"
                )
            }
        )
    })

    # --- Controle de Estado da UI (Trava de Abas) ---
    observe({
        # Se os dados não foram carregados
        if (is.null(rv$data)) {
            # Desabilita o conteúdo inteiro das outras páginas usando divs encapsuladores
            shinyjs::disable("content_cores")
            shinyjs::disable("content_viz")
            shinyjs::disable("content_export")

            # Adiciona também uma mensagem de aviso visual no lugar das outras abas caso consigam navegar
            # (No futuro podemos usar shinyjs::addClass para dar 'pointer-events-none' na aba da navbar se for bslib)
        } else {
            shinyjs::enable("content_cores")

            # Aba de Visualizações e Exportação só liberam se cores foram confirmadas
            if (rv$colors_confirmed) {
                shinyjs::enable("content_viz")
                shinyjs::enable("content_export")
            } else {
                shinyjs::disable("content_viz")
                shinyjs::disable("content_export")
            }
        }
    })

    # Confirmação de cores
    observeEvent(input$btn_confirm_colors, {
        rv$colors <- list(
            primary = input$color_primary,
            secondary = input$color_secondary,
            tertiary = input$color_tertiary,
            quaternary = input$color_quaternary
        )
        rv$colors_confirmed <- TRUE
        shinyalert::shinyalert(
            "Cores Salvas!",
            "As configurações de cores foram aplicadas ao projeto.",
            type = "success"
        )
    })

    # --- Upload de Dados ---
    observeEvent(input$file_upload, {
        req(input$file_upload)

        # Obter extensão
        ext <- tools::file_ext(input$file_upload$name)

        # Tentar fazer o parse, capturando erros
        tryCatch(
            {
                parsed <- parse_custom_data(
                    input$file_upload$datapath,
                    ext = ext
                )

                # Se sucesso, salva no reativo
                rv$metadata <- parsed$metadata
                rv$data <- parsed$data

                # Notificação de Sucesso
                shinyalert::shinyalert(
                    title = "Sucesso!",
                    text = "Base de dados carregada com sucesso.",
                    type = "success",
                    timer = 3000,
                    showConfirmButton = FALSE
                )
            },
            error = function(e) {
                # Notificação de Erro
                shinyalert::shinyalert(
                    title = "Erro na leitura",
                    text = e$message,
                    type = "error"
                )
                # Reset
                rv$metadata <- NULL
                rv$data <- NULL
            }
        )
    })

    # --- Tabela de Dados (Preview) ---
    output$table_data_preview <- reactable::renderReactable({
        req(rv$data)
        reactable::reactable(
            head(rv$data, 100), # Mostrar até 100 linhas no preview
            searchable = TRUE,
            striped = TRUE,
            highlight = TRUE,
            compact = TRUE
            # ,
            # theme = reactable::reactableTheme(
            #     primaryColor = input$color_primary
            # )
        )
    })

    # --- Gráficos de Exemplo (Cores) ---
    output$plot_example_bar <- renderPlot({
        df <- data.frame(
            Categoria = c("A", "B", "C", "D"),
            Valor = c(40, 30, 20, 10)
        )
        ggplot(df, aes(x = Categoria, y = Valor, fill = Categoria)) +
            geom_col() +
            scale_fill_manual(
                values = c(
                    input$color_primary,
                    input$color_secondary,
                    input$color_tertiary,
                    input$color_quaternary
                )
            ) +
            theme_minimal() +
            labs(title = "Gráfico de Barras Exemplo") +
            theme(legend.position = "none")
    })

    output$plot_example_pie <- renderPlot({
        df <- data.frame(
            Categoria = c("A", "B", "C", "D"),
            Valor = c(40, 30, 20, 10)
        )
        ggplot(df, aes(x = "", y = Valor, fill = Categoria)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            scale_fill_manual(
                values = c(
                    input$color_primary,
                    input$color_secondary,
                    input$color_tertiary,
                    input$color_quaternary
                )
            ) +
            theme_void() +
            labs(title = "Gráfico de Pizza Exemplo")
    })

    # --- Módulo das Questões Visuais (Aba 3) ---
    rv$blocos_ativos <- character(0)
    rv$blocos_dados <- list() # Guarda os reactives retornados pelos módulos
    blocos_contador <- reactiveVal(0)

    # Adicionar novo Bloco
    observeEvent(input$btn_add_block, {
        req(rv$metadata)

        # Pega a lista de opções (IDs que ainda NÃO estão selecionados nos ativos)
        opcoes_disponiveis <- rv$metadata$id_questao
        names(opcoes_disponiveis) <- paste(
            rv$metadata$id_questao,
            "-",
            substring(rv$metadata$enunciado, 1, 30),
            "..."
        )

        # Incrementa contador para ID único
        novo_id_num <- blocos_contador() + 1
        blocos_contador(novo_id_num)
        novo_id_str <- paste0("bloco_", novo_id_num)

        # Registra na reatividade global
        rv$blocos_ativos <- c(rv$blocos_ativos, novo_id_str)

        # Callback de remoção
        remover_callback <- function(modulo_id) {
            rv$blocos_ativos <- setdiff(rv$blocos_ativos, modulo_id)
            rv$blocos_dados[[modulo_id]] <- NULL
        }

        # Insere a UI do Módulo na div de container (prepend para aparecer em cima)
        insertUI(
            selector = "#ui_blocks_container",
            where = "afterBegin",
            ui = questionBlockUI(novo_id_str, dic_choices = opcoes_disponiveis)
        )

        # Inicializa o Server do Módulo e guarda o estado reativo
        rv$blocos_dados[[novo_id_str]] <- questionBlockServer(
            novo_id_str,
            rv,
            remover_callback
        )
    })

    # --- Gestão de Layout / Exportação (Aba 4) ---
    exportBuilderServer("export_tab", rv)
}

# Inicialização
shinyApp(ui, server)
