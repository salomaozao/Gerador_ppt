#' UI do Módulo de Bloco de Questão
#' @param id ID único do módulo
#' @param dic_choices Lista nomeada das questões disponíveis no dicionário
questionBlockUI <- function(id, dic_choices = NULL) {
    ns <- NS(id)

    card(
        id = ns("card_block"),
        class = "mb-3",
        layout_columns(
            col_widths = c(4, 4, 4),

            # --- Coluna 1: Questão ---
            div(
                class = "p-2",
                h5("1. Escolha a Questão"),
                selectInput(
                    ns("sel_questao"),
                    "Código da Questão:",
                    choices = dic_choices,
                    selected = character(0)
                ),
                uiOutput(ns("ui_enunciado"))
            ),

            # --- Coluna 2: Visualização ---
            div(
                class = "p-2 border-start border-end",
                h5("2. Visualização"),
                selectInput(
                    ns("sel_tipo_viz"),
                    "Tipo de Gráfico/Tabela:",
                    choices = c(
                        "Tabela de Frequência" = "tabela_freq",
                        "Gráfico de Barras" = "graf_barras",
                        "Gráfico de Pizza" = "graf_pizza"
                    ),
                    selected = "graf_barras"
                ),
                textInput(ns("txt_legenda"), "Legenda/Título do Gráfico:"),
                hr(),
                uiOutput(ns("plot_preview"))
            ),

            # --- Coluna 3: Detalhes e Ordenação ---
            div(
                class = "p-2",
                h5("3. Detalhes"),
                selectInput(
                    ns("sel_legenda_pos"),
                    "Posição da Legenda (Gráficos):",
                    choices = c(
                        "Nenhuma" = "none",
                        "Direita" = "right",
                        "Esquerda" = "left",
                        "Base" = "bottom",
                        "Topo" = "top"
                    ),
                    selected = "bottom"
                ),
                hr(),
                uiOutput(ns("ui_sortable_options")),
                hr(),
                actionButton(
                    ns("btn_remove"),
                    "Excluir Questão",
                    class = "btn-danger w-100",
                    icon = icon("trash")
                )
            )
        )
    )
}

#' Server do Módulo de Bloco de Questão
#' @param id ID único do módulo
#' @param rv Variável reativa global com metadata e colors
#' @param remove_callback Função callback para dizer ao app principal para remover a UI
questionBlockServer <- function(
    id,
    rv,
    remove_callback,
    estado_inicial = NULL
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Injeta estado inicial se fornecido
        if (!is.null(estado_inicial)) {
            observe({
                updateSelectInput(
                    session,
                    "sel_questao",
                    selected = estado_inicial$questao
                )
                updateSelectInput(
                    session,
                    "sel_tipo_viz",
                    selected = estado_inicial$tipo_viz
                )
                updateTextInput(
                    session,
                    "txt_legenda",
                    value = estado_inicial$titulo
                )
                updateSelectInput(
                    session,
                    "sel_legenda_pos",
                    selected = estado_inicial$pos_legenda
                )
            })
        }

        # Renderizar o enunciado reativamente com base na escolha
        output$ui_enunciado <- renderUI({
            req(input$sel_questao)
            req(rv$metadata)

            enunciado_texto <- rv$metadata %>%
                filter(id_questao == input$sel_questao) %>%
                pull(enunciado)

            if (length(enunciado_texto) == 0) {
                enunciado_texto <- "Enunciado não encontrado."
            }

            # Atualiza a legenda automaticamente para o código da questão
            updateTextInput(session, "txt_legenda", value = input$sel_questao)

            tagList(
                strong("Enunciado:"),
                p(enunciado_texto, class = "text-muted")
            )
        })

        # --- UI Dinâmica de Ordenação de Categorias ---
        output$ui_sortable_options <- renderUI({
            req(rv$data)
            req(input$sel_questao)

            var_name <- input$sel_questao
            df_var <- rv$data[[var_name]]

            # Precisamos do pacote sortable para drag & drop UI
            req(requireNamespace("sortable", quietly = TRUE))

            # Obtém opções únicas não-nulas
            opcoes_unicas <- unique(df_var)
            opcoes_unicas <- opcoes_unicas[!is.na(opcoes_unicas)]

            # Se houver estado_salvo de ordem e for a mesma questão, usa a ordem salva
            if (
                !is.null(estado_inicial) &&
                    !is.null(estado_inicial$ordem_categorias)
            ) {
                if (setequal(estado_inicial$ordem_categorias, opcoes_unicas)) {
                    opcoes_unicas <- estado_inicial$ordem_categorias
                }
                estado_inicial$ordem_categorias <<- NULL # Consume to not repeat
            }

            sortable::rank_list(
                text = "Arraste para reordenar as categorias:",
                labels = opcoes_unicas,
                input_id = ns("order_categorias")
            )
        })

        # Renderização condicional da Visualização (Plot vs Tabela PPTX preview)
        output$plot_preview <- renderUI({
            req(rv$data)
            req(input$sel_questao)
            req(input$sel_tipo_viz)

            # Dados da questão selecionada
            var_name <- input$sel_questao
            df_var <- rv$data[[var_name]]

            # Cálculo de Frequências básico
            df_freq <- rv$data %>%
                filter(!is.na(!!sym(var_name))) %>%
                count(!!sym(var_name), name = "Frequência") %>%
                mutate(
                    Percentual = sprintf(
                        "%.1f%%",
                        (Frequência / sum(Frequência)) * 100
                    )
                ) %>%
                rename(Opção = !!sym(var_name))

            # Aplicar a ordenação do usuário, caso exista
            if (!is.null(input$order_categorias)) {
                df_freq <- df_freq %>%
                    mutate(
                        Opção = factor(Opção, levels = input$order_categorias)
                    ) %>%
                    arrange(Opção)
            }

            if (input$sel_tipo_viz == "tabela_freq") {
                # Renderiza Tabela Estilo PPTX com Flextable e Officer
                req(requireNamespace("flextable", quietly = TRUE))

                ft <- flextable::flextable(df_freq) %>%
                    flextable::theme_vanilla() %>%
                    flextable::set_caption(caption = input$txt_legenda) %>%
                    flextable::autofit()

                return(flextable::htmltools_value(ft))
            } else if (input$sel_tipo_viz == "graf_barras") {
                # Para garantir a ordem correta quando o gráfico inverte os eixos (coord_flip)
                df_freq <- df_freq %>%
                    mutate(Opção = forcats::fct_rev(factor(Opção)))

                p <- ggplot(
                    df_freq,
                    aes(x = Opção, y = Frequência, fill = Opção)
                ) +
                    geom_col() +
                    theme_minimal() +
                    labs(title = input$txt_legenda, x = NULL) +
                    coord_flip()

                # Aplicando cores globais (se disponíveis)
                if (!is.null(rv$colors_confirmed) && rv$colors_confirmed) {
                    if (!is.null(rv$colors) && length(rv$colors) > 0) {
                        cores_usadas <- rep(
                            unname(unlist(rv$colors)),
                            length.out = nrow(df_freq)
                        )
                        p <- p + scale_fill_manual(values = cores_usadas)
                    } else {
                        p <- p + scale_fill_viridis_d(option = "D")
                    }
                }

                if (input$sel_legenda_pos != "none") {
                    p <- p + theme(legend.position = input$sel_legenda_pos)
                } else {
                    p <- p + theme(legend.position = "none")
                }

                output$dyn_plot <- renderPlot(p)
                return(plotOutput(ns("dyn_plot"), height = "250px"))
            } else if (input$sel_tipo_viz == "graf_pizza") {
                p <- ggplot(
                    df_freq,
                    aes(x = "", y = Frequência, fill = Opção)
                ) +
                    geom_bar(stat = "identity", width = 1) +
                    coord_polar(theta = "y") +
                    theme_void() +
                    labs(title = input$txt_legenda)

                if (!is.null(rv$colors_confirmed) && rv$colors_confirmed) {
                    if (!is.null(rv$colors) && length(rv$colors) > 0) {
                        cores_usadas <- rep(
                            unname(unlist(rv$colors)),
                            length.out = nrow(df_freq)
                        )
                        p <- p + scale_fill_manual(values = cores_usadas)
                    } else {
                        p <- p + scale_fill_viridis_d(option = "D")
                    }
                }

                if (input$sel_legenda_pos != "none") {
                    p <- p + theme(legend.position = input$sel_legenda_pos)
                } else {
                    p <- p + theme(legend.position = "none")
                }

                output$dyn_plot <- renderPlot(p)
                return(plotOutput(ns("dyn_plot"), height = "250px"))
            }
        })

        # Ação de Remover esse bloco
        observeEvent(input$btn_remove, {
            # Remove a UI localmente (no client-side via shinyjs ou removeUI)
            removeUI(selector = paste0("#", ns("card_block")))

            # Informa ao app principal para remover da lista global
            if (!is.null(remove_callback)) {
                remove_callback(id)
            }
        })

        # Retorna o estado reativo deste bloco
        return(reactive({
            list(
                id = id,
                questao = input$sel_questao,
                tipo_viz = input$sel_tipo_viz,
                titulo = input$txt_legenda,
                pos_legenda = input$sel_legenda_pos,
                ordem_categorias = input$order_categorias
            )
        }))
    })
}
