# Módulo de Exportação e Construção de Slides PPTX

exportBuilderUI <- function(id) {
    ns <- NS(id)
    tagList(
        layout_columns(
            col_widths = c(8, 4),

            # Esquerda: Controle dos Slides e Drag-and-Drop
            card(
                card_header("Organização das Lâminas"),
                actionButton(
                    ns("btn_add_slide"),
                    "Criar Nova Página",
                    class = "btn-primary mb-3",
                    icon = icon("plus")
                ),
                uiOutput(ns("ui_export_order"))
            ),

            # Direita: Preview Visual e Exportação
            card(
                card_header("Pré-visualização & Exportação"),
                selectInput(
                    ns("sel_preview_slide"),
                    "Inspecionar Slide:",
                    choices = c("Slide 1" = 1)
                ),
                div(
                    class = "border p-2 mb-3 bg-light",
                    style = "min-height: 300px;",
                    uiOutput(ns("ui_slide_preview"))
                ),
                hr(),
                p(
                    "Após ordenar os slides, clique abaixo para baixar o PowerPoint."
                ),
                downloadButton(
                    ns("btn_gerar_pptx"),
                    "Gerar Relatório .PPTX",
                    class = "btn-success btn-lg w-100",
                    icon = icon("file-powerpoint")
                )
            )
        )
    )
}

exportBuilderServer <- function(id, rv) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Inicia com 1 slide globalmente se ainda não existir
        if (is.null(isolate(rv$qtd_slides))) {
            rv$qtd_slides <- 1
        }

        observeEvent(input$btn_add_slide, {
            rv$qtd_slides <- rv$qtd_slides + 1
        })

        output$ui_export_order <- renderUI({
            req(length(rv$blocos_ativos) > 0)

            # Coleta os nomes/títulos de cada bloco ativo puxando de seu estado reativo
            nomes_blocos <- sapply(rv$blocos_ativos, function(b_id) {
                estado_atual <- rv$blocos_dados[[b_id]]()
                nome_display <- ifelse(
                    estado_atual$titulo != "",
                    estado_atual$titulo,
                    estado_atual$questao
                )
                paste0(
                    b_id,
                    " | ",
                    nome_display,
                    " (",
                    estado_atual$tipo_viz,
                    ")"
                )
            })

            req(requireNamespace("sortable", quietly = TRUE))

            # Se houver estado salvo dos slides, alocamos os labels
            slides_salvos <- isolate(rv$slides_arranjos)

            # Remove os blocos já alocados em slides do "Banco de Questões"
            blocos_no_banco <- nomes_blocos
            if (!is.null(slides_salvos)) {
                alocados <- unlist(slides_salvos)
                blocos_no_banco <- setdiff(nomes_blocos, alocados)
            }

            # Cria as rank_list dinamicamente para cada slide criado
            lista_slides <- lapply(1:rv$qtd_slides, function(i) {
                lbs <- if (
                    !is.null(slides_salvos) && length(slides_salvos) >= i
                ) {
                    slides_salvos[[i]]
                } else {
                    NULL
                }
                sortable::add_rank_list(
                    text = paste("Slide", i),
                    labels = lbs,
                    input_id = ns(paste0("slide_", i))
                )
            })

            # O Banco de Questões (itens não alocados)
            banco_questoes <- sortable::add_rank_list(
                text = "Banco de Questões Disponíveis:",
                labels = blocos_no_banco,
                input_id = ns("banco_questoes")
            )

            # Limpa o array de estados salvos após o primeiro uso
            rv$slides_arranjos <- NULL

            # Une o banco + as áreas dos slides na chamada do bucket_list
            args_bucket <- c(list(banco_questoes), lista_slides)
            args_bucket$header <- "Arraste os blocos para organizar as lâminas (MÁX. 2 por slide)"
            args_bucket$group_name <- ns("bucket_slides")
            args_bucket$orientation <- "horizontal"

            do.call(sortable::bucket_list, args_bucket)
        })

        # Atualiza as opções do selectInput de Preview
        observe({
            req(rv$qtd_slides)
            opcoes <- seq_len(rv$qtd_slides)
            names(opcoes) <- paste("Slide", opcoes)
            updateSelectInput(session, "sel_preview_slide", choices = opcoes)
        })

        # Lógica do Preview Visual do Slide
        output$ui_slide_preview <- renderUI({
            req(input$sel_preview_slide)

            slide_id <- paste0("slide_", input$sel_preview_slide)
            blocos_no_slide <- input[[slide_id]]

            if (is.null(blocos_no_slide) || length(blocos_no_slide) == 0) {
                return(h5(
                    "Slide vazio. Arraste blocos para cá!",
                    class = "text-center text-muted mt-5"
                ))
            }

            if (length(blocos_no_slide) > 2) {
                blocos_no_slide <- blocos_no_slide[1:2]
                shinyalert::shinyalert(
                    "Aviso",
                    "A prévia e PPTX suportam max 2 elementos por slide.",
                    type = "warning",
                    className = "custom-alert"
                )
            }

            # Renderizador Interno (Preview)
            render_preview_bloco <- function(bloco_str) {
                str_splited <- strsplit(bloco_str, " \\| ")[[1]]
                bloco_id <- str_splited[1]

                estado_atual <- rv$blocos_dados[[bloco_id]]()
                var_name <- estado_atual$questao

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

                if (!is.null(estado_atual$ordem_categorias)) {
                    df_freq <- df_freq %>%
                        mutate(
                            Opção = factor(
                                Opção,
                                levels = estado_atual$ordem_categorias
                            )
                        ) %>%
                        arrange(Opção)
                }

                if (estado_atual$tipo_viz == "tabela_freq") {
                    req(requireNamespace("flextable", quietly = TRUE))
                    ft <- flextable::flextable(df_freq) %>%
                        flextable::theme_vanilla() %>%
                        flextable::set_caption(
                            caption = estado_atual$titulo
                        ) %>%
                        flextable::autofit()
                    return(flextable::htmltools_value(ft))
                } else {
                    if (estado_atual$tipo_viz == "graf_barras") {
                        df_freq <- df_freq %>%
                            mutate(Opção = forcats::fct_rev(factor(Opção)))
                        p <- ggplot(
                            df_freq,
                            aes(x = Opção, y = Frequência, fill = Opção)
                        ) +
                            geom_col() +
                            theme_minimal() +
                            labs(title = estado_atual$titulo, x = NULL) +
                            coord_flip()
                    } else {
                        p <- ggplot(
                            df_freq,
                            aes(x = "", y = Frequência, fill = Opção)
                        ) +
                            geom_bar(stat = "identity", width = 1) +
                            coord_polar(theta = "y") +
                            theme_void() +
                            labs(title = estado_atual$titulo)
                    }

                    if (!is.null(rv$colors_confirmed) && rv$colors_confirmed) {
                        p <- p + scale_fill_viridis_d(option = "D")
                    }
                    if (estado_atual$pos_legenda != "none") {
                        p <- p +
                            theme(legend.position = estado_atual$pos_legenda)
                    } else {
                        p <- p + theme(legend.position = "none")
                    }

                    tf <- tempfile(fileext = ".png")
                    ggsave(tf, plot = p, width = 5, height = 4, dpi = 90)
                    return(tags$img(
                        src = base64enc::dataURI(file = tf, mime = "image/png"),
                        width = "100%",
                        style = "max-height: 350px; object-fit: contain;"
                    ))
                }
            }

            elementos_renderizados <- lapply(
                blocos_no_slide,
                render_preview_bloco
            )

            if (length(elementos_renderizados) == 1) {
                return(div(class = "p-2", elementos_renderizados[[1]]))
            } else {
                return(layout_columns(
                    col_widths = c(6, 6),
                    div(class = "p-1", elementos_renderizados[[1]]),
                    div(class = "p-1 border-start", elementos_renderizados[[2]])
                ))
            }
        })

        output$btn_gerar_pptx <- downloadHandler(
            filename = function() {
                paste0(
                    "Relatorio_Gerado_",
                    format(Sys.Date(), "%d-%m-%Y"),
                    ".pptx"
                )
            },
            contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
            content = function(file) {
                tryCatch(
                    {
                        if (!requireNamespace("officer", quietly = TRUE)) {
                            stop("Pacote 'officer' necessário.")
                        }
                        if (!requireNamespace("mschart", quietly = TRUE)) {
                            stop("Pacote 'mschart' necessário.")
                        }
                        if (!requireNamespace("flextable", quietly = TRUE)) {
                            stop("Pacote 'flextable' necessário.")
                        }

                        showNotification(
                            "Compilando PPTX...",
                            type = "message",
                            duration = NULL,
                            id = "notif_pptx"
                        )

                        meu_pptx <- officer::read_pptx()

                        for (i in seq_len(isolate(rv$qtd_slides))) {
                            slide_id <- paste0("slide_", i)
                            blocos_no_slide <- isolate(input[[slide_id]])

                            if (
                                is.null(blocos_no_slide) ||
                                    length(blocos_no_slide) == 0
                            ) {
                                next
                            }

                            if (length(blocos_no_slide) == 1) {
                                meu_pptx <- officer::add_slide(
                                    meu_pptx,
                                    layout = "Title and Content",
                                    master = "Office Theme"
                                )
                            } else {
                                meu_pptx <- officer::add_slide(
                                    meu_pptx,
                                    layout = "Two Content",
                                    master = "Office Theme"
                                )
                                blocos_no_slide <- blocos_no_slide[1:2]
                            }

                            for (b in seq_along(blocos_no_slide)) {
                                bloco_str <- blocos_no_slide[b]
                                bloco_id <- strsplit(bloco_str, " \\| ")[[1]][1]
                                estado <- isolate(rv$blocos_dados[[bloco_id]]())

                                ph_target <- ifelse(
                                    b == 1,
                                    "Content Placeholder 2",
                                    "Content Placeholder 3"
                                )

                                var_name <- estado$questao
                                df_freq <- isolate(rv$data) %>%
                                    filter(!is.na(!!sym(var_name))) %>%
                                    count(
                                        !!sym(var_name),
                                        name = "Frequência"
                                    ) %>%
                                    mutate(
                                        Percentual = sprintf(
                                            "%.1f%%",
                                            (Frequência / sum(Frequência)) * 100
                                        )
                                    ) %>%
                                    rename(Opção = !!sym(var_name))

                                if (!is.null(estado$ordem_categorias)) {
                                    df_freq <- df_freq %>%
                                        mutate(
                                            Opção = factor(
                                                Opção,
                                                levels = estado$ordem_categorias
                                            )
                                        ) %>%
                                        arrange(Opção)
                                }

                                if (b == 1 && length(blocos_no_slide) == 1) {
                                    meu_pptx <- officer::ph_with(
                                        meu_pptx,
                                        value = estado$titulo,
                                        location = officer::ph_location_type(
                                            type = "title"
                                        )
                                    )
                                } else if (
                                    b == 1 && length(blocos_no_slide) == 2
                                ) {
                                    meu_pptx <- officer::ph_with(
                                        meu_pptx,
                                        value = "Painel Comparativo",
                                        location = officer::ph_location_type(
                                            type = "title"
                                        )
                                    )
                                }

                                if (estado$tipo_viz == "tabela_freq") {
                                    ft <- flextable::flextable(df_freq) %>%
                                        flextable::theme_vanilla() %>%
                                        flextable::set_caption(
                                            caption = estado$titulo
                                        ) %>%
                                        flextable::autofit()
                                    meu_pptx <- officer::ph_with(
                                        meu_pptx,
                                        value = ft,
                                        location = officer::ph_location_label(
                                            ph_label = ph_target
                                        )
                                    )
                                } else {
                                    # Configuração de Cores (Paletas Customizadas do Tema)
                                    usar_cores <- !is.null(isolate(
                                        rv$colors_confirmed
                                    )) &&
                                        isolate(rv$colors_confirmed)

                                    num_cats <- nrow(df_freq)

                                    if (
                                        !is.null(isolate(rv$colors)) &&
                                            length(isolate(rv$colors)) > 0
                                    ) {
                                        # Usa paleta customizada (repetindo as 4 cores caso existam mais categorias)
                                        cores_base <- unname(unlist(isolate(
                                            rv$colors
                                        )))
                                        cores_hex <- rep(
                                            cores_base,
                                            length.out = num_cats
                                        )
                                    } else {
                                        # Fallback viridis caso rv$colors não esteja setado
                                        cores_hex <- scales::viridis_pal(
                                            option = "D"
                                        )(num_cats)
                                    }

                                    cores_nomeadas <- setNames(
                                        cores_hex,
                                        as.character(df_freq$Opção)
                                    )

                                    if (estado$tipo_viz == "graf_barras") {
                                        # Precisamos reverter a ordem porque o PowerPoint horizontal
                                        # plota de baixo para cima, invertendo a visão natural da UI
                                        df_freq_chart <- df_freq %>%
                                            mutate(
                                                Opção = forcats::fct_rev(factor(
                                                    Opção
                                                ))
                                            )

                                        # mschart (Natívo Office)
                                        my_chart <- mschart::ms_barchart(
                                            data = df_freq_chart,
                                            x = "Opção",
                                            y = "Frequência",
                                            group = "Opção"
                                        ) %>%
                                            mschart::chart_settings(
                                                dir = "horizontal",
                                                gap_width = 50 # Barras mais grossas
                                            )

                                        l_pos <- ifelse(
                                            estado$pos_legenda == "none",
                                            "n",
                                            substr(estado$pos_legenda, 1, 1)
                                        )

                                        my_chart <- my_chart %>%
                                            mschart::chart_theme(
                                                legend_position = l_pos
                                            ) %>%
                                            mschart::chart_data_labels(
                                                show_val = TRUE,
                                                position = "ctr" # Rótulos centralizados nas barras
                                            ) %>%
                                            mschart::chart_labels(
                                                title = estado$titulo
                                            )

                                        if (usar_cores) {
                                            my_chart <- my_chart %>%
                                                mschart::chart_data_fill(
                                                    values = cores_nomeadas
                                                )
                                        }

                                        meu_pptx <- officer::ph_with(
                                            meu_pptx,
                                            value = my_chart,
                                            location = officer::ph_location_label(
                                                ph_label = ph_target
                                            )
                                        )
                                    } else {
                                        # rvg (SVG para o PowerPoint, para Pizza pois mschart n suporta)
                                        p <- ggplot2::ggplot(
                                            df_freq,
                                            ggplot2::aes(
                                                x = "",
                                                y = Frequência,
                                                fill = Opção
                                            )
                                        ) +
                                            ggplot2::geom_bar(
                                                stat = "identity",
                                                width = 1
                                            ) +
                                            ggplot2::coord_polar(theta = "y") +
                                            ggplot2::theme_void() +
                                            ggplot2::labs(title = estado$titulo)

                                        if (usar_cores) {
                                            p <- p +
                                                ggplot2::scale_fill_viridis_d(
                                                    option = "D"
                                                )
                                        }
                                        if (estado$pos_legenda != "none") {
                                            p <- p +
                                                ggplot2::theme(
                                                    legend.position = estado$pos_legenda
                                                )
                                        } else {
                                            p <- p +
                                                ggplot2::theme(
                                                    legend.position = "none"
                                                )
                                        }

                                        pptx_gg <- rvg::dml(ggobj = p)
                                        meu_pptx <- officer::ph_with(
                                            meu_pptx,
                                            value = pptx_gg,
                                            location = officer::ph_location_label(
                                                ph_label = ph_target
                                            )
                                        )
                                    }
                                }
                            }
                        }

                        if (length(meu_pptx) == 0) {
                            removeNotification("notif_pptx")
                            shinyalert::shinyalert(
                                "Erro Exportação",
                                "Adicione pelo menos 1 bloco a 1 slide para exportar.",
                                type = "error",
                                className = "custom-alert"
                            )
                            return(NULL)
                        }

                        print(meu_pptx, target = file)
                        removeNotification("notif_pptx")
                        shinyalert::shinyalert(
                            "Sucesso!",
                            "PPTX baixado com sucesso.",
                            type = "success",
                            className = "custom-alert"
                        )
                    },
                    error = function(e) {
                        removeNotification("notif_pptx")
                        shinyalert::shinyalert(
                            "Erro Exportação",
                            paste("Falha na geração:", e$message),
                            type = "error",
                            className = "custom-alert"
                        )
                        return(NULL)
                    }
                )
            }
        )
    })
}
