# Plano Geral de Desenvolvimento e Expansão: Gerador de Relatórios Dinâmicos (PPTX)

Este documento estabelece a visão estratégica, a arquitetura consolidada e o roteiro extensivo (roadmap) para as próximas fases evolutivas do aplicativo **Gerador de Relatórios em R Shiny e PPTX**. Seu propósito é servir de bússola para futuras expansões e integrações, garantindo escalabilidade, manutenção clara e aprimoramento da experiência do usuário (UX).

---

## 1. Visão Geral do Produto e Arquitetura Consolidada

O aplicativo atualmente funciona como uma esteira completa de processamento de dados e design de painéis: ele coleta bases de dados brutas (com metadados embutidos), permite o tratamento reativo e a escolha de design visual, e finaliza exportando **arquivos vetoriais em formato Office (PowerPoint)**.

A arquitetura foi pensada de forma modular e baseada no paradigma de reatividade do Shiny:

*   **Camada de Ingestão (Parser):** `R/data_parser.R`
    *   **Função:** Trata o caso de negócio bem específico de linhas de metadados antes dos dados reais (Dicionário de dados "on-the-fly"). 
    *   **Vantagem Organizacional:** Isola a bagunça do wrangling (limpeza) do servidor do painel e converte rapidamente colunas complexas em *factors* tratáveis.
*   **Camada de Apresentação e Fluxo (UI / `app.R`):**
    *   **Função:** Escrita estruturada com o ecossistema `bslib`. Em vez de navegação livre, criamos uma rampa ("Wizard") com restrições lógicas do pacote `shinyjs`. Isso impede que processos downstream (como desenhar gráficos) quebrem por ausência de processos upstream (dados ou cores apagadas).
*   **Camada de Geração Dinâmica (Shiny Modules):** `R/module_question_block.R`
    *   **Função:** A espinha dorsal. Em vez de duplicidade de código no *server*, o aplicativo renderiza `n` instâncias independentes de um módulo, todas devolvendo variáveis reativas. Usa o poder do pacote `sortable` (Rank Lists) para gerir *factors* visualmente na UI.
*   **Camada de Exportação (Officer / Rvg):**
    *   **Função:** Interpretar a *sandbox* montada na página (Slide Builder), organizar o *placeholder mapping* (títular e alinhar objetos na lâmina), e usar DrawingML (`rvg`) ou `flextable` para preservar edição posterior. O fluxo conta com uma barreira defensiva (`tryCatch`) que impede um congelamento mudo da máquina no download.

---

## 2. Roteiro de Expansão (O "Roadmap" Extensivo)

Abaixo listamos as trilhas de evolução sugeridas, divididas por complexidade ou necessidades de negócio.

### Trilha A: Funcionalidades Core de Visualização
Embora a base tenha tabelas de frequência, pizza e barras verticais/horizontais estabelecidas, dados estatísticos em pesquisas complexas exigem flexibilidade adicional.
1.  **Suporte a Variáveis de Múltipla Escolha (Dummies):** Muitas pesquisas vêm quebradas (ex: "Qual fruta você gosta? Maçã (0 ou 1), Banana (0 ou 1)"). Desenvolver uma sub-rotina no Módulo que reconhece strings regex ou famílias de colunas cruzando e colapsando tabelas antes do `ggplot`.
2.  **Novos Tipos de Visualização:**
    *   **Barras Empilhadas 100% (Likert):** Fundamentais para NPS e questionários de satisfação.
    *   **Gráficos de Linha / Séries Temporais:** Para pesquisas de rastreio de meses ou de turmas sequenciais.
    *   **Nuvem de Palavras (Wordcloud2 ou ggwordcloud):** Integração para tratar respostas puramente textuais de campos de "Opinião aberta". Precisará de um removedor de *stopwords* inserido na limpeza.
3.  **Filtros de Segmentação Globais (O "Painel Filtro"):**
    *   Permitir que o usuário escolha globalmente recortes (ex: "Criar PPT apenas com dados da Região Sul", ou "Apenas Mulheres"). Isso injetaria um `filter()` extra no `rv$data_filtered` antes do fluxo fluir para as lâminas.

### Trilha B: Templates e Branding Coorporativo (A "Cereja" Estética)
A exportação cria lâminas "Office Theme" padrão. O salto de excelência envolve carregar guias de marca corporativa (Brandbook).
1.  **Templates Base Customizáveis (`.potx` ou `.pptx` vazios):**
    *   Permitir um upload extra na aba 2 de um arquivo base do PowerPoint.
    *   O motor `officer::read_pptx("meu_layout_empresa.pptx")` herdará as masters (Slide Mestres).
    *   Crucial mapear os *placeholders* no R para bateram exato com caixas pré-fabricadas. 
2.  **Sistema de Paletas Expandido (`ggthemr` ou `bslib` custom sass):**
    *   Aplicar essas seleções de paleta em todos os gradientes do `ggplot`, ao invés de usar o padrão de contraste base ou o Viridis fixo.

### Trilha C: Save & Load (O Sistema de Projetos / Sessões)
Cenário comum: um analista passa 40 minutos diagramando meticulosamente os relatórios do PPTX e organizando 30 lâminas... se a página der F5 ou o servidor cair, todo o trabalho é perdido.
1.  **Exportação do Estado Reativo do Módulo `.RDS`:**
    *   Mapear todo o vetor de `input` de cada Módulo gerado através do botão **Salvar Sessão**. O Shiny deve transformar tudo num arquivo binário `.rds` baixável.
2.  **Loader de Projeto:**
    *   Ao voltar ao app, o analista poderia submeter os Dados Originais + Seu Arquivo `.rds`. O R faria um laço invertido que invocaria `insertUI()` injetando cada módulo na sua configuração salva (atualizando as cores e ordem nas listas).

### Trilha D: Performance, Logging e Profiling 
Com a massividade de dados pesados e centenas de gráficos gerados on-the-fly via encodificação base64 no renderUI, podemos engarrafar o aplicativo (ou sobrecarregar a RAM do contêiner onde hospedar).
1.  **Integração do pacote `logger`:** Trocar os `print` e mensagens estáticas no terminal R para armazenagem logada (INFO, WARNING, FATAL ERROR), vital quando for enviar à produção corporativa.
2.  **Substituição da lógica `Base64` direta por `renderPlot` modulares (lazy loading):** Modificar a aba final para um esquema de grid assíncrono. Isso fará o preview ser gerado sob demanda, em vez de exigir geração de todo o script para visibilidade.
3.  **Deploy em Arquiteturas Escaláveis:** Se isto subir no AWS CaaS (via Docker / ShinyProxy) ou num Posit Connect, devemos refazer a validação de permissões de disco temporal de bibliotecas sistêmicas e DLLs para mitigar o clássico caso que sofremos do bloqueio de arquivos das bibliotecas visuais como `systemfonts` – usando talvez scripts *bash* rodando R em contextos estéreis.

---

## 3. Estratégia de Modularidade a Curto Prazo
Embora tenhamos componentizado as Questões (Aba 3), a Aba 4 `app.R` continua inflada e abrigando múltiplas responsabilidades (DragDrop logic, Preview, Motor PPTX).
**Ação Arquitetônica Imediata Recomendada:**
*   Criar `R/module_export_builder.R`
*   Transferir toda a interface do `bucket_list` e toda a `tryCatch` de download.
*   Manter a *main* (`app.R`) servindo puramente de maestro passando o objeto `rv`. 

---
Esse documento deve ser revisitado ao iniciar as Sprints de Desenvolvimento Fases 6+. O objetivo central alcançado hoje serve como base madura e sólida para comportar toda a complexidade exigida aqui dentro do ecossistema de reatividade Shiny.
