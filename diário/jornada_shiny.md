# Jornada de Desenvolvimento: Gerador de Relatórios em R Shiny 🚀

Resumo de tudo que construímos e depuramos até agora neste projeto modular para transformar Dataframes em PowerPoint:

### Fase 1 e 2: Ingestão de Dados Complexa
- Criamos o script customizado `R/data_parser.R` para lidar não somente com dados comuns, mas com regras estritas de negócios (3 linhas iniciais de Metadados: nome da pergunta, categoria Pai, enunciado completo).
- Montamos uma interface blindada com `bslib` e `shinyjs` em `app.R`. O usuário não consegue acessar as abas visuais de configuração enquanto não confirmar a escolha de um arquivo de dados válido.
- Exibição limpa em tabela de preview usando `reactable`.

### Fase 3: Identidade Visual Dinâmica
- Inserimos ferramentas do `colourpicker` para captura de paleta primária/secundária.
- Os gráficos de exemplo em tela reagem live à escolha de cores para provar o conceito.
- A aplicação toda retém um *State* de que "As cores foram confirmadas" para destravar a ABA 3 (Montagem de Visualizações).

### Fase 4 e 5: O Core - Shiny Modules Dinâmicos
- **Desafio Arquitetural**: Criamos a capacidade de gerar `N` perguntas ao rodarmos `insertUI`. Para isso, dividimos as responsabilidades e escrevemos o `questionBlockUI/Server` em `R/module_question_block.R`.
- Implementamos o **Drag and Drop de Fatores** usando bibliotecas HTML5 (como `sortable`), permitindo ao usuário decidir visualmente a ordem das categorias que apareceriam no gráfico (e na legenda).
- **Gestão de Layout em Abas**: Outro desafio de UI, implementar Arrastar e Soltar caixas HTML para organizar o esqueleto dos Slides em um Banco de Questões usando o `bucket_list`. Transformamos todo esse front-end espesso em outro módulo: `R/module_export_builder.R`.

### Fase 6 e Além: Depuração e Polimento Fino
Diversas integrações de ponta precisaram de resolução de bugs não triviais do Ecossistema R/Windows:
- **Locking de Bibliotecas**: Nosso exportador dependia do `{officer}` e `{rvg}`. O pacote `gdtools` (dependência C++) ficou travado em background pelo Windows. Rodamos scripts forçados de *PowerShell via agente* para caçar PIDs travados, purgar as DLLs em `/win-library/4.4/00LOCK` e forçar uma build segura do zero destas libs C++.
- **Problemas de Contexto Reativo (Shiny)**: O Shiny costuma punir o analista que chama varíaveis `rv$...` num `downloadHandler`. Fizemos uma higienização profunda do código embutindo `isolate()` cirurgicamente em *loops for* e avaliações que oprimiam o App na hora da Geração do Arquivo.
- **Gráficos Nativos Microsoft**: Um salto gigantesco no valor final do arquivo gerado para o cliente. Nós estripamos a renderização SVG "Cega" e implementamos a poderosa API do `{mschart}` na geração do PowerPoint, recriando toda a paleta de Cores (`viridis` baseada no drag-and-drop da UI) localmente via R para transbordar a formatação perfeita ao `.pptx`, permitindo que o Excel continue embutido sob as barras para os usuários que receberem o PPT final.
- **Save State Restauration Profundo**: Fizemos com que o `Baixar Sessão .rds` deixasse de ser um mero cache do Dataframe bruto e se tornasse um *Snapshot* inteiro do cérebro da UI. A rotina de Load recupera arranjos complexos, reconstruindo Módulos Visuais (`insertUI`) iterativamente para que a tela pisque e o usuário retome de onde parou 3 dias atrás, nas mesmíssimas perguntas ordenadas num determinado slide do PPT.

---
Seu Gerador modular cruzou o marco de Produto Mínimo Viável para um Aplicativo Escalável, contrapesado contra erros de React e Encoding e gerando relatórios 100% nativos para a plataforma Office em 3 cliques.
