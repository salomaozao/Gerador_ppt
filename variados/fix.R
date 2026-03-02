lines <- readLines('app.R', warn = FALSE)
# Find the line that marks the beginning of the Export logic
idx <- grep('Gest.o de Layout / Exporta..o', lines)
if (length(idx) > 0) {
    # We want to keep up to idx[1], then add our module call and closing bracket
    new_lines <- c(
        lines[1:idx[1]],
        '    exportBuilderServer("export_tab", rv)',
        '}'
    )
    # Write back keeping the encoding straight to avoid destroying accents
    writeLines(new_lines, con = file('app.R', encoding = "UTF-8"))
    print("Sucesso!")
} else {
    print("Tag not found!")
}
