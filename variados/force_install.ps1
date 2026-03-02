$ErrorActionPreference = "SilentlyContinue"
Write-Host "Forcando o encerramento de processos R..."
Stop-Process -Name "rsession" -Force
Stop-Process -Name "Rscript" -Force
Stop-Process -Name "Rterm" -Force
Start-Sleep -Seconds 2

Write-Host "Removendo pastas corrompidas 00LOCK..."
Remove-Item -Path "C:\Users\GabrielNascimento\AppData\Local\R\win-library\4.4\00LOCK*" -Recurse -Force
Remove-Item -Path "C:\Users\GabrielNascimento\AppData\Local\R\win-library\4.4\systemfonts" -Recurse -Force
Remove-Item -Path "C:\Users\GabrielNascimento\AppData\Local\R\win-library\4.4\gdtools" -Recurse -Force
Remove-Item -Path "C:\Users\GabrielNascimento\AppData\Local\R\win-library\4.4\rvg" -Recurse -Force
Start-Sleep -Seconds 1

Write-Host "Iniciando instalacao limpa..."
& Rscript -e "install.packages(c('systemfonts', 'gdtools', 'rvg'), repos='https://cloud.r-project.org')"
Write-Host "Concluido!"
