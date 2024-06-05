#__________________________________________________________________________________________________

# Processamento dos microdados do CAGED

# Autor: Alexandre Barros
# Email: alexandrepitstop@gmail.com

# --/09/2023
#__________________________________________________________________________________________________


#> Pacotes -------------------------------------------------------------------
libs <- c("DBI", "RSQLite", "tidyverse", "geobr" , "magrittr" , "magrittr", "readxl", "writexl", "archive")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )


# carregando dependências 
library(tidyverse)
library(geobr)
library(readxl)
library(DBI)
library(RSQLite)
library(RCurl)
library(archive)


# define diretórios

setwd('C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED')
td <- tempdir()

# Define the destination directory path
td <- "C:/Users/YourUsername/TempDirectory"

# Check if the directory exists; if not, create it
if (!dir.exists(td)) {
  dir.create(td)
}

# path ftp do mês de referência
path_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/2023/"


listar_source <- function(path_url) {
  # Scrape files
  file_list <- getURL(path_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_list <- strsplit(file_list, "\n")[[1]]
  file_list <- file_list[file_list != ""]  # Remove empty entries
  
  # Extract only directories and files, excluding parent directory links
  gz_files <- file_list[grepl("\\.7z", file_list, ignore.case = TRUE)]
  
  # Create the full path for each .7z file
  full_paths <- file.path(path_url, gz_files)
  
  return(full_paths)
}

# List the .7z files in the specified FTP directory
arquivos_listados <- listar_source(path_url)

# Print the list of full paths to .7z files
print(arquivos_listados)



for (arquivo_7z in arquivos_listados) {
  
  
  tryCatch({
    download.file(url = arquivo_7z, destfile = td, mode = "wb")
    
    # Lista de arquivos extraídos
    print(paste("Arquivos extraídos de", arquivo_7z, ":"))
    
  }, error = function(e) {
    # Handle errors, e.g., print an error message
    cat("Error downloading or processing:", arquivo_7z, "\n")
    cat("Error message:", conditionMessage(e), "\n")
  })
}

# List the files in the temporary directory
temp_files <- list.files(tf)

# Print the list of temporary files
print(temp_files)


# Construct the command to extract the .7z file using the 7z command-line tool
extract_command <- sprintf('7z x "%s" -o"%s"', seven_zip_file, tf)

# Execute the 7z command to extract the file
system(extract_command)

# Check if the extraction was successful
if (dir.exists(temp_extract_dir)) {
  cat("Files have been successfully extracted to the temporary directory:", temp_extract_dir, "\n")
} else {
  cat("Error: Extraction failed\n")
}




# municípios do estado do Piauí
municipios_piaui = read_municipality(year = 2010, 
                                showProgress = FALSE) %>% 
                    filter(code_state == '22') %>%
                    mutate(name_muni = str_to_upper(name_muni))


# territórios de desenvolvimento
  if(F){
    territorios_piaui = read_xlsx('C:/Users/Alexandre/Downloads/d_municipio_territorio.xlsx',
                                  sheet = 1)
    
    
    plot = ggplot() +
      geom_sf(data=t, aes(fill=`IDHM 2010`, geometry = geom), size=.15)+
      labs(title="IDHM dos Municipíos do Piauí",
           subtitle = 'Ano base 2010',
           caption='Fonte: Elaboração própria', size=8)+
      scale_fill_distiller(palette = 'Greens',limits=c(0.4, 0.8),
                           name="Faixa IDH")+
      theme_minimal() 
    plot  
  }

# carrega libraries




# carrega funções
source( "mtps.R" , prompt = FALSE )

# cria catálogo de dados
catalog <- catalog_mtps( output_dir = output_dir )

# limita apenas para bases de 2016
catalog <- subset( catalog , year == 2016 )