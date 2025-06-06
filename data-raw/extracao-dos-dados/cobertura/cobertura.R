#Cobertura SINASC para municípios
sub_registro_sinasc_muni2015 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2015.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2015,
    .before = uf
  )

sub_registro_sinasc_muni2016 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2016.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2016,
    .before = uf
  )

sub_registro_sinasc_muni2017 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2017.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2017,
    .before = uf
  )

sub_registro_sinasc_muni2018 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2018.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2018,
    .before = uf
  )

sub_registro_sinasc_muni2019 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2019.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2019,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sinasc_muni2020 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2020.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2020,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sinasc_muni2021 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2021.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_municipio,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2021,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sinasc_muni2022 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_muni_2022.ods")
colnames(sub_registro_sinasc_muni2022) <- c("codigo_uf", "nome_uf", "codigo_municipio", "nome_municipio",
                                            "estimado_nascidos", "subregistro_ibge", "sub_notificacao_ms")
sub_registro_sinasc_muni2022 <- filter(sub_registro_sinasc_muni2022, nome_uf != "Nome UF")

sub_registro_sinasc_muni2022 <- sub_registro_sinasc_muni2022 |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_municipio,
    sub_notificacao_ms_sinasc = sub_notificacao_ms
  ) |>
  dplyr::mutate(
    ano = 2022,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sinasc_muni_2015_2021 <- rbind(
  sub_registro_sinasc_muni2015,
  sub_registro_sinasc_muni2016,
  sub_registro_sinasc_muni2017,
  sub_registro_sinasc_muni2018,
  sub_registro_sinasc_muni2019,
  sub_registro_sinasc_muni2020,
  sub_registro_sinasc_muni2021,
  sub_registro_sinasc_muni2022
)

sub_registro_sinasc_muni_2015_2021$sub_notificacao_ms_sinasc <- round(as.numeric(sub_registro_sinasc_muni_2015_2021$sub_notificacao_ms_sinasc), 1)

sub_registro_sinasc_muni_2015_2021 <- sub_registro_sinasc_muni_2015_2021 |>
  dplyr::mutate(
    cobertura = 100 - sub_notificacao_ms_sinasc
  )

#Cobertura SIM para municípios
sub_registro_sim_muni2015 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2015.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2015,
    .before = uf
  )

sub_registro_sim_muni2016 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2016.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2016,
    .before = uf
  )

sub_registro_sim_muni2017 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2017.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2017,
    .before = uf
  )

sub_registro_sim_muni2018 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2018.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2018,
    .before = uf
  )

sub_registro_sim_muni2019 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2019.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2019,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sim_muni2020 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2020.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_uf_2,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2020,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sim_muni2021 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2021.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_municipio,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2021,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sim_muni2022 <- readODS::read_ods("data-raw/ods/sub_registro_sim_muni_2022.ods")
colnames(sub_registro_sim_muni2022) <- c("codigo_uf", "nome_uf", "codigo_municipio", "nome_municipio",
                                            "estimado_obitos", "subregistro_ibge", "sub_notificacao_ms_percent")
sub_registro_sim_muni2022 <- filter(sub_registro_sim_muni2022, nome_uf != "Nome UF")

sub_registro_sim_muni2022 <- sub_registro_sim_muni2022 |>
  janitor::clean_names() |>
  dplyr::select(
    uf = nome_uf,
    codmunres = codigo_municipio,
    municipio = nome_municipio,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2022,
    codmunres = gsub(".{1}$", "", codmunres),
    .before = uf
  )

sub_registro_sim_muni_2015_2021 <- rbind(
  sub_registro_sim_muni2015,
  sub_registro_sim_muni2016,
  sub_registro_sim_muni2017,
  sub_registro_sim_muni2018,
  sub_registro_sim_muni2019,
  sub_registro_sim_muni2020,
  sub_registro_sim_muni2021,
  sub_registro_sim_muni2022
)

sub_registro_sim_muni_2015_2021$sub_notificacao_ms <- round(as.numeric(sub_registro_sim_muni_2015_2021$sub_notificacao_ms), 1)

sub_registro_sim_muni_2015_2021 <- sub_registro_sim_muni_2015_2021 |>
  dplyr::mutate(
    cobertura = 100 - sub_notificacao_ms
  )

#Juntando as bases referentes aos municípios
base_cobertura_muni_2015_2021 <- dplyr::full_join(
  sub_registro_sinasc_muni_2015_2021 |> dplyr::rename(cobertura_sinasc = cobertura),
  sub_registro_sim_muni_2015_2021 |> dplyr::rename(cobertura_sim = cobertura),
  by = c("ano", "uf", "codmunres", "municipio")
  ) |>
  dplyr::select(!c(sub_notificacao_ms))


#Cobertura SINASC para UFs, regiões e Brasil
sub_registro_sinasc_uf_regioes2015 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2015.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2015,
    .before = localidade
  )

sub_registro_sinasc_uf_regioes2016 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2016.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2016,
    .before = localidade
  )

sub_registro_sinasc_uf_regioes2017 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2017.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2017,
    .before = localidade
  )

sub_registro_sinasc_uf_regioes2018 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2018.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2018,
    .before = localidade
  )

sub_registro_sinasc_uf_regioes2019 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2019.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2019,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sinasc_uf_regioes2020 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2020.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2020,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sinasc_uf_regioes2021 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2021.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2021,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sinasc_uf_regioes2022 <- readODS::read_ods("data-raw/ods/sub_registro_sinasc_uf_regioes_2022.ods")
colnames(sub_registro_sinasc_uf_regioes2022) <- c("codigo_uf", "nome_uf",
                                         "estimado_nascidos", "subregistro_ibge", "sub_notificacao_ms_percent")
sub_registro_sinasc_uf_regioes2022 <- filter(sub_registro_sinasc_uf_regioes2022, nome_uf != "Nome UF")

sub_registro_sinasc_uf_regioes2022 <- sub_registro_sinasc_uf_regioes2022 |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sinasc = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2022,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sinasc_uf_regioes_2015_2021 <- rbind(
  sub_registro_sinasc_uf_regioes2015,
  sub_registro_sinasc_uf_regioes2016,
  sub_registro_sinasc_uf_regioes2017,
  sub_registro_sinasc_uf_regioes2018,
  sub_registro_sinasc_uf_regioes2019,
  sub_registro_sinasc_uf_regioes2020,
  sub_registro_sinasc_uf_regioes2021,
  sub_registro_sinasc_uf_regioes2022
)

sub_registro_sinasc_uf_regioes_2015_2021$sub_notificacao_ms <- round(as.numeric(sub_registro_sinasc_uf_regioes_2015_2021$sub_notificacao_ms), 1)

sub_registro_sinasc_uf_regioes_2015_2021 <- sub_registro_sinasc_uf_regioes_2015_2021 |>
  dplyr::mutate(
    cobertura = 100 - sub_notificacao_ms
  )

sub_registro_sinasc_uf_regioes_2015_2021$localidade[which(sub_registro_sinasc_uf_regioes_2015_2021$localidade == "Total Brasil")] <- "Brasil"

#Cobertura SIM para UFs, regiões e Brasil
sub_registro_sim_uf_regioes2015 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2015.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2015,
    .before = localidade
  )

sub_registro_sim_uf_regioes2016 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2016.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2016,
    .before = localidade
  )

sub_registro_sim_uf_regioes2017 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2017.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2017,
    .before = localidade
  )

sub_registro_sim_uf_regioes2018 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2018.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2018,
    .before = localidade
  )

sub_registro_sim_uf_regioes2019 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2019.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2019,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sim_uf_regioes2020 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2020.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2020,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sim_uf_regioes2021 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2021.ods") |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2021,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sim_uf_regioes2022 <- readODS::read_ods("data-raw/ods/sub_registro_sim_uf_regioes_2022.ods")
colnames(sub_registro_sim_uf_regioes2022) <- c("codigo_uf", "nome_uf",
                                                  "estimado_obitos", "subregistro_ibge", "sub_notificacao_ms_percent")
sub_registro_sim_uf_regioes2022 <- filter(sub_registro_sim_uf_regioes2022, nome_uf != "Nome UF")

sub_registro_sim_uf_regioes2022 <- sub_registro_sim_uf_regioes2022 |>
  janitor::clean_names() |>
  dplyr::select(
    localidade = nome_uf,
    sub_notificacao_ms_sim = sub_notificacao_ms_percent
  ) |>
  dplyr::mutate(
    ano = 2022,
    .before = localidade
  ) |>
  dplyr::filter(
    localidade != "Ignorado"
  )

sub_registro_sim_uf_regioes_2015_2021 <- rbind(
  sub_registro_sim_uf_regioes2015,
  sub_registro_sim_uf_regioes2016,
  sub_registro_sim_uf_regioes2017,
  sub_registro_sim_uf_regioes2018,
  sub_registro_sim_uf_regioes2019,
  sub_registro_sim_uf_regioes2020,
  sub_registro_sim_uf_regioes2021,
  sub_registro_sim_uf_regioes2022
)

sub_registro_sim_uf_regioes_2015_2021$sub_notificacao_ms <- round(as.numeric(sub_registro_sim_uf_regioes_2015_2021$sub_notificacao_ms), 1)
sub_registro_sim_uf_regioes_2015_2021$localidade[which(sub_registro_sim_uf_regioes_2015_2021$localidade == "Total Brasil")] <- "Brasil"

sub_registro_sim_uf_regioes_2015_2021 <- sub_registro_sim_uf_regioes_2015_2021 |>
  dplyr::mutate(
    cobertura = 100 - sub_notificacao_ms
  )

#Juntando as bases referentes às UFs e regiões
base_cobertura_uf_regioes_2015_2021 <- dplyr::full_join(
  sub_registro_sinasc_uf_regioes_2015_2021  |> dplyr::rename(cobertura_sinasc = cobertura),
  sub_registro_sim_uf_regioes_2015_2021 |> dplyr::rename(cobertura_sim = cobertura),
  by = c("ano", "localidade")
  ) |>
  dplyr::select(!c(sub_notificacao_ms.x, sub_notificacao_ms.y))


#Exportando as bases
usethis::use_data(base_cobertura_muni_2015_2021, overwrite = TRUE)
usethis::use_data(base_cobertura_uf_regioes_2015_2021, overwrite = TRUE)
usethis::use_data(sub_registro_sinasc_muni_2015_2021, overwrite = TRUE)
usethis::use_data(sub_registro_sim_muni_2015_2021, overwrite = TRUE)
usethis::use_data(sub_registro_sinasc_uf_regioes_2015_2021, overwrite = TRUE)
usethis::use_data(sub_registro_sim_uf_regioes_2015_2021, overwrite = TRUE)




