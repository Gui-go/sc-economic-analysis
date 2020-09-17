# install.packages("sidrar")
library(sidrar)

info_sidra(x = "4096")

seqsem <- seq.Date(from = as.Date("2012-02-15"), to = today(), by = "quarter") %>% 
  quarter(., with_year = TRUE) %>% 
  sub(pattern = "\\.", replacement = "0", x = .)

t4096 <- get_sidra(
  x = "4096", 
  period = seqsem, 
  geo = c("State", "Brazil"), 
  geo.filter = list("Brazil" = 1, "State" = c(42))
)

t4096f <- t4096 %>% 
  janitor::clean_names() %>%
  dplyr::select("brasil_e_unidade_da_federacao", "brasil_e_unidade_da_federacao_codigo", "trimestre_codigo", "trimestre", "variavel", "posicao_na_ocupacao_no_trabalho_principal", "valor", "unidade_de_medida") %>% 
  dplyr::filter(variavel == "Distribuição percentual das pessoas de 14 anos ou mais de idade, ocupadas na semana de referência") %>% 
  dplyr::filter(brasil_e_unidade_da_federacao == "Santa Catarina")

seqtri <- seq.Date(from = as.Date("2012-02-15"), to = today(), by = "quarter")
t4096f_Total <- t4096f[which(t4096f$posicao_na_ocupacao_no_trabalho_principal == "Total"), "valor"]
t4096f_Empregado <- t4096f[which(t4096f$posicao_na_ocupacao_no_trabalho_principal == "Empregado"), "valor"]
t4096f_Empregador <- t4096f[which(t4096f$posicao_na_ocupacao_no_trabalho_principal == "Empregador"), "valor"]
t4096f_Conta_propria <- t4096f[which(t4096f$posicao_na_ocupacao_no_trabalho_principal == "Conta própria"), "valor"]
t4096f_Trabalhador_familiar_auxiliar <- t4096f[which(t4096f$posicao_na_ocupacao_no_trabalho_principal == "Trabalhador familiar auxiliar"), "valor"]

t4096fdf <- data.frame(
  t4096f_Total = t4096f_Total,
  t4096f_Empregado = t4096f_Empregado,
  t4096f_Empregador = t4096f_Empregador,
  t4096f_Conta_propria = t4096f_Conta_propria,
  t4096f_Trabalhador_familiar_auxiliar = t4096f_Trabalhador_familiar_auxiliar,
  trimestres = seqtri[1:length(t4096f_Total)]
)

secm1 <- 2.9
t4096plot <- ggplot(t4096fdf, aes(x=trimestres))+
  geom_line(aes(y = t4096f_Empregado, color = "Empregado com carteira assinada"))+
  geom_line(aes(y = t4096f_Conta_propria*secm1, color = "Trabalhador autônomo"))+
  scale_y_continuous(name = "Empregados", 
                     sec.axis = sec_axis(~./secm1, name = "Empregados por conta própria", 
                                         labels = function(b) { paste0(round(b / 1, 0), ".")}))+
  scale_color_manual(values=c("#9999CC", "#CC6666"))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(
    title = "Variação na ocupação principal dos residentes de Santa Catarina",
    subtitle = "Distribuição percentual das pessoas de  anos ou mais de idade, ocupadas na semana de referência",
    caption = "Guilherme Viegas",
    colour = ""
  )
t4096plot



# Second ------------------------------------------------------------------
info_sidra(x = "5434")

seqsem <- seq.Date(from = as.Date("2012-02-15"), to = today(), by = "quarter") %>% 
  quarter(., with_year = TRUE) %>% 
  sub(pattern = "\\.", replacement = "0", x = .)

t5434 <- get_sidra(
  x = "5434", 
  period = seqsem, 
  geo = c("State", "Brazil"), 
  geo.filter = list("Brazil" = 1, "State" = c(42))
)

t5434f <- t5434 %>% 
  janitor::clean_names() %>%
  dplyr::select("brasil_e_unidade_da_federacao", "brasil_e_unidade_da_federacao_codigo", "trimestre_codigo", "trimestre", "variavel", "grupamento_de_atividades_no_trabalho_principal_pnadc", "valor", "unidade_de_medida") %>% 
  dplyr::filter(variavel == "Distribuição percentual das pessoas de 14 anos ou mais de idade, ocupadas na semana de referência") %>% 
  dplyr::filter(brasil_e_unidade_da_federacao == "Santa Catarina")

unique(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc)

# seqtri <- seq.Date(from = as.Date("2012-02-15"), to = lubridate::today(), by = "quarter")
t5434f_Total <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Total"), "valor"]
t5434f_agro <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Agricultura, pecuária, produção florestal, pesca e aquicultura"), "valor"]
t5434f_ind_geral <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Indústria geral"), "valor"]
t5434f_ind_transf <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Indústria de transformação"), "valor"]
t5434f_const <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Construção"), "valor"]
t5434f_comer <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Comércio, reparação de veículos automotores e motocicletas"), "valor"]
t5434f_transp <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Transporte, armazenagem e correio"), "valor"]
t5434f_aloj_alim <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Alojamento e alimentação"), "valor"]
t5434f_info <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"), "valor"]
t5434f_adm_pub <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Administração pública, defesa, seguridade social, educação, saúde humana e serviços sociais"), "valor"]
t5434f_outroserv <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Outro serviço"), "valor"]
t5434f_serv_domest <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Serviço doméstico"), "valor"]
t5434f_maldef <- t5434f[which(t5434f$grupamento_de_atividades_no_trabalho_principal_pnadc == "Atividades mal definidas"), "valor"]


t5434df <- data.frame(
  t5434f_Total = t5434f_Total,
  t5434f_agro = t5434f_agro,
  t5434f_ind_geral = t5434f_ind_geral,
  t5434f_ind_transf = t5434f_ind_transf,
  t5434f_const = t5434f_const,
  t5434f_comer = t5434f_comer,
  t5434f_transp = t5434f_transp,
  t5434f_aloj_alim = t5434f_aloj_alim,
  t5434f_info = t5434f_info,
  t5434f_adm_pub = t5434f_adm_pub,
  t5434f_outroserv = t5434f_outroserv,
  t5434f_serv_domest = t5434f_serv_domest,
  t5434f_maldef = t5434f_maldef,
  trimestres = seqtri[1:length(t5434f_Total)]
)

secm2 <- 3
ggplot(t5434df, aes(x=trimestres))+
  # geom_line(aes(y = t5434f_agro, color = "t5434f_agro"))+
  geom_line(aes(y = t5434f_ind_geral, color = "t5434f_ind_geral"))+ # ***
  # geom_line(aes(y = t5434f_ind_transf, color = "t5434f_ind_transf"))+ # ***
  # geom_line(aes(y = t5434f_const, color = "t5434f_const"))+ # **
  # geom_line(aes(y = t5434f_comer, color = "t5434f_comer"))+ # *
  # geom_line(aes(y = t5434f_transp, color = "t5434f_transp"))+
  geom_line(aes(y = t5434f_info*secm2, color = "t5434f_info"))+ # ***
  # geom_line(aes(y = t5434f_adm_pub, color = "t5434f_adm_pub"))+
  # geom_line(aes(y = t5434f_outroserv, color = "t5434f_outroserv"))+ # ***
  # geom_line(aes(y = t5434f_serv_domest, color = "t5434f_serv_domest"))+ # **
  # geom_line(aes(y = t5434f_maldef, color = "t5434f_maldef"))+
  scale_y_continuous(name = "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas", 
                     sec.axis = sec_axis(~./secm2, name = "Empregados por conta própria", 
                                         labels = function(b) { paste0(round(b / 1, 0), ".")}))+
  # scale_color_manual(values=c("#9999CC", "#CC6666"))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(
    title = "Variação na ocupação principal dos residentes de Santa Catarina",
    subtitle = "Distribuição percentual das pessoas de 14 anos ou mais de idade, ocupadas na semana de referência",
    caption = "Guilherme Viegas"#,
    # colour = ""
  )

# T5433 -------------------------------------------------------------------
# info_sidra(x = "5433")

seqsem <- seq.Date(from = as.Date("2012-02-15"), to = lubridate::today(), by = "quarter") %>% 
  quarter(., with_year = TRUE) %>% 
  sub(pattern = "\\.", replacement = "0", x = .)

t5433 <- get_sidra(
  x = "5433", 
  period = seqsem, 
  geo = c("State", "Brazil"), 
  geo.filter = list("Brazil" = 1, "State" = c(42)),
)

# unique(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal)

t5433f <- t5433 %>% 
  janitor::clean_names() %>%
  dplyr::select("brasil_e_unidade_da_federacao", "brasil_e_unidade_da_federacao_codigo", "trimestre_codigo", "trimestre", "variavel", "posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal", "valor", "unidade_de_medida") %>% 
  dplyr::filter(variavel == "Rendimento médio nominal do trabalho principal, efetivamente recebido no mês de referência, pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho") %>% 
  # dplyr::filter(posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Empregador") %>% 
  dplyr::filter(brasil_e_unidade_da_federacao == "Santa Catarina")

seqtri <- seq.Date(from = as.Date("2012-02-15"), to = lubridate::today(), by = "quarter")
# t5433f_Total <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Total"), "valor"]
# t5433f_priv <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Empregado no setor privado, exclusive trabalhador doméstico"), "valor"]
# t5433f_priv_cc <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Empregado no setor privado, exclusive trabalhador doméstico - com carteira de trabalho assinada"), "valor"]
# t5433f_priv_sc <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Empregado no setor privado, exclusive trabalhador doméstico - sem carteira de trabalho assinada"), "valor"]
t5433f_empregador <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Empregador"), "valor"]
t5433f_cp <- t5433f[which(t5433f$posicao_na_ocupacao_e_categoria_do_emprego_no_trabalho_principal == "Conta própria"), "valor"]


t5434df <- data.frame(
  # t5433f_Total = t5433f_Total,
  # t5433f_priv = t5433f_priv,
  # t5433f_priv_cc = t5433f_priv_cc,
  # t5433f_priv_sc = t5433f_priv_sc,
  t5433f_empregador = t5433f_empregador,
  t5433f_cp = t5433f_cp,
  trimestres = seqtri[1:length(t5433f_empregador)]
)

secm3 <- 2
ggplot(t5434df, aes(x=trimestres))+
  geom_line(aes(y = t5433f_empregador, color = "Empregador"))+
  geom_line(aes(y = t5433f_cp*secm3, color = "Trabalhador autônomo"))+
  scale_y_continuous(name = "Empregador", 
                     sec.axis = sec_axis(~./secm3, name = "Trabalhador autônomo", 
                                         labels = function(b) { paste0(round(b / 1, 0), ".")}))+
  scale_color_manual(values=c("#9999CC", "#CC6666"))+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(
    title = "Variação no rendimento nominal efetivamente recebido dos residentes de Santa Catarina",
    subtitle = "Rendimento médio nominal do trabalho principal, efetivamente recebido no mês de referência, pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho",
    caption = "Guilherme Viegas",
    colour = "Categoria do trabalho"
  )



