library(tidyverse)
library(arrow)

get_digits <- function(x) {
  x <- x[!is.na(x) & x >= 10]
  s <- sprintf("%.0f", floor(x))
  tibble(
    montant = x,
    d1 = as.integer(substr(s, 1, 1)),
    d2 = as.integer(substr(s, 2, 2))
  )
}

benford_d1 <- function() log10(1 + 1 / (1:9))
benford_d2 <- function() sapply(0:9, function(d) sum(log10(1 + 1 / (10 * (1:9) + d))))
benford_d12 <- function() log10(1 + 1 / (10:99))

mad_benford <- function(obs_prop, exp_prop) mean(abs(obs_prop - exp_prop))

test_benford <- function(digits, valeurs, theo) {
  obs_n <- table(factor(digits, levels = valeurs))
  n <- sum(obs_n)
  obs_p <- as.numeric(obs_n) / n
  chi <- chisq.test(as.numeric(obs_n), p = theo)
  list(
    table = tibble(chiffre = valeurs, observe = obs_p, theorique = theo),
    n = n,
    mad = mad_benford(obs_p, theo),
    p_value = chi$p.value
  )
}

plot_benford <- function(res, titre) {
  ggplot(res$table, aes(x = factor(chiffre))) +
    geom_col(aes(y = observe), fill = "steelblue") +
    geom_line(aes(y = theorique, group = 1), color = "red", linewidth = 1) +
    geom_point(aes(y = theorique), color = "red") +
    labs(title = titre,
         subtitle = paste0("n = ", res$n, " | MAD = ", round(res$mad, 4)),
         x = "Chiffre", y = "Frequence") +
    theme_minimal()
}

df <- read_parquet("decp.parquet")

df_clean <- df %>%
  filter(!is.na(montant), montant > 0) %>%
  filter(donneesActuelles %in% TRUE, !(montant_anomalie %in% TRUE)) %>%
  distinct(uid, .keep_all = TRUE)
nrow(df_clean)

d <- get_digits(df_clean$montant)

res1 <- test_benford(d$d1, 1:9, benford_d1())
res2 <- test_benford(d$d2, 0:9, benford_d2())
print(res1$mad); print(res2$mad); print(res1$n)
print(res1$p_value); print(res2$p_value)

p1 <- plot_benford(res1, "Benford - 1er chiffre (donnees nettoyees)")
p2 <- plot_benford(res2, "Benford - 2e chiffre (donnees nettoyees)")
ggsave("benford_1er_chiffre_propre.png", p1, width = 7, height = 4, dpi = 150)
ggsave("benford_2e_chiffre_propre.png", p2, width = 7, height = 4, dpi = 150)

d <- d %>% mutate(d12 = d1 * 10 + d2)
n <- nrow(d)
anomalies <- d %>%
  count(d12, name = "n_obs") %>%
  right_join(tibble(d12 = 10:99, p_theo = benford_d12()), by = "d12") %>%
  mutate(
    n_obs = replace_na(n_obs, 0L),
    p_obs = n_obs / n,
    z = (abs(p_obs - p_theo) - 1 / (2 * n)) / sqrt(p_theo * (1 - p_theo) / n),
    exces = n_obs - p_theo * n
  ) %>%
  arrange(desc(z))
print(head(anomalies, 10))

df_clean %>% count(montant, sort = TRUE) %>% head(15)
mean(df_clean$montant %% 10000 == 0)

acheteurs <- df_clean %>%
  mutate(rond = montant %% 10000 == 0) %>%
  group_by(acheteur_nom) %>%
  summarise(n_marches = n(), part_ronds = mean(rond)) %>%
  filter(n_marches >= 200) %>%
  arrange(desc(part_ronds)) %>%
  head(20)
print(acheteurs)
write_csv(acheteurs, "acheteurs_a_verifier.csv")