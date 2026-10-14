library(tidyverse)
library(lubridate)
theme_set(theme_gray(base_family = "AppleGothic"))

# ===== 데이터 불러오기 =====
music <- read_csv("중간대체과제/EvolutionPopUSA_MainData.csv")
glimpse(music)

# ===== 데이터 준비 =====
df <- music %>%
  select(recording_id, artist_name_clean, track_name,
         first_entry, quarter, year, era,
         starts_with("tTopic"))

dim(df)
colSums(is.na(df))

range(df$year)
summary(df$tTopic_05)
summary(rowSums(select(df, starts_with("tTopic"))))
df %>% count(year, quarter) %>% filter(n < 10)

df <- df %>%
  mutate(q      = factor(str_extract(quarter, "Q[1-4]"),
                         levels = c("Q1", "Q2", "Q3", "Q4")),
         month  = month(ymd(first_entry)),
         summer = (q == "Q3"))

table(df$q, df$month)
table(df$q, useNA = "ifany")

# ===== EDA: Variation =====
ggplot(df, aes(x = tTopic_05)) +
  geom_histogram(binwidth = 0.02) +
  labs(x = "T5 비중 (guitar, loud, energetic)", y = "곡 수")

# ===== EDA: Covariation =====
ggplot(df, aes(x = q, y = tTopic_05)) +
  geom_boxplot() +
  labs(x = "차트 진입 분기", y = "T5 비중 (guitar, loud, energetic)")

df %>%
  group_by(q) %>%
  summarize(n = n(), mean = mean(tTopic_05), sd = sd(tTopic_05))

df %>%
  group_by(q) %>%
  summarize(m = mean(tTopic_05), se = sd(tTopic_05) / sqrt(n()), n = n()) %>%
  ggplot(aes(q, m)) +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se)) +
  labs(x = "차트 진입 분기", y = "T5 평균 비중 (95% CI)")

# ===== EDA: Conditioning (연도) =====
df %>%
  count(year, q) %>%
  ggplot(aes(year, q, fill = n)) +
  geom_tile() +
  labs(x = "연도", y = "차트 진입 분기", fill = "곡 수")

df <- df %>%
  group_by(year) %>%
  mutate(t5_rel = tTopic_05 - mean(tTopic_05)) %>%
  ungroup()

ggplot(df, aes(x = q, y = t5_rel)) +
  geom_boxplot() +
  labs(x = "차트 진입 분기", y = "연도 대비 T5 상대 비중")

df %>%
  group_by(q) %>%
  summarize(m = mean(t5_rel), se = sd(t5_rel) / sqrt(n()), n = n()) %>%
  ggplot(aes(q, m)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se)) +
  labs(x = "차트 진입 분기", y = "연도 대비 T5 상대 비중 평균 (95% CI)")

df %>%
  group_by(month) %>%
  summarize(m = mean(t5_rel), se = sd(t5_rel) / sqrt(n()), n = n()) %>%
  ggplot(aes(month, m)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se)) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "차트 진입 월", y = "연도 대비 T5 상대 비중 평균 (95% CI)")

df %>%
  group_by(era, q) %>%
  summarize(m = mean(t5_rel), se = sd(t5_rel) / sqrt(n()), n = n(), .groups = "drop") %>%
  ggplot(aes(q, m)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se)) +
  geom_text(aes(label = paste0("n=", n)), y = -Inf, vjust = -0.5, size = 3) +
  facet_wrap(~ era, labeller = label_both) +
  labs(x = "차트 진입 분기", y = "연도 대비 T5 상대 비중 평균 (95% CI)")

# ===== Exceptions: T5 지배 곡 =====
df %>%
  group_by(q) %>%
  summarize(n = n(), share_dominant = mean(tTopic_05 > 0.5))

# ===== 회귀: 3분기 vs 나머지 (연구 질문) =====
library(broom)

m_crude <- lm(tTopic_05 ~ summer, data = df)
m_lin   <- lm(tTopic_05 ~ summer + year, data = df)
m_fe    <- lm(tTopic_05 ~ summer + factor(year), data = df)

bind_rows(crude  = tidy(m_crude, conf.int = TRUE),
          linear = tidy(m_lin,   conf.int = TRUE),
          fe     = tidy(m_fe,    conf.int = TRUE),
          .id = "model") %>%
  filter(term == "summerTRUE") %>%
  select(model, estimate, conf.low, conf.high)

c(summary(m_crude)$r.squared, summary(m_lin)$r.squared, summary(m_fe)$r.squared)

# 보조: 분기별 (기준 1분기)
m_q <- lm(tTopic_05 ~ q + factor(year), data = df)
tidy(m_q, conf.int = TRUE) %>%
  filter(str_detect(term, "^q")) %>%
  select(term, estimate, conf.low, conf.high)

sd(df$tTopic_05)

plot(m_fe, which = 1)

df %>% count(artist_name_clean, sort = TRUE) %>% head(10)

# ===== 시대 상호작용 =====
df %>% group_by(era) %>% summarize(from = min(year), to = max(year), n = n())

m_int <- lm(tTopic_05 ~ q * factor(era) + factor(year), data = df)
anova(m_q, m_int)

# ===== 민감도 분석 =====
m_jja <- lm(tTopic_05 ~ I(month %in% 6:8) + factor(year), data = df)
m_t3  <- lm(tTopic_03 ~ summer + factor(year), data = df)

bind_rows(base = tidy(m_fe,  conf.int = TRUE),
          jja  = tidy(m_jja, conf.int = TRUE),
          t3   = tidy(m_t3,  conf.int = TRUE),
          .id = "model") %>%
  filter(str_detect(term, "summer|month")) %>%
  select(model, estimate, conf.low, conf.high)

# ===== 그림 3, 4: 계수 비교 =====
x_lim <- c(-0.007, 0.016)   # 두 그림의 x축을 같게 맞춤

# 그림 3: 회귀 모형 비교 (3분기 vs 나머지)
reg_df <- bind_rows(
  tidy(m_crude, conf.int = TRUE) %>% mutate(label = "① 분기만"),
  tidy(m_lin,   conf.int = TRUE) %>% mutate(label = "② + 연도(선형)"),
  tidy(m_fe,    conf.int = TRUE) %>% mutate(label = "③ + 연도 고정")
) %>%
  filter(term == "summerTRUE") %>%
  mutate(label = fct_rev(fct_inorder(label)))

ggplot(reg_df, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  coord_cartesian(xlim = x_lim) +
  labs(x = "3분기 vs 나머지 분기의 T5 비중 차이 (95% CI)", y = NULL)

ggsave("fig3_regression.png", width = 6.5, height = 2.4, dpi = 300)

# 그림 4: 민감도 분석 (기준 모형과 비교)
sens_df <- bind_rows(
  tidy(m_fe,  conf.int = TRUE) %>% filter(term == "summerTRUE") %>%
    mutate(label = "기준: T5, 3분기 vs 나머지"),
  tidy(m_jja, conf.int = TRUE) %>% filter(str_detect(term, "month")) %>%
    mutate(label = "계절 정의: T5, 6~8월 vs 나머지"),
  tidy(m_t3,  conf.int = TRUE) %>% filter(term == "summerTRUE") %>%
    mutate(label = "결과변수: T3, 3분기 vs 나머지")
) %>%
  mutate(label = fct_rev(fct_inorder(label)))

ggplot(sens_df, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  coord_cartesian(xlim = x_lim) +
  labs(x = "여름 vs 나머지의 비중 차이 (연도 고정, 95% CI)", y = NULL)

ggsave("fig4_sensitivity.png", width = 6.5, height = 2.4, dpi = 300)
