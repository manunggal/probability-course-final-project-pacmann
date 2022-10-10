library(tidyverse)
data = read_csv("insurance.csv")
View(data)
View(data %>% filter(sex == "male"))

# data grouped by sex
data_sex_grouped = data %>%
  group_by(sex) %>%
  summarise(bmi_mean = mean(bmi),
            bmi_sd = sd(bmi),
            age_mean = mean(age),
            charges_mean = mean(charges),
            charges_median = median(charges))

#data grouped by smoking status
data_smoker_grouped = data %>%
  group_by(smoker) %>%
  summarise(
    bmi_mean = mean(bmi),
    charges_mean = mean(charges)
  )

# data grouped by smoking status for BMI > 25
data_smoker_bmi25_grouped = data %>%
  filter(bmi > 25) %>%
  group_by(smoker) %>%
  summarise(
    bmi_mean = mean(bmi),
    charges_mean = mean(charges)
  )

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# distribution plot
# mean of age




#
age_dist_plot = ggplot(data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "dodge", color = "grey") +
  geom_vline(data = data_sex_grouped, aes(xintercept = age_mean, color = sex)) +
  annotate("text", x=40, y=90, label="Female age mean = 39.5", angle=90) +
  annotate("text", x=38, y=90, label="Male age mean = 38.9", angle=90) +
  labs(title = "Age Histogram by Sex Group", x = 'Age', y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# bmi
# bmi berdasarkan sex
bmi_dist_plot = ggplot(data, aes(x = bmi, fill = sex)) +
  geom_density(alpha = .3) +
  geom_vline(
    data = data_sex_grouped,
    aes(xintercept = bmi_mean, color = sex),
    linetype = "longdash", size=1)+
  scale_x_continuous(minor_breaks = seq(0, 60, 2)) +
  # plot axis text
  xlab("BMI") + ylab("Density") +
  theme(
    panel.grid.major = element_line(
      size = rel(2)),
    panel.grid.minor = element_line(
      size = rel(0.5))
  ) +
  annotate("text", x=29.5, y=0.04, label="Female BMI mean = 30.4", angle=90) +
  annotate("text", x=31.5, y=0.04, label="Male BMI mean = 30.9", angle=90)

# bmi berdasarkan smoking status
bmi_smoker_dist_plot = ggplot(data, aes(x = bmi, fill = smoker)) +
  geom_density(alpha = .3) +
  geom_vline(
    data = data_smoker_grouped,
    aes(xintercept = bmi_mean, color = smoker),
    linetype = "longdash", size=1)+
  scale_x_continuous(minor_breaks = seq(0, 60, 2)) +
  # plot axis text
  labs(title = "BMI for Smokers vs non Smokers", x = 'BMI', y = "Density") +
  theme(
    panel.grid.major = element_line(
      size = rel(2)),
    panel.grid.minor = element_line(
      size = rel(0.5)),
    plot.title = element_text(hjust = 0.5)
  )


# Tagihan berdasarkan merokok
charges_dist_plot = ggplot(data, aes(x = charges, fill = smoker)) +
  geom_density(alpha = .3) +
  geom_vline(
    data = data_smoker_grouped,
    aes(xintercept = charges_mean, color = smoker),
    linetype = "longdash", size=1) +
  # plot axis text
  labs(title = "Charges for Smokers vs non Smokers", x = 'Charges', y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=31000, y=4e-05, label="Smoker charge mean = 32.050", angle=90) +
  annotate("text", x=7500, y=4e-05, label="non-Smoker charge mean = 8434", angle=90)


# tagihan untuk BMI di atas 25
charges_bmi_plus25_plot = ggplot(data %>% filter(bmi >25), aes(x = charges, fill = smoker)) +
  geom_density(alpha = .3) +
  geom_vline(
    data = data_smoker_bmi25_grouped,
    aes(xintercept = charges_mean, color = smoker),
    linetype = "longdash", size=1) +
  # plot axis text
  labs(title = "Charges for Smokers vs non Smokers (BMI >25)", x = 'Charges', y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=36000, y=4e-05, label="Smoker charge mean = 35.117", angle=90) +
  annotate("text", x=8000, y=4e-05, label="non-Smoker charge mean = 8.630", angle=90)

# gender dengan tagihan tertinggi
charges_sex_boxplot = ggplot(data, aes(x = sex, y = charges, fill = sex)) +
  geom_boxplot() +
  stat_summary(fun ="mean", color = '#f5f390') +
  # plot axis text
  labs(title = "Charges Boxplot by Sex", x = 'Sex', y = "Charges") +
  theme(plot.title = element_text(hjust = 0.5))


# gender dengan tagihan tertinggi grouped by smoker
charges_sex_smoker_boxplot = ggplot(data, aes(x = sex, y = charges, fill = smoker)) +
  geom_boxplot()

# distribusi peluang tiap region
charges_by_region_densityplot =  ggplot(data, aes(x = charges, fill = region)) +
  geom_density(alpha = .7) +
  # plot axis text
  labs(title = "Charges Distribution per Region", x = 'Charges', y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

# banyaknya entries per group
proporsi_region_plot = ggplot(data %>% count(region), aes(x = region, y = n)) +
  geom_bar(stat = 'identity', width = 0.5) +
  # plot axis text
  labs(title = "Proporsi Data per Region", x = 'Region', y = "Entries") +
  theme(plot.title = element_text(hjust = 0.5))
View(data %>% count(smoker))

# jumlah smoker
proporsi_smoker_plot = ggplot(data %>% count(smoker), aes(x = smoker, y = n)) +
  geom_bar(stat = 'identity', width = 0.5) +
  # plot axis text
  labs(title = "Proporsi Smoker", x = 'Smoker', y = "Entries") +
  theme(plot.title = element_text(hjust = 0.5))

# grouped stacked perokok
smoker_sex_data = data %>%
  filter(smoker == 'yes') %>%
  count(sex) %>%
  mutate(smoker = "smoker")

smoker_sex_plot = ggplot(smoker_sex_data,
                         aes(x = smoker, y = n, fill = sex)) +
  geom_col() +
  geom_text(aes(label = paste0(round(n/sum(smoker_sex_data$n)*100, digit = 0), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Smoker Numbers per Sex", x = 'Smoker', y = "Entries") +
  theme(plot.title = element_text(hjust = 0.5))

# peluang tagihan berdasarkan BMI

# category bmi
data = data %>%
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "underweight",
      bmi >= 18.5 & bmi <= 25 ~ "normal",
      bmi > 25 & bmi <= 30 ~ "overweight",
      bmi > 30 ~ "obese"
    )
  )
data_bmi = data %>%
  group_by(bmi_cat) %>%
  summarise(charges_mean = mean(charges))

data_bmi = data_bmi %>%
  mutate(probability_charges = round(charges_mean / sum(charges_mean)*100))

charges_by_bmi_densityplot =  ggplot(data, aes(x = bmi, y = charges, color = bmi_cat, shape = smoker)) +
  geom_point(size = 2) +
  # plot axis text
  labs(title = "Charges vs bmi for smoker and non-smoker", x = 'BMI', y = "Charges") +
  theme(plot.title = element_text(hjust = 0.5))

# peluang bmmi > 25 mendapatkan tagihan > 16.700

bmi25_smoker_data = data %>%
  filter(bmi > 25 & smoker == 'yes' & charges > 16700)

# probability
prob_bmi25_charges16700_smoker = round((count(bmi25_smoker_data) / count(data))*100)


# peluang perokok dengan tagihan di atas 16700
smoker_charges16700_data = data %>%
  filter(smoker == 'yes' & charges > 16700)

prob_smoker_charges16700 = round((count(smoker_charges16700_data) / count(data))*100)




# mana yg mungkin terjadi, BMI >25 dan tagihan di atas 16.7, atau BMI < 25 dna tagihan > 16.7k
bmi_more25_charges167_data = data %>%
  filter(bmi > 25 & charges > 16700)

bmi_less25_charges167_data = data %>%
  filter(bmi < 25 & charges > 16700)

prob_bmi_more25_charges167 = round((count(bmi_more25_charges167_data) / count(data))*100)
prob_bmi_less25_charges167 = round((count(bmi_less25_charges167_data) / count(data))*100)


# mana yg mungkin terjadi, perokok, BMI >25 dan tagihan di atas 16.7, atau perokok BMI < 25 dna tagihan > 16.7k
bmi_smoker_more25_charges167_data = data %>%
  filter(smoker == 'yes' & bmi > 25 & charges > 16700)

bmi_smoker_less25_charges167_data = data %>%
  filter(smoker == 'yes' & bmi < 25 & charges > 16700)

prob_smoker_bmi_more25_charges167 = round((count(bmi_smoker_more25_charges167_data) / count(data))*100)
prob_smoker_bmi_less25_charges167 = round((count(bmi_smoker_less25_charges167_data) / count(data))*100)



# korelasi variable
library(corrplot)
# numeric data for correlation plot
cor_data = cor(data %>% select(age, bmi, children, charges))


corrplot(cor_data, method = 'square', addCoef.col = 'black')

# Pengujian hipotesa
# asumsi sample <=30 tanpa info ttg sd

# sampling perokok
smoker_sample = slice_sample(data %>% filter(smoker == 'yes'), n = 30) %>% select(charges)
nonsmoker_sample = slice_sample(data %>% filter(smoker != 'yes'), n = 30) %>%  select(charges)

# t-test
t_test_tagihan_perokok = t.test(smoker_sample, nonsmoker_sample, var.equal = TRUE, alternative = "greater")


# tagihan kesehatan BMI > 25 vs BMI < 25
bmi_plus25_sample = slice_sample(data %>% filter(bmi > 25), n = 30) %>% select(charges)
bmi_less25_sample = slice_sample(data %>% filter(bmi <= 25), n = 30) %>% select(charges)

t_test_tagihan_bmi = t.test(bmi_plus25_sample, bmi_less25_sample, var.equal = TRUE, alternative = "greater")


# tagihan kesehatan laki2 lebih besar dari perempuan
male_sample = slice_sample(data %>% filter(sex == 'male'), n = 30) %>% select(charges)
female_sample = slice_sample(data %>% filter(sex == 'female'), n = 30) %>% select(charges)

t_test_tagihan_sex = t.test(male_sample, female_sample, var.equal = TRUE, alternative = "greater")
