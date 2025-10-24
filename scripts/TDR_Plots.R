library(tidyverse)

TDR_AC <- read.csv("Table A-C - Sheet1.csv")
TDR_AC_corrected <- TDR_AC %>% 
  pivot_longer(
    cols = matches("^[A-Ka-k]\\.(beg|mid|end)$"),
    names_to = c("Bed", "Position"),
    names_pattern = "([A-C])\\.(beg|mid|end)",
    values_to = "TDR"
  ) %>% 
  mutate(
    Bed = toupper(Bed),
    Position = recode(Position, beg = "top", mid = "middle", end = "end")
  )

view(TDR_AC_corrected)

TDR_DF <- read.csv("Table D-F - Sheet1.csv")
TDR_DF_corrected <- TDR_DF %>% 
  pivot_longer(
    cols = matches("^[A-Ka-k]\\.(beg|mid|end)$"),
    names_to = c("Bed", "Position"),
    names_pattern = "([D-F])\\.(beg|mid|end)",
    values_to = "TDR"
  ) %>% 
  mutate(
    Bed = toupper(Bed),
    Position = recode(Position, beg = "top", mid = "middle", end = "end")
  )

view(TDR_DF_corrected)

TDR_GI <- read.csv("Table G-I - Sheet1.csv")

TDR_GI <- TDR_GI %>%
  rename(
    "I.mid" = "l.mid",
    "I.end" = "l.end"
  )

view(TDR_GI)

TDR_GI_corrected <- TDR_GI %>% 
  pivot_longer(
    cols = matches("^[A-Ka-k]\\.(beg|mid|end)$"),
    names_to = c("Bed", "Position"),
    names_pattern = "([G-I])\\.(beg|mid|end)",
    values_to = "TDR"
  ) %>% 
  mutate(
    Bed = toupper(Bed),
    Position = recode(Position, beg = "top", mid = "middle", end = "end")
  )

view(TDR_GI_corrected)

print(colnames(TDR_GI_corrected), quote = TRUE)



TDR_JK <- read.csv("Table J-K - Sheet1.csv")
TDR_JK_corrected <- TDR_JK %>% 
  pivot_longer(
    cols = matches("^[A-Ka-k]\\.(beg|mid|end)$"),
    names_to = c("Bed", "Position"),
    names_pattern = "([J-K])\\.(beg|mid|end)",
    values_to = "TDR"
  ) %>% 
  mutate(
    Bed = toupper(Bed),
    Position = recode(Position, beg = "top", mid = "middle", end = "end")
  )

view(TDR_JK_corrected)

tdr1 <- TDR_AC_corrected
tdr2 <- TDR_DF_corrected
tdr3 <- TDR_GI_corrected
tdr4 <- TDR_JK_corrected

combined_tdr <- bind_rows(tdr1, tdr2, tdr3, tdr4)
view(combined_tdr)


partial_na_dates <- combined_tdr %>% 
  group_by(date) %>% 
  summarise(
    any_na = any(is.na(TDR)),
    any_non_na = any(!is.na(TDR))
  ) %>% 
  filter(any_na & any_non_na) %>% 
  pull(date)

view(partial_na_dates)

write_csv(combined_tdr, "WL2_2024_TDR_Processed.csv")

#TDR plots
library(ggplot2)

ggplot(combined_tdr, aes(x = Bed, y = TDR, fill = Position)) +
  geom_boxplot() +
  labs(title = "TDR Variation",
       x = "Bed",
       y = "TDR (Soil Moisture)") +
  theme_minimal()

ggplot(combined_tdr, aes(x = Bed, y = TDR)) +
  geom_boxplot(aes(fill = Bed)) +
  facet_wrap(~Position) +
  labs(title = "TDR for Position by Bed",
       x = "Bed", y = "TDR") +
  theme_get()

ggplot(combined_tdr, aes(x = Bed, y = TDR)) +
  geom_boxplot(aes(fill = Bed)) +
  facet_wrap(~Position) +
  scale_fill_grey(start = 0.3, end = 0.8) +  
  labs(title = "TDR for Position by Bed",
       x = "Bed", y = "TDR") +
  theme_minimal()  

ggplot(combined_tdr, aes(x = Bed, y = TDR)) +
  geom_jitter(aes(color = Position), width = 0.2, alpha = 0.6) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(title = "Raw TDR Values by Bed and Position")

library(ggplot2)
library(dplyr)

bed_means <- combined_tdr %>% 
  group_by(Bed) %>% 
  summarize(mean_TDR = mean(TDR, na.rm = TRUE))

ggplot(bed_means, aes(x = Bed, y = mean_TDR)) +
  geom_col(fill = "steelblue") +
  labs(title = "Mean TDR for Each Bed", y = "Mean TDR", x = "Bed") +
  theme_minimal()

 ggplot(combined_tdr, aes(x = date, y = TDR, color = Bed)) +
   geom_point(size = 3, alpha = 0.5) +
   labs(title = "All TDR Points over Time", x = "Date", y = "TDR", color = "Bed") +
   theme_classic() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
   scale_color_viridis_d(option = "viridis")
 

 
 bed_avg <- combined_tdr %>% 
  group_by(date, Bed) %>% 
  summarize(mean_TDR = mean(TDR, na.rm = TRUE), .groups = "drop")


 
 ggplot(bed_avg, aes(x = date, y = mean_TDR, color = Bed)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Average TDR per Bed over Time", y = "Avg TDR", x = "Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  scale_color_viridis_d(option = "viridis")



position_avg <- combined_tdr %>% 
  group_by(date, Position) %>% 
  summarize(mean_TDR = mean(TDR, na.rm = TRUE), .groups = "drop")
  
ggplot(position_avg, aes(x = date, y = mean_TDR, color = Position)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Average TDR per Position over Time", y = "Avg TDR", x = "Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 11))
