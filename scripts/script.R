library(ggplot2)
library(scales)

# BB - 2024

# oś czasu pusta

df <- data.frame(x = c(-2500, 2500))

p <- ggplot(df, aes(x = x)) +
  geom_blank() +
  scale_x_continuous(
    limits = c(-2500, 2500),
    breaks = seq(-2500, 2500, 500),
    minor_breaks = seq(-2500, 2500, 100),
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  labs(x = "rok") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Główne kreski
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(5, "pt"),
    
    # Mniejsze kreski
    axis.minor.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.minor.ticks.length = unit(2, "pt"),
    
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(hjust = 1, size = 10, vjust = -1)
  )

ggsave(
  filename = "./sources/BB-0153-a-24-1-o-4.png",
  plot = p,
  width = 12,
  height = 6,
  units = "cm",
  dpi = 300
)

# oś czasu strzałka

df <- data.frame(x = c(-2500, 2500))

p <- ggplot(df, aes(x = x)) +
  geom_blank() +
  scale_x_continuous(
    limits = c(-2500, 2500),
    breaks = seq(-2500, 2500, 500),
    minor_breaks = seq(-2500, 2500, 100),
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  labs(x = "rok") +
  coord_fixed(ratio = 1) +
  # --- DODANA STRZAŁKA I ETYKIETA ---
  # Strzałka prowadząca z góry (od y = 0.3 do y = 0) w punkcie x = -476
  annotate(
    "segment", 
    x = -476, xend = -476, 
    y = 800, yend = 300, 
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "black", linewidth = 0.8
  ) +

theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    # Główne kreski
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(5, "pt"),
    
    # Mniejsze kreski
    axis.minor.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.minor.ticks.length = unit(2, "pt"),
    
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(hjust = 1, size = 10, vjust = -1)
  )

print(p)

ggsave(
  filename = "./sources/BB-0153-a-24-1-o-11.png",
  plot = p,
  width = 12,
  height = 6,
  units = "cm",
  dpi = 300
)


# siatka strumień pusta


p <- ggplot() +
  scale_x_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, by = 0.2),
    minor_breaks = seq(0, 2, by = 0.1),
    expand = expansion(mult = c(0.02, 0.02)),
    labels = label_comma(decimal.mark = ",", accuracy = 0.1)
  ) +
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, by = 0.2),
    minor_breaks = seq(0, 2, by = 0.1),
    expand = expansion(mult = c(0.02, 0.02)),
    labels = label_comma(decimal.mark = ",", accuracy = 0.1)
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("odległość") ~ "(metry)"),
    y = quote("→" ~ italic("wysokość") ~  "(metry)")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "gray50", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray75", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(hjust = 1, face = "italic", size = 12),
    axis.title.y = element_text(angle = 90, vjust = 0.5, face = "italic", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = "./sources/BB-0153-a-24-1-o-6.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# siatka strumień wypełniona

df <- dplyr::tibble(x = c(0, 0.4, 0.8, 1.2, 1.6, 2)) |>
  dplyr::mutate(y = 2 - 0.5 * x * x)

p <- ggplot(df, aes(x = x, y = y)) +
  # --- DODANA KRZYWA I PUNKTY ---
  geom_smooth(color = "black", linewidth = 1) +       # Linia/krzywa łącząca punkty
  geom_point(color = "black", size = 2.5) +          # Punkty z ramki danych
  # -----------------------------
scale_x_continuous(
  limits = c(0, 2),
  breaks = seq(0, 2, by = 0.2),
  minor_breaks = seq(0, 2, by = 0.1),
  expand = expansion(mult = c(0.02, 0.02)),
  labels = label_comma(decimal.mark = ",", accuracy = 0.1)
) +
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, by = 0.2),
    minor_breaks = seq(0, 2, by = 0.1),
    expand = expansion(mult = c(0.02, 0.02)),
    labels = label_comma(decimal.mark = ",", accuracy = 0.1)
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("odległość") ~ "(metry)"),
    y = quote("→" ~ italic("wysokość") ~  "(metry)")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "gray50", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray75", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(hjust = 1, face = "italic", size = 12),
    axis.title.y = element_text(angle = 90, vjust = 0.5, face = "italic", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p)

ggsave(
  filename = "./sources/BB-0153-a-24-1-o-12.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)


# siatka Bravo bez y label

p <- ggplot() +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    minor_breaks = seq(0, 50, by = 5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 32),
    breaks = seq(0, 32, by = 4),
    minor_breaks = NULL,
    labels = c("0", rep("......", 8)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("liczba tygodni")),
    y = quote("→" ~ italic("łączna liczba numerów Bravo"))
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "gray50", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray75", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(hjust = 1, face = "italic", size = 12),
    axis.title.y = element_text(angle = 90, vjust = 0.5, face = "italic", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )

p


ggsave(
  filename = "./sources/BB-0153-a-24-1-o-9.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# siatka Bravo z y label

df <- dplyr::tibble(x = c(0, 10, 20, 30, 40, 50)) |>
  dplyr::mutate(y = 4 + x / 2)

p <- ggplot(df, aes(x = x, y = y)) +
  # --- LINIA I PUNKTY ---
  geom_line(color = "black", linewidth = 1) +          
  geom_point(color = "black", fill = "white", shape = 21, size = 3, stroke = 1) + 
  # -----------------------------
scale_x_continuous(
  limits = c(0, 50),
  breaks = seq(0, 50, by = 10),
  minor_breaks = seq(0, 50, by = 5),
  expand = expansion(mult = c(0.02, 0.02))
) +
  scale_y_continuous(
    limits = c(0, 32),
    breaks = seq(0, 32, by = 4),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("liczba tygodni")),
    y = quote("→" ~ italic("łączna liczba numerów Bravo")) # Przy pionowym tekście lepiej sprawdzi się w jednej linii z strzałką
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "gray50", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray75", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(hjust = 1, face = "italic", size = 12),
    # Zmieniono angle na 90, aby napis był pionowy:
    axis.title.y = element_text(angle = 90, vjust = 0.5, face = "italic", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p)

ggsave(
  filename = "./sources/BB-0153-a-24-1-o-13.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# KB - 2024


