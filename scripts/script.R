library(ggplot2)
library(scales)
library(ggbreak)

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
    y = quote("→" ~ italic("wysokość ") ~  "(metry)")
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
  geom_function(
    fun = function(x)  2 - 0.5 * x * x,
    xlim = c(0, 2),
    color = "black",
    linewidth = 1
  ) +       # Linia/krzywa łącząca punkty
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
    y = quote("→" ~ italic("wysokość" ) ~  "(metry)")
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
  geom_point(color = "black", size = 2.5) + 
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

# kokos pusty

p <- ggplot() +
  scale_x_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 2),
    minor_breaks = seq(0, 20, by = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 700),
    breaks = seq(0, 700, by = 100),
    minor_breaks = seq(0, 700, by = 50),
    expand = expansion(mult = c(0.02, 0.02))
  ) + scale_x_break(c(1, 12)) + # , space = 0.3 
#  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("d") ~ "(cm)"),
    y = quote("→" ~ italic("ilość włókna kokosowego ") ~  "(cm³)")
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
  filename = "./sources/KB-0153-a-24-1-u-2.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# kokos pełny

df <- dplyr::tibble(x = c(12, 14, 16, 18, 20)) |>
  dplyr::mutate(y = 1.6 * x^2 - 1.6 * x - 6)

p <- ggplot(df, aes(x = x, y = y)) +
  # --- LINIA I PUNKTY ---
  geom_function(
    fun = function(x) 1.6*x^2 - 1.6*x - 6,
    xlim = c(12, 20),
    color = "black",
    linewidth = 1
  ) + 
  geom_point(color = "black", size = 2.5) +          # Punkty z ramki danych
  scale_x_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 2),
    minor_breaks = seq(0, 20, by = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 700),
    breaks = seq(0, 700, by = 100),
    minor_breaks = seq(0, 700, by = 50),
    expand = expansion(mult = c(0.02, 0.02))
  ) + scale_x_break(c(1, 12)) + # , space = 0.3 
#    coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("d") ~ "(cm)"),
    y = quote("→" ~ italic("ilość włókna kokosowego ") ~  "(cm³)")
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
  filename = "./sources/KB-0153-a-24-1-c-1.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# Lodowiec - bez y label

p <- ggplot() +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    minor_breaks = seq(0, 60, by = 5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 20),
    minor_breaks = seq(0, 180, by = 10),
    labels = c("0", rep("......", 9)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 0.5) +
  labs(
    x = quote("→" ~ italic("t") ~ "(lat)"),
    y = quote("→" ~ italic("O") ~  "(km²)") 
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

p


ggsave(
  filename = "./sources/KB-0153-a-24-1-u-3.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# Lodowiec pełny

df <- dplyr::tibble(x = c(0, 10, 20, 30, 40, 50, 60)) |>
  dplyr::mutate(y = 165 * 0.96^x)

p <- ggplot(df, aes(x = x, y = y)) +
  # --- LINIA I PUNKTY ---
  geom_function(
    fun = function(x) 165 * 0.96^x,
    xlim = c(0, 60),
    color = "black",
    linewidth = 1
  ) + 
  geom_point(color = "black", size = 2.5) +  
  # -----------------------------
scale_x_continuous(
  limits = c(0, 60),
  breaks = seq(0, 60, by = 10),
  minor_breaks = seq(0, 60, by = 5),
  expand = expansion(mult = c(0.02, 0.02))
) +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 20),
    minor_breaks = seq(0, 180, by = 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 0.5) +
  labs(
    x = quote("→" ~ italic("t") ~ "(lat)"),
    y = quote("→" ~ italic("O") ~  "(km²)") # Przy pionowym tekście lepiej sprawdzi się w jednej linii z strzałką
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
  filename = "./sources/KB-0153-a-24-1-c-2.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# Oszczędzanie - bez y label

p <- ggplot() +
  # --- LINIA ---
  geom_function(
    fun = function(x) (50.085 * (1.0017^x - 1)) / 0.0017 + 50,
    xlim = c(0, 50),
    color = "black",
    linewidth = 1
  ) + 
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = seq(0, 3000, by = 500),
    minor_breaks = NULL,
    labels = c("0", rep("......", 6)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 0.017) +
  labs(
    x = quote("→" ~ italic("n") ~ "(miesięcy)"),
    y = quote("→" ~ italic("B") ~  "(euro)")
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

p

ggsave(
  filename = "./sources/GT-0153-a-24-1-u-1.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# Oszczędzanie pełny

df <- dplyr::tibble(x = c(0, 10, 20, 30, 40, 50)) |>
  dplyr::mutate(y = (50.085 * (1.0017^x - 1)) / 0.0017 + 50)

p <- ggplot() +
  # --- LINIA ---
  geom_function(
    fun = function(x) (50.085 * (1.0017^x - 1)) / 0.0017 + 50,
    xlim = c(0, 50),
    color = "black",
    linewidth = 1
  ) + 
scale_x_continuous(
  limits = c(0, 50),
  breaks = seq(0, 50, by = 10),
  minor_breaks = NULL,
  expand = expansion(mult = c(0.02, 0.02))
) +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = seq(0, 3000, by = 500),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 0.017) +
  labs(
    x = quote("→" ~ italic("n") ~ "(miesięcy)"),
    y = quote("→" ~ italic("B") ~  "(euro)")
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
  filename = "./sources/GT-0153-a-24-1-c-1.png",
  plot = p,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 300
)

# harfa - pusta

df <- dplyr::tibble(x = seq(from = 0, to = 4.2, by = 0.1)) |>
  dplyr::mutate(y = 1.6 * x) |>
  dplyr::mutate(z = 0.07 * x^3 - 0.4 * x^2 + 9.25) |>
  dplyr::mutate(idx = round(10 * x)) |>
  dplyr::mutate(
    typ = dplyr::case_when(
      idx %% 7 == 0 ~ "czerwony",
      idx %% 7 == 3 ~ "czarny",
      .default = "szary"
    )
  ) |>
  dplyr::mutate(
    color = dplyr::case_when(
      typ == "szary" ~ "gray70",
      .default = "black"
    )
  ) |>
  dplyr::mutate(
    linetype = dplyr::case_when(
      typ == "czerwony" ~ "dashed",
      .default = "solid"
    )
  )

df_idx <- df |> dplyr::filter(idx %in% c(0:7, 10, 14, 21, 28, c(35: 42)))

df_pts <- dplyr::tribble(
  ~x,   ~y,    ~label, ~dx,   ~dy,
  0,    0,     "A",     0.35, -0.0 ,
  0,    9.25,  "C",     0.0 ,  0.35,
  3.5,  5.6,   "L",     0.25 , -0.25,
  3.5,  7.35,  "M",     0.0 ,  0.35,
  4.2,  6.72,  "P",     0.25 , -0.25,
  4.2,  7.38,  "Q",     0.0 ,  0.35,
  5,    8,     "B",     0.25,  0.25
)


p <- ggplot() +
  # --- LINIA ---
  geom_function(
    fun = function(x) 1.6 * x,
    xlim = c(0, 5),
    color = "black",
    linewidth = 1
  ) + 
  # --- KRZYWA ---
  geom_function(
    fun = function(x) 0.07 * x^3 - 0.4 * x^2 + 9.25,
    xlim = c(0, 5),
    color = "black",
    linewidth = 1
  ) + 
  geom_point(data = df, mapping = aes(x = x, y = y), color = "black", size = 1.0) +
  geom_point(data = df, mapping = aes(x = x, y = z), color = "black", size = 1.0) +
  geom_point(data = df_pts, mapping = aes(x = x, y = y), color = "black", size = 1.5) +
  geom_text(
    data = df_pts,
    mapping = aes(x = x + dx, y = y + dy, label = label),
    size = 4,
    fontface = "italic"
  ) + 
  geom_segment(data = df_idx, 
               mapping = aes(x = x, y = y, xend = x, yend = z, 
                             color = color, linetype = linetype),
               linewidth = 0.6) +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_x_continuous(
    limits = c(0, 6),
    breaks = seq(0, 6, by = 1),
    minor_breaks = seq(0, 6, by = 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    minor_breaks = seq(0, 10, by = 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("a") ~ "(dm)"),
    y = quote("→" ~ italic("h") ~ "(dm)")
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
  filename = "./sources/GT-0153-a-24-1-u-2.png",
  plot = p,
  width = 12,
  height = 18,
  units = "cm",
  dpi = 300
)


# harfa - wypełniona

df <- dplyr::tibble(x = seq(from = 0, to = 4.2, by = 0.1)) |>
  dplyr::mutate(y = 1.6 * x) |>
  dplyr::mutate(z = 0.07 * x^3 - 0.4 * x^2 + 9.25) |>
  dplyr::mutate(idx = round(10 * x)) |>
  dplyr::mutate(
    typ = dplyr::case_when(
      idx %% 7 == 0 ~ "czerwony",
      idx %% 7 == 3 ~ "czarny",
      .default = "szary"
    )
  ) |>
  dplyr::mutate(
    color = dplyr::case_when(
      typ == "szary" ~ "gray70",
      .default = "black"
    )
  ) |>
  dplyr::mutate(
    linetype = dplyr::case_when(
      typ == "czerwony" ~ "dashed",
      .default = "solid"
    )
  )

df_idx <- df |> dplyr::filter(idx %in% c(0:7, 10, 14, 17, 21, 24, 28, 31, c(35: 42)))

df_pts <- dplyr::tribble(
  ~x,   ~y,    ~label, ~dx,   ~dy,
  0,    0,     "A",     0.35, -0.0 ,
  0,    9.25,  "C",     0.0 ,  0.35,
  3.5,  5.6,   "L",     0.25 , -0.25,
  3.5,  7.35,  "M",     0.0 ,  0.35,
  4.2,  6.72,  "P",     0.25 , -0.25,
  4.2,  7.38,  "Q",     0.0 ,  0.35,
  5,    8,     "B",     0.25,  0.25
)


p <- ggplot() +
  # --- LINIA ---
  geom_function(
    fun = function(x) 1.6 * x,
    xlim = c(0, 5),
    color = "black",
    linewidth = 1
  ) + 
  # --- KRZYWA ---
  geom_function(
    fun = function(x) 0.07 * x^3 - 0.4 * x^2 + 9.25,
    xlim = c(0, 5),
    color = "black",
    linewidth = 1
  ) + 
  geom_point(data = df, mapping = aes(x = x, y = y), color = "black", size = 1.0) +
  geom_point(data = df, mapping = aes(x = x, y = z), color = "black", size = 1.0) +
  geom_point(data = df_pts, mapping = aes(x = x, y = y), color = "black", size = 1.5) +
  geom_text(
    data = df_pts,
    mapping = aes(x = x + dx, y = y + dy, label = label),
    size = 4,
    fontface = "italic"
  ) + 
  geom_segment(data = df_idx, 
               mapping = aes(x = x, y = y, xend = x, yend = z, 
                             color = color, linetype = linetype),
               linewidth = 0.6) +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_x_continuous(
    limits = c(0, 6),
    breaks = seq(0, 6, by = 1),
    minor_breaks = seq(0, 6, by = 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    minor_breaks = seq(0, 10, by = 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = quote("→" ~ italic("a") ~ "(dm)"),
    y = quote("→" ~ italic("h") ~ "(dm)")
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
  filename = "./sources/GT-0153-a-24-1-c-2.png",
  plot = p,
  width = 12,
  height = 18,
  units = "cm",
  dpi = 300
)

# wielościany foremne

f_4  <- function(x) 0.118 * x^3 # czworościan
f_6  <- function(x) 1.0   * x^3 # sześcian
f_8  <- function(x) 0.471 * x^3 # ośmiościan
f_12 <- function(x) 7.66  * x^3 # dwunastościan
f_20 <- function(x) 2.18  * x^3 # dwudziestościan

etykiety <- tibble::tribble(
  ~label, ~x,        ~dx,   ~dy,   ~fun,
  "V"  ,   10       - 1,  -0.25,  6,     f_4,
  "III",   5.646216 - 1,  -0.25,  5,     f_6,
  "IV" ,   7.251761 - 1,  -0.25,  5,     f_8,
  "I"  ,   2.864274 - 1,  -0.20,  5,     f_12,
  "II" ,   4.353838 - 1,  -0.25,  5,     f_20
) |>
  dplyr::rowwise() |>
  dplyr::mutate(y = fun(x)) |>
  dplyr::ungroup()

p <- ggplot() +
  # --- KRZYWA ---
  geom_function(
    fun = function(x) f_4(x),
    xlim = c(0, 10),
    color = "black",
    linewidth = 1
  ) + 
  geom_function(
    fun = function(x) f_6(x),
    xlim = c(0, 5.646216),
    color = "black",
    linewidth = 1
  ) + 
  geom_function(
    fun = function(x) f_8(x),
    xlim = c(0, 7.251761),
    color = "black",
    linewidth = 1
  ) + 
  geom_function(
    fun = function(x) f_12(x),
    xlim = c(0, 2.864274),
    color = "black",
    linewidth = 1
  ) + 
  geom_function(
    fun = function(x) f_20(x),
    xlim = c(0, 4.353838),
    color = "black",
    linewidth = 1
  ) + 
  # --- ETYKIETY I–V ---
  geom_text(
    data = etykiety,
    mapping = aes(x = x + dx, y = y + dy, label = label),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 20),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_fixed(ratio = 10/180) +
  labs(
    x = quote("→" ~ italic("krawędź")),
    y = quote("→" ~ italic("objętość"))
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
  filename = "./sources/GT-0153-a-24-1-u-3.png",
  plot = p,
  width = 12,
  height = 18,
  units = "cm",
  dpi = 300
)




# -----------------------------------------------------------------------------
# kompiluj

files <- list.files("./sources/", pattern = "\\.qmd$")

for (file in files) {
  # Ścieżka do pliku w folderze sources
  file_path <- file.path("./sources", file)
  
  # system2 oczekuje komendy oraz argumentów jako wektora
  system2("quarto", args = c("render", file_path))
}

system2("quarto", args = c("render"))


