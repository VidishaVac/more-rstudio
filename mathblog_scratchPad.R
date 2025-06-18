# Blog 1 -- Coloring grid problem

# 1) write down math formula of permutations that you need to determine all possible options
# 2) show how you can use the following to get all permutations of the 4 colors, with NO restriction
library(gtools)
rm(list=ls())
x <- c('r', 'b', 'g', 'y')
P <- permutations(n=4,r=4,v=x,repeats.allowed=T) # 256x4 

# some data cleanup
colnames(P) <- c("A","B","C","D")
P <- data.frame(P)

# 3) Next, due to restrictions, since we need only to include only certain permutations,
# show how this can be done using the following steps

# a) create column that has all permutations concatenated
P$concat <- paste(P$A, P$B, P$C, P$D, P$A, sep="")

# b) use pattern searching logic, using regex/grepl (how to do this in dplyr with pipe)
# to indicate which permutations we keep vs throw out
# along the way, when possible, show the Math formulas for each & explain how the following
# links with what the formulas actually mean


library(dplyr)
library(magrittr)

# Older -- not used
P <- P %>% 
  mutate(rr = if_else(grepl("rr", concat)==TRUE,1,0), 
         bb = if_else(grepl("bb", concat)==TRUE,1,0), 
         gg = if_else(grepl("gg", concat)==TRUE,1,0), 
         yy = if_else(grepl("yy", concat)==TRUE,1,0))

# c) finally, sum up only those that follow the restrictions
# again when possible show Math formulas 
P$sum <- P$rr+P$bb+P$gg+P$yy
P$final <- ifelse(P$sum==0,1,0)
sum(P$final)


# Compact -- use this
P <- P %>% 
  mutate(concat=paste(P$A, P$B, P$C, P$D, P$A, sep="")) %>%
  mutate(flag = case_when(grepl("rr", concat) ~ "invalid", 
                          grepl("bb", concat) ~ "invalid", 
                          grepl("gg", concat) ~ "invalid", 
                          grepl("yy", concat) ~ "invalid", .default = "valid")) %>%
  group_by(flag) %>% summarise(count=n())
knitr::kable(P)
  
# Function to flag configurations with exactly 0, 1, 2 or 4 invalid adjacencies
flag_invalid_adj <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Check each adjacency pair for invalidity
      ab_invalid = A == B,
      bc_invalid = B == C,
      cd_invalid = C == D,
      ad_invalid = A == D,
      # Count the number of invalid pairs
      invalid = sum(ab_invalid, bc_invalid, cd_invalid, ad_invalid),
      # Flag configurations with exactly two, three, or four invalid pairs
      flag_two_invalid = as.integer(invalid == 2),
      flag_three_invalid = as.integer(invalid == 3),
      flag_four_invalid = as.integer(invalid == 4)
    ) %>%
    ungroup() 
}
cols <- P[,1:4]
invalid_data <- flag_invalid_adj(cols) %>% select(A, B, C, D, invalid)
finalCounts <- invalid_data %>% group_by(invalid) %>% summarise(count=n())

# 3) Draw the squares

# Select first row of all groups
s <- invalid_data %>% group_by(invalid) %>% filter(row_number()==4)

# Load necessary libraries for elegant visualizations
library(ggplot2)
library(gridExtra)  
library(showtext)   # For using custom fonts

# Load a font using showtext
font_add_google("Rouge Script", "rouge script")  # Example: Adding the "Lobster" font
showtext_auto()  # Automatically use showtext for all plots

# Step 1: Create my color tibble (4x4)
sq <- s %>% 
  mutate_all(~ case_when(
    . == "b" ~ "blue",
    . == "g" ~ "green",
    . == "y" ~ "yellow",
    . == "r" ~ "red",
    TRUE ~ .  # Keep original value if it doesn't match
  ))

# Step 2: Convert tibble to a matrix
color_matrix <- sq[,-5] %>% select(A,B,D,C) %>% as.matrix() # to match the coloring order of ggplot2

# Check the dimensions of the matrix
#print(dim(color_matrix))  # Should be 4x4

# Step 3: Function to create a plot based on a color vector and a title
create_plot <- function(colors, title) {
  df <- expand.grid(x = 0:1, y = 0:1)
  df$color <- colors
  
  ggplot(df, aes(x = x, y = y, fill = color)) +
    geom_tile(color = "white") +  # Change border color to white
    scale_fill_identity() +        # Use the colors as they are
    theme_minimal() +              # Use a minimal theme
    coord_fixed() +                # Keep aspect ratio
    labs(title = title) +          # Add the title
    theme(
      legend.position = "none",    # Remove legend
      axis.title = element_blank(), # Suppress axis titles
      axis.text = element_blank(),  # Suppress axis text
      axis.ticks = element_blank(), # Suppress axis ticks
      panel.grid = element_blank(),   # Suppress grid lines
      plot.title = element_text(family = "rouge script", size = 20, hjust = 0.5, face = )  # Use the custom font for the title
    )
}

# Step 4: Create a vector of titles for each plot
titles <- c("Plot 1", 
            "Plot 2", 
            "Plot 3", 
            "Plot 4")

# Step 5: Create a list of plots using the color matrix and titles
plots <- lapply(1:nrow(color_matrix), function(i) {
  create_plot(color_matrix[i, ], titles[i])
})

# Create an overall title
overall_title <- textGrob("Overall Title", gp = gpar(fontsize = 20, fontfamily = "rouge script"))

# Create a spacer with reduced height
spacer <- rectGrob(gp = gpar(fill = NA, col = NA), width = unit(1, "npc"), height = unit(0.5, "lines"))

# Arrange the overall title and plots
grid.arrange(overall_title, spacer, 
             arrangeGrob(grobs=plots, ncol = 2), 
             ncol = 1, heights = c(1, 0.1, 4))