
# Define colour palette with SG colours

SGblue <- "#0065bd"
SGblue2 <- "#002d54"
SGgrey <- "#333333"

Ochres <- c("#634100", "#D6A319")
#scales::show_col(Ochres)

SGblues <- c(SGblue2, "#004989", SGblue, "#4181DD", "#75AAFF", "#ADDDFF", "#E6F4F1")
#scales::show_col(SGblues)

# Categorical scale for 2 categories
SGmix2 <- c(SGblue, Ochres[2])
#scales::show_col(SGmix2)

# Categorical scale for 3 categories
SGmix3 <- c(SGblue, Ochres)
#scales::show_col(SGmix3)
  
# Categorical scale for 4 categories
SGmix4 <- c(SGblue, SGblues[5], Ochres[2:1])
#scales::show_col(SGmix4)

# Categorical scale for up to 5 categories
SGmix6 <- colorRampPalette(SGmix4)(6)
#scales::show_col(SGmix6)

# Categorical scale for up to 6 categories
SGmix5 <- colorRampPalette(SGmix4)(5)
#scales::show_col(SGmix5)

# Categorical scale for up to 7 categories
SGmix7 <- colorRampPalette(SGmix4)(7)
#scales::show_col(SGmix7)

# Categorical scale for up to 8 categories
SGmix8 <- colorRampPalette(SGmix4)(8)
#scales::show_col(SGmix8)

# Categorical scale for up to 10 categories
SGmix10 <- colorRampPalette(SGmix4)(10)
#scales::show_col(SGmix10)














