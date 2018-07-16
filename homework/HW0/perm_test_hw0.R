library(dplyr)

fem_ds <- c(rep(1, 37), rep(0, 76-37))
fem_sex <- c(rep('Female', 76))
male_ds <- c(rep(1, 50), rep(0, 133-50))
male_sex <- c(rep('Male', 133))
fem_tab <- data.frame(fem_ds, fem_sex)
fem_tab <- rename(fem_tab, sex = fem_sex)
fem_tab <- rename(fem_tab, DS1 = fem_ds)
male_tab <- data.frame(male_ds, male_sex)
male_tab <- rename(male_tab, sex = male_sex)
male_tab <- rename(male_tab, DS1 = male_ds)
full_ds <- bind_rows(fem_tab, male_tab)
id_m <- which(full_ds$sex == "Male")
ds_m <- mean(male_ds)
id_f <- which(full_ds$sex == "Female")
ds_f <- mean(fem_ds)
obs <- ds_f - ds_m; obs
ds_col <- full_ds$DS1
# permutation test
N <- 10^5 - 1
diffs <- numeric(N)
for (i in 1:N) {
  # sample size of 76, from 1 to 209, without replacement
  index <- sample(209, size = 76, replace = FALSE)
  diffs[i] <- mean(ds_col[index]) - mean(ds_col[-index])
}
# calculate p-value
pvalue <- (sum(diffs >= obs)+1)/(N+1); pvalue
