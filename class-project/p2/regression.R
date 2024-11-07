## MVA 1 --> Multivariate linear regression
library(tidyverse)
library(stargazer)
library(ggpubr)
library(car)


## ---------------------------------- Load and Preprocess Data ---------------------------------- ##
trees <- readRDS('data/proj/input_pca.rds') %>%
  select(PC1, CCLCD, GENUS, DSTRBCD1) %>%
  mutate(log_PC1=log(PC1+3.47)) %>%  # log transform the response variable
  mutate(CCLCD = factor(CCLCD), GENUS = factor(GENUS), DSTRBCD1 = factor(DSTRBCD1))  # convert to factors


## ------------------------------------- Linear Regression ------------------------------------- ##
mod <- lm(log_PC1~CCLCD+GENUS*DSTRBCD1, data = trees)


## ------------------------------------- Check assumptions ------------------------------------- ##
## Normality of residuals
normality <- ggplot() +
  geom_qq(data = data.frame(residuals = residuals(mod)), aes(sample = residuals), shape=1) +
  geom_qq_line(data = data.frame(residuals = residuals(mod)), aes(sample = residuals), color = "red") + 
  theme_minimal() +
  xlab('Normal Quantiles') + ylab('Residuals') +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10))

## homoscedasticity
homo <- ggplot() +
  geom_point(data = data.frame(fitted = fitted(mod), residuals = residuals(mod)), 
             aes(x = fitted, y = residuals), shape=1) +
  geom_hline(yintercept = 0, color = "red") +  # Add horizontal line at 0
  xlab("Fitted") + 
  ylab("Residuals") +
  theme_minimal() +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10))

## final figure
fig.test <- ggarrange(normality, homo, nrow=2, ncol=1, labels=c('a', 'b'))


## ---------------------------------------- Result ---------------------------------------- ##
table <- stargazer(mod, type = "text")

# get mean and CIs of the coes
coef_values <- coef(mod)
ci_values <- confint(mod)
coef_df <- data.frame(  # Combine results into a data frame
  variable = names(coef_values),
  estimate = coef_values,
  conf.low = ci_values[, 1],
  conf.high = ci_values[, 2]) %>%
  na.omit()


coe <- ggplot() +
  geom_pointrange(data=coef_df, aes(x = variable, y = estimate, ymin = conf.low, ymax = conf.high)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red") +  # Add horizontal line at 0
  labs(title = '', x = 'Variable', y = 'Coefficient') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10))

## final fig
fig <- ggarrange(fig.test, coe, ncol=2, nrow=1, labels=c('', 'c'), widths = c(1, 1.2))
ggsave('class-project/figs/p2-fig2.png', fig, width=10, height=8)
