library(tidyr)
library(purrr)
library(ggplot2)

# Figures
## Root mean square error 
lapply(X = names, FUN = function(x) stat.mar.5[[x]] %>% t %>% 
          .[,16:18] %>% as.data.frame %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% na.omit() %>%
   mutate(method = factor(method, levels = unique(method))) %>%
   mutate(ind = factor(ind, level = c("rmse.x1", "rmse.x2", "rmse.y"),
                       labels = c(expression(italic(X)["1"]), 
                                  expression(italic(X)["2"]),
                                  expression(italic(Y))))) %>%
   
   ggplot(., aes(x = method, y = values)) + geom_boxplot() + 
   labs(x = "imputation method", y = "root mean square error") + 
   facet_wrap(~ind, scales = "free_x", labeller = label_parsed) + 
   coord_flip() + scale_x_discrete(labels= c(
      expression("pmm" ~ textstyle((italic(m)~"= 5"))), 
      expression("pmm" ~ textstyle((italic(m)~"= 1"))), 
      expression("bayesian" ~ textstyle((italic(m)~"= 5"))),
      expression("bayesian" ~ textstyle((italic(m)~"= 1"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 5"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 1"))), 
      expression("regression"),
      expression("mean")), limits=rev) + theme_bw() +
   theme(plot.margin=unit(c(0.1,1,0.1,1),"cm"))

ggsave("Figures/MAR/Rmse.pdf", dpi = 300, width = 20, height = 8, units = "cm")

## Bias means, variances, and correlation
ref <- stat.true %>% t %>% as.data.frame %>% .[,c(1:6,13:15)]

data <- lapply(X = names, FUN = function(x) stat.mar.5[[x]] %>% t %>% 
                  .[,c(1:6,13:15)] %>% as.data.frame %>% - ref %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>%
   mutate(ind = factor(ind, level = c("mean.x1", "mean.x2", "mean.y", 
                                      "var.x1", "var.x2", "var.y", 
                                      "cor.x1.x2", "cor.x1.x3", "cor.x2.x3"),
                       labels = c(expression(mu["x1"]), 
                                  expression(mu["x2"]),
                                  expression(mu["y"]),
                                  expression(sigma["x1"]^2),
                                  expression(sigma["x2"]^2),
                                  expression(sigma["y"]^2),
                                  expression(rho["x1,x2"]),
                                  expression(rho["x1,x3"]),
                                  expression(rho["x2,x3"]))))

data.line <- data.frame(X = factor(
   c("mean.x1", "mean.x2", "mean.y", "var.x1", "var.x2", "var.y", 
     "cor.x1.x2", "cor.x1.x3", "cor.x2.x3"), 
   level = c("mean.x1", "mean.x2", "mean.y", "var.x1", "var.x2", "var.y", 
             "cor.x1.x2", "cor.x1.x3", "cor.x2.x3"), 
   labels = c(expression(mu["x1"]), 
              expression(mu["x2"]),
              expression(mu["y"]),
              expression(sigma["x1"]^2),
              expression(sigma["x2"]^2),
              expression(sigma["y"]^2),
              expression(rho["x1,x2"]),
              expression(rho["x1,x3"]),
              expression(rho["x2,x3"]))), Z = rep(0,9))

ggplot(data = data, aes(x = method, y = values)) + 
   geom_hline(data = data.line, 
              aes(yintercept = Z), color="grey", linetype="dashed") +
   labs(x = "imputation method", y = "bias") + 
   geom_boxplot() +
   facet_wrap(~ind, scales = "free_x", labeller = label_parsed) + 
   coord_flip() + theme_bw() + 
   scale_x_discrete(labels= c(
      expression("pmm" ~ textstyle((italic(m)~"= 5"))), 
      expression("pmm" ~ textstyle((italic(m)~"= 1"))), 
      expression("bayesian" ~ textstyle((italic(m)~"= 5"))),
      expression("bayesian" ~ textstyle((italic(m)~"= 1"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 5"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 1"))), 
      expression("regression"),
      expression("mean")), limits=rev) +
   theme(plot.margin=unit(c(0.1,1,0.1,1),"cm"))

ggsave("Figures/MAR/Bias Descriptive Statistics.pdf", dpi = 300, width = 20, height = 22.5, units = "cm")

## Bias Beta's Sigma & R-squared 
ref <- analysis.true %>% t %>% as.data.frame() %>% .[,-c(3:4, 7:8)]

data <- lapply(X = names, FUN = function(x) analysis.mar.5[[x]] %>% t %>% 
                  .[,-c(3:4, 7:8)] %>% as.data.frame %>% - ref %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>%
   mutate(ind = factor(ind, level = c("beta.x1","beta.x2","sigma","r.squared"),
                       labels = c(expression(beta["1"]), expression(beta["2"]), 
                                  expression(sigma["e"]^2), expression(R^2))))

data.line <- data.frame(X = factor(
   c("beta.x1","beta.x2","sigma","r.squared"), 
   level = c("beta.x1","beta.x2","sigma","r.squared"), 
   labels = c(expression(beta["1"]), expression(beta["2"]), 
              expression(sigma["e"]^2), expression(R^2))), Z = rep(0,4))

ggplot(data = data, aes(x = method, y = values)) + 
   geom_hline(data = data.line, 
              aes(yintercept = Z), color="grey", linetype="dashed") + 
   geom_boxplot() + 
   labs(x = "imputation method", y =  "bias") + 
   facet_wrap(~ind, scales = "free_x", labeller = label_parsed, nrow = 2) + 
   coord_flip() + theme_bw() +
   scale_x_discrete(labels= c(
      expression("pmm" ~ textstyle((italic(m)~"= 5"))), 
      expression("pmm" ~ textstyle((italic(m)~"= 1"))), 
      expression("bayesian" ~ textstyle((italic(m)~"= 5"))),
      expression("bayesian" ~ textstyle((italic(m)~"= 1"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 5"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 1"))), 
      expression("regression"),
      expression("mean")), limits=rev) +
   theme(plot.margin=unit(c(0.1,1,0.1,1),"cm"))

ggsave("Figures/MAR/Bias Analysis.pdf", dpi = 300, width = 20, height = 15, units = "cm")

## Coverage Rate Betas
data.3 <- lapply(X = names, FUN = function(x) analysis.mar.3[[x]] %>% t %>% 
                    .[,3:4] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".3")
data.5 <- lapply(X = names, FUN = function(x) analysis.mar.5[[x]] %>% t %>% 
                    .[,3:4] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".5")
data.7 <- lapply(X = names, FUN = function(x) analysis.mar.7[[x]] %>% t %>% 
                    .[,3:4] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".7")

data.line <- data.frame(X = factor(c("coverage.beta.x1","coverage.beta.x2"), 
                                   level = c("coverage.beta.x1", "coverage.beta.x2"),
                                   labels = c(expression(beta["1"]), expression(beta["2"]))), Z = c(0.95, 0.95))

rbind(data.3, data.5, data.7) %>% 
   mutate(ind = factor(ind, level = c("coverage.beta.x1", "coverage.beta.x2"),
                       labels = c(expression(beta["1"]), expression(beta["2"])))) %>%
   
   ggplot(., aes(x = method, y = values)) + 
   geom_hline(data = data.line, 
              aes(yintercept = Z), color="grey", linetype="dashed") +
   geom_point(aes(shape = factor(type))) + 
   labs(x = "imputation method", y = "coverage rate") + 
   facet_wrap(~ind, scales = "free_x", ncol = 2, labeller = label_parsed) + 
   coord_flip() + scale_x_discrete(labels= c(
      expression("pmm" ~ textstyle((italic(m)~"= 5"))), 
      expression("pmm" ~ textstyle((italic(m)~"= 1"))), 
      expression("bayes" ~ textstyle((italic(m)~"= 5"))),
      expression("bayes" ~ textstyle((italic(m)~"= 1"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 5"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 1"))), 
      expression("regression"),
      expression("mean")), limits=rev) + theme_bw() + 
   scale_shape_discrete(name = expression("Missingness"), 
                        labels = c("30 %", "50 %", "70 %")) +
   theme(plot.margin=unit(c(0.1,1,0.1,1),"cm")) +
   theme(legend.position="top", legend.title = element_text(size=10, face="bold"))

ggsave("Figures/MAR/Coverage Rate Betas.pdf", dpi = 300, width = 20, height = 9.5, units = "cm")

## Coverage Rate Means
data.3 <- lapply(X = names, FUN = function(x) stat.mar.3[[x]] %>% t %>% 
                    .[,7:9] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".3")

data.5 <- lapply(X = names, FUN = function(x) stat.mar.5[[x]] %>% t %>% 
                    .[,7:9] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".5")

data.7 <- lapply(X = names, FUN = function(x) stat.mar.7[[x]] %>% t %>% 
                    .[,7:9] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
   do.call("rbind", .) %>% 
   mutate(method = factor(method, levels = unique(method))) %>% mutate(type = ".7")

data.line <- data.frame(X = factor(c("coverage.beta.x1","coverage.beta.x2"), 
                                   level = c("coverage.beta.x1", "coverage.beta.x2"),
                                   labels = c(expression(beta["1"]), expression(beta["2"]))), Z = c(0.95, 0.95))

rbind(data.3, data.5, data.7) %>% 
   mutate(ind = factor(ind, level = c("sem.coverage.x1", "sem.coverage.x2", "sem.coverage.y"),
                       labels = c(expression(mu["x1"]), expression(mu["x2"]), expression(mu["y"])))) %>%
   
   ggplot(., aes(x = method, y = values)) + 
   geom_hline(data = data.line, 
              aes(yintercept = Z), color="grey", linetype="dashed") +
   geom_point(aes(shape = factor(type))) + 
   labs(x = "imputation method", y = "coverage rate") + 
   facet_wrap(~ind, scales = "free_x", ncol = 3, labeller = label_parsed) + 
   coord_flip() + scale_x_discrete(labels= c(
      expression("pmm" ~ textstyle((italic(m)~"= 5"))), 
      expression("pmm" ~ textstyle((italic(m)~"= 1"))), 
      expression("bayes" ~ textstyle((italic(m)~"= 5"))),
      expression("bayes" ~ textstyle((italic(m)~"= 1"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 5"))),
      expression("stochastic" ~ textstyle((italic(m)~"= 1"))), 
      expression("regression"),
      expression("mean")), limits=rev) + theme_bw() + 
   scale_shape_discrete(name = expression("Missingness"), 
                        labels = c("30 %", "50 %", "70 %")) +
   theme(plot.margin=unit(c(0.1,1,0.1,1),"cm")) + 
   theme(legend.position="top", legend.title = element_text(size=10, face="bold"))

ggsave("Figures/MAR/Coverage Rate Means.pdf", dpi = 300, width = 20, height = 9.5, units = "cm")

# Tables
create_bias_table <- function(data, ref, r, p) {
   lapply(X = names, FUN = function(x) data[[x]] %>% t %>% 
             .[,r] %>% as.data.frame %>% -ref %>% stack() %>% mutate(method = x)) %>%
      do.call("rbind", .) %>% na.omit() %>%
      aggregate(.~method + ind, ., function(x) c(round(mean(x),2), round(sd(x),2)), 
                simplify = F, drop = F) %>%
      mutate(values = gsub("[(c,]", "", values)) %>% 
      mutate(values = gsub("[ ]", " (", values)) %>% 
      pivot_wider(names_from = method, values_from = values) %>% 
      mutate(missing = p)}

create_coverage_table <- function(data, r, p) {
   lapply(X = names, FUN = function(x) data[[x]] %>% t %>% 
             .[,r] %>% as.data.frame %>% colMeans %>% stack() %>% mutate(method = x)) %>% 
      do.call("rbind", .) %>%
      pivot_wider(names_from = method, values_from = values) %>%
      mutate(missing = p)}

create_confidence_width_table <- function(data, r, p) {
   lapply(X = names, FUN = function(x) data[[x]] %>% t %>% 
             .[,r] %>% as.data.frame %>% stack() %>% mutate(method = x)) %>%
      do.call("rbind", .) %>% na.omit() %>%
      aggregate(.~method + ind, ., function(x) round(mean(x),3), simplify = F, drop = F) %>% 
      mutate(values = as.character(values)) %>% 
      pivot_wider(names_from = method, values_from = values) %>% 
      mutate(missing = p)}

# RMSE
rbind(create_bias_table(stat.mar.3, 0, 16:18, 0.3),
      create_bias_table(stat.mar.5, 0, 16:18, 0.5),
      create_bias_table(stat.mar.7, 0, 16:18, 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Rmse.csv')

# Bias Mu, var, cor
ref <- stat.true %>% t %>% as.data.frame %>% .[,c(1:6,13:15)]

rbind(create_bias_table(stat.mar.3, ref, c(1:6,13:15), 0.3),
      create_bias_table(stat.mar.5, ref, c(1:6,13:15), 0.5),
      create_bias_table(stat.mar.7, ref, c(1:6,13:15), 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Bias Descriptive Statistics.csv')

# Bias Regression Analysis
ref <- analysis.true %>% t %>% as.data.frame() %>% .[,-c(3:4, 7:8)]

rbind(create_bias_table(analysis.mar.3, ref, -c(3:4, 7:8), 0.3),
      create_bias_table(analysis.mar.5, ref, -c(3:4, 7:8), 0.5),
      create_bias_table(analysis.mar.7, ref, -c(3:4, 7:8), 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Bias Analysis.csv')

# Coverage Betas
rbind(create_coverage_table(analysis.mar.3, 3:4, 0.3),
      create_coverage_table(analysis.mar.5, 3:4, 0.5),
      create_coverage_table(analysis.mar.7, 3:4, 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Coverage Rate Betas.csv')

# Coverage Mu 
rbind(create_coverage_table(stat.mar.3, 7:9, 0.3),
      create_coverage_table(stat.mar.5, 7:9, 0.5),
      create_coverage_table(stat.mar.7, 7:9, 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Coverage Rate Means.csv')

# Average Confidence Interval Width Betas
rbind(create_confidence_width_table(analysis.mar.3, 7:8, 0.3),
      create_confidence_width_table(analysis.mar.5, 7:8, 0.5),
      create_confidence_width_table(analysis.mar.7, 7:8, 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Average Confidence Interval Width Betas.csv')

# Average Confidence Interval Width Mu
rbind(create_confidence_width_table(stat.mar.3, 19:21, 0.3),
      create_confidence_width_table(stat.mar.5, 19:21, 0.5),
      create_confidence_width_table(stat.mar.7, 19:21, 0.7)) %>%
   .[,c("ind", "missing", names)] %>%
   write.csv(.,'Tables/MAR/Average Confidence Interval Width Mu.csv')