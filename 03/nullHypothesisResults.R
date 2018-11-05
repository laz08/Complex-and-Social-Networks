

languages = c("Arabic", "Basque", "Catalan", "Chinese", "Czech", "English", "Greek", "Hungarian", "Italian", "Turkish")
network_c <- c(0.3246, 0.2704, 0.3426, 0.3246, 0.3039, 0.3432, 0.3198, 0.2866, 0.3263, 0.3425)
switching_p <- c(0.4, 0.20 ,0.05 ,0.35 ,0.60 , 0.20, 0.45, 0.60, 0.35, 0.3)
binomial_results <- c(0,0,0,0,0,0,0,0,0,0)

closeness_metrics = data.frame(lang=languages, net=network_c, p_binomial = binomial_results, p_switch = switching_p)


M_results <- data.frame(lang="Basque", def = "1.941", ra = "1.955", inc = "1.822", dec = "1.90")
