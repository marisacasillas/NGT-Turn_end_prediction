library(tidyverse)
library(patchwork)

source("0-NGTButtonPress-helper.R")
i.have.access.to.nonanon.subject.info <- "N"

# Create visual overview of participant pool
if (i.have.access.to.nonanon.subject.info == "Y") {
  all.sub.info <- read_csv(paste0(info.path, "SubjectInfo-non-anon.csv"))

  matched.characteristics.summary <- all.sub.info %>%
    mutate(
      Age.bin = case_when(
        Age.at.test < 15 ~ "< 15",
        Age.at.test >= 15 & Age.at.test < 35 ~ "15-34",
        Age.at.test >= 35 & Age.at.test < 55 ~ "35-54",
        Age.at.test >= 55 & Age.at.test < 75 ~ "55-74",
        Age.at.test >= 75 ~ "75+"),
      Gender = ifelse(Gender == 1, "Woman", "Man"),
      Signer = ifelse(Group == 0, "Non-signer", "Signer"),
      Group = case_when(
        Group == 1 ~ "EL",
        Group == -1 ~ "LL",
        Group == 0 ~ "NS")
      ) %>%
    group_by(Age.bin, Education.code, Gender, Signer, Group) %>%
    summarize(n_ptcps = n(), .groups = "keep")
  matched.characteristics.summary$Signer <- factor(
    matched.characteristics.summary$Signer, levels = c(
      "Signer", "Non-signer"))
    
  # Signer vs. non-signer groups
  signer.nonsigner.matching <- ggplot(
    matched.characteristics.summary) +
    geom_raster(aes(x = Age.bin, y = Education.code,
      alpha = n_ptcps, fill = Gender)) +
    facet_wrap(. ~ Signer * Gender, ncol = 2) +
    ggtitle("A) Signing vs. non-signing participants\n") +
    ylab("Highest education level") +
    xlab("Age group (years)\n") +
    guides(alpha = guide_legend(title = "# People")) +
    scale_fill_manual(values = c("darkorange", "firebrick1")) +
    theme(plot.title = element_text(size = 40, face = "bold"),
      legend.position = "bottom") +
    plot.style
  
  # EL vs. LL groups
  
  # A: Basic demographic compared vars (Age, Gender, Education)
  EL.LL.matching <- ggplot(
    subset(matched.characteristics.summary, Signer == "Signer")) +
    geom_raster(aes(x = Age.bin, y = Education.code,
      alpha = n_ptcps, fill = Gender)) +
    facet_wrap(. ~ Group * Gender, ncol = 4) +
    ggtitle("\n\nB) Early vs. late NGT learners\n") +
    ylab("Highest education level") +
    xlab("Age group (years)") +
    scale_fill_manual(values = c("darkorange", "firebrick1")) +
    theme(legend.position = "none",
      plot.title = element_text(size = 40, face = "bold")) +
    plot.style
  
  # B: Auditory status and input type across EL and LL groups
  all.sub.info$Status <- ifelse(all.sub.info$Deaf == 1, "Deaf", "Hearing")
  all.sub.info$Status <- factor(all.sub.info$Status, levels = c("Hearing", "Deaf"))
  all.sub.info$SignerGroup <- ifelse(all.sub.info$Group == 1, "EL", "LL")
  all.sub.info$SignerGroup <- factor(all.sub.info$SignerGroup, levels = c("EL", "LL"))
  all.sub.info$Input.Code <- factor(all.sub.info$Input.Code, labels = c(
    "At home", "Home + prim. school", "Home + other", "Primary school",
    "Prim. school + other", "Secondary school", "Other"))
  all.sub.info$Input.Code <- factor(all.sub.info$Input.Code, levels = c(
    "Secondary school", "Other", "Prim. school + other",
    "Primary school", "Home + other", "Home + prim. school", "At home"))
  EL.LL.hearing.and.input <- ggplot(
    subset(all.sub.info, Group != 0)) +
    geom_bar(aes(y = Status, fill = Input.Code)) +
    facet_grid(. ~ SignerGroup) +
    ggtitle("") +
    ylab("Auditory status\n") +
    xlab("# People") +
    scale_fill_manual(values = rev(c("darkslateblue", "slateblue", "slateblue1",
      "seagreen3", "palegreen", "olivedrab1", "navy"))) +
    guides(fill = guide_legend(title = "NGT input type", reverse = TRUE)) +
    plot.style
  
  # C: Age of sign onset across EL and LL groups
  age.sign.onset <- subset(all.sub.info, Group != 0) %>%
    group_by(SignerGroup) %>%
    summarize(
      mean = mean(Age.of.sign.onset),
      median = median(Age.of.sign.onset))
  
  EL.LL.NGTonset <- ggplot(
    subset(all.sub.info, Group != 0)) +
    geom_density(aes(x = Age.of.sign.onset, fill = SignerGroup), alpha = 0.3) +
    scale_fill_manual(values = c("gray10", "gray80")) +
    guides(fill = guide_legend(title = "")) +
    geom_vline(data = age.sign.onset,
      aes(xintercept = mean), lty = "solid", color = "red", lwd = 2) +
    geom_vline(data = age.sign.onset,
      aes(xintercept = median), lty = "dashed", color = "red", lwd = 2) +
    annotate("text", label = paste0("Mean = ", round(age.sign.onset$mean[1],1)),
      x = age.sign.onset$mean[1] + 6, y = 0.2, size = 10, color = "red") +
    annotate("text", label = paste0("Mean = ", round(age.sign.onset$mean[2],1)),
      x = age.sign.onset$mean[2] + 6, y = 0.2, size = 10, color = "red") +
    ylab("\nDensity\n") +
    xlab("Age of NGT onset") +
    theme(legend.position = c(0.9,0.9),
      legend.title = element_blank(),
      legend.background = element_blank()) +
    plot.style
  
  layout <- c(
    area(t = 1, l = 1, b = 3, r = 5),
    area(t = 4, l = 1, b = 5, r = 5),
    area(t = 6, l = 1, b = 8, r = 3),
    area(t = 6, l = 4, b = 8, r = 5)
  )
  
  signer.nonsigner.matching +
    EL.LL.matching +
    EL.LL.hearing.and.input +
    EL.LL.NGTonset +
    plot_layout(design = layout)

  ggsave("test.png", dpi = 300, units = "cm", width = 90, height = 80)
  
}
