library(tidyverse)
library(patchwork)
library(ggpattern)

source("0-NGTButtonPress-helper.R")
i.have.access.to.nonanon.subject.info <- "N"
# File stored at: https://uchicago.box.com/s/u44dg82vw9c1k5eg6eiglh3y4kt1bhfp
# (access requires an invitation on Box)

# Create visual overview of participant pool ----
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
      alpha = n_ptcps), fill = "royalblue") +
    facet_wrap(. ~ Signer * Gender, ncol = 2) +
    ggtitle("A) Signing vs. non-signing participants\n") +
    ylab("Highest education level") +
    xlab("Age group (years)\n") +
    guides(alpha = guide_legend(title = "# People")) +
    theme(
      plot.title = element_text(size = 40, face = "bold"),
      legend.position = "bottom",
      strip.background = element_blank()) +
    plot.style
  
  # EL vs. LL groups
  
  # A: Basic demographic compared vars (Age, Gender, Education)
  EL.LL.matching <- ggplot(
    subset(matched.characteristics.summary, Signer == "Signer")) +
    geom_raster(aes(x = Age.bin, y = Education.code,
      alpha = n_ptcps), fill = "royalblue") +
    facet_wrap(. ~ Group * Gender, ncol = 4) +
    ggtitle("\n\nB) Early vs. late NGT learners\n") +
    ylab("Highest education level") +
    xlab("Age group (years)") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 40, face = "bold"),
      strip.background = element_blank()) +
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
  # Previous (color dependent) version
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
    plot.style +
    theme(strip.background = element_blank())

  # New patterned version
  input.summary.barplot <- subset(all.sub.info, Group != 0) %>%
    group_by(Status, Input.Code, SignerGroup) %>%
    summarize(n = n())
  EL.LL.hearing.and.input.patterns <- ggplot(input.summary.barplot, aes(x = n, y = Status,
                                         pattern_fill = Input.Code,
                                         pattern_colour = Input.Code,
                                         fill = Input.Code)) +
    geom_col_pattern(
      aes(pattern = Input.Code, pattern_angle = Input.Code),
      colour          = 'black',
      pattern_density = 0.5,
      pattern_spacing = 0.05
    ) +
    scale_fill_manual(values = c('gray80', 'yellow', 'palegreen', 'blue', 'navy', 'red', 'palegreen')) +
    scale_pattern_fill_manual(values = c('blue', 'red', 'yellow', 'darkgreen', 'palegreen', 'navy', 'gray80')) +
    facet_grid(. ~ SignerGroup) +
    ggtitle("") +
    ylab("Auditory status\n") +
    xlab("# People") +
    guides(fill = guide_legend(title = "NGT input type", reverse = TRUE),
           pattern = guide_legend(title = "NGT input type", reverse = TRUE),
           pattern_fill = guide_legend(title = "NGT input type", reverse = TRUE),
           pattern_colour = guide_legend(title = "NGT input type", reverse = TRUE),
           pattern_angle = guide_legend(title = "NGT input type", reverse = TRUE)) +
    plot.style +
    theme(
      strip.background = element_blank(),
      legend.key.size = unit(2, 'cm'))
  ggexport(EL.LL.hearing.and.input.patterns,
           filename = "input.patterns.png",
           width = 7500, height = 4000, res = 300)

  
  # C: Age of sign onset across EL and LL groups
  age.sign.onset <- subset(all.sub.info, Group != 0) %>%
    group_by(SignerGroup) %>%
    summarize(
      mean = mean(Age.of.sign.onset),
      median = median(Age.of.sign.onset))
  
  EL.LL.NGTonset <- ggplot(
    subset(all.sub.info, Group != 0)) +
    geom_density(aes(x = Age.of.sign.onset, fill = SignerGroup), alpha = 0.3) +
    scale_fill_manual(values = c("skyblue1", "blue4")) +
    guides(fill = guide_legend(title = "")) +
    geom_vline(data = age.sign.onset,
      aes(xintercept = mean), lty = "solid", color = "black", lwd = 2) +
    geom_vline(data = age.sign.onset,
      aes(xintercept = median), lty = "dashed", color = "black", lwd = 2) +
    annotate("text", label = paste0("Mean = ", round(age.sign.onset$mean[1],1)),
      x = age.sign.onset$mean[1] + 6, y = 0.2, size = 10, color = "black") +
    annotate("text", label = paste0("Mean = ", round(age.sign.onset$mean[2],1)),
      x = age.sign.onset$mean[2] + 6, y = 0.2, size = 10, color = "black") +
    ylab("\nDensity\n") +
    xlab("Age of NGT onset") +
    theme(legend.position = c(0.9,0.9),
      legend.title = element_blank(),
      legend.background = element_blank()) +
    plot.style
  
  layout.SM.plot1 <- c(
    area(t = 1, l = 1, b = 3, r = 5),
    area(t = 4, l = 1, b = 5, r = 5),
    area(t = 6, l = 1, b = 8, r = 3),
    area(t = 6, l = 4, b = 8, r = 5)
  )
  
  signer.nonsigner.matching +
    EL.LL.matching +
    EL.LL.hearing.and.input.patterns +
    EL.LL.NGTonset +
    plot_layout(design = layout.SM.plot1)

  ggsave("Demographic-overview.png", dpi = 300,
         units = "cm", width = 90, height = 80)
  ggsave("Demographic-overview.pdf", device = "pdf",
         units = "cm", width = 90, height = 80)
  
}

# Create visual overview of stimuli ----
item.basic.info <- read_csv("supp_info/items.codes.all.csv")[2:9]
item.features <- read_csv("supp_info/items.codes.all.csv")[74:124]
bound.item.info <- bind_cols(item.basic.info, item.features)
plottable.items <- bound.item.info %>%
  mutate(
    UniqueItem = paste(Code, DyadCode, sep = "_"),
    UtteranceType = case_when(
      QuestionType == "polar" ~ 1,
      QuestionType == "alternative" ~ 2,
      QuestionType == "wh" ~ 3,
      TRUE ~ 0),
    DurationSec = Duration/1000) %>%
  select(-Code, -DyadCode, -Signer, -Duration, -Question, -QuestionType, -SpeakerOnLeft) %>%
  pivot_longer(!UniqueItem, names_to = "Feature", values_to = "Value") %>%
  rowwise() %>%
  mutate(
    Code = unlist(strsplit(UniqueItem, "_"))[1],
    DyadCode = unlist(strsplit(UniqueItem, "_"))[2]
  ) %>%
  left_join(select(item.basic.info, c("Code", "Signer", "DyadCode", "MultiUnit", "Duration")),
    by = c("Code", "DyadCode")) %>%
  left_join(select(bound.item.info, c("Code", "DyadCode", "Gramm.Q.Assoc.s", "MGest.Q.Assoc.s")),
    by = c("Code", "DyadCode")) %>%
  rename("Dyad" = DyadCode) %>%
  filter(!grepl("^[PR]", Code)) %>%
  select(Dyad, Signer, Code, MultiUnit, Feature, Value,
    Gramm.Q.Assoc.s, MGest.Q.Assoc.s, Duration) %>%
  mutate(
    MultiUnit = ifelse(MultiUnit == 1, "Multi-unit", "Single-unit"),
    TidyVidName = str_extract(Code, "\\d+"),
    Clip = as.factor(paste(Dyad, TidyVidName))) %>%
  arrange(Dyad, Gramm.Q.Assoc.s, MGest.Q.Assoc.s, -Duration)

## Set orders of x and y labels
plottable.items$Feature <- factor(plottable.items$Feature, levels = c(
  # Conventionalized, NGT-only question signs
  "QS.ann", "HOE.ann", "HOE..LANG.ann", "WAAROM.ann", "WAT.ann", "HOEVEEL.ann",
  # Gesture-associated question signs
  "PO.ann", "JIJ.ann", "JULLIE..TWEE.ann", "PO..JIJ.ann",
  # Brow frowning and raising
  "AU1..2.ann", "AU1..2..4.ann", "AU4.ann", "AU4..9.ann", "AU5.ann", "AU4..5.ann",
  # Blinks and tags
  "blink.ann", "tag1.ann", "tag2.ann","tag3.ann",  "tag4.ann", 
  # Head tilts
  "HPb.ann", "HT.ann", "HPf.ann",
  # Other head movements
  "HS.ann", "Wiggle.ann",
  # Backchannels
  "NM..BC.ann", "NM.ann", "nod.ann",
  # Manual prosodic cues
  "lenghtening.ann", "fold..hands.ann", "self..groom.ann", "hold.ann",
  # Other
  "repetition.ann", "JA.ann",
  # Categories...
  "MultiUnit", "DurationSec", "UtteranceType",
  # Relevant for last analysis
  "Gramm.Q.Assoc.s", "MGest.Q.Assoc.s", "Gest.Q.Assoc.s", 
  # Other
  "Brows_summary_raises", "Brows_summary_frowns", "Brows_summary_raiseandfrown",
  "Head_summary_tilts", "Head_summary_blinks", "Head_summary_mvmts",
  "Backchannel", "Nonmanual_prosodic_cues", "Manual_prosodic_cues",
  "Lexical_cues", "Question_words", "NonWHQuestion_words", "Tag_cues"
))

plottable.items$Feature <- factor(plottable.items$Feature, labels = c(
  # Conventionalized, NGT-only question signs
  "QS", "HOE", "HOE-LANG", "WAAROM", "WAT", "HOEVEEL",
  # Gesture-associated question signs
  "PO", "JIJ", "JULLIE-TWEE", "PO-JIJ.ann",
  # Brow frowning and raising
  "AU1+2", "AU1+2+4", "AU4", "AU4+9", "AU5", "AU4+5",
  # Blinks and tags
  "blink", "tag1", "tag2","tag3",  "tag4", 
  # Head tilts
  "HPb", "HT", "HPf",
  # Other head movements
  "HS", "Wiggle",
  # Backchannels
  "NM-BC", "NM", "nod",
  # Manual prosodic cues
  "lengthening", "fold-hands", "self-groom", "hold",
  # Other
  "repetition", "JA",
  # Categories...
  "MultiUnit", "DurationSec", "UtteranceType",
  # Relevant for last analysis
  "NGT-only Q marker", "NGT & Dutch manual Q marker", "NGT & Dutch Q marker", 
  # Other
  "Brow raise", "Brown lowering", "Brow movement",
  "Head tilt", "Blink", "Head/face movement",
  "Backchannel", "Nonmanual prosodic cue", "Manual prosodic cue",
  "Lexical cues", "Question sign", "Non-WH question sign", "Tag"
))

plottable.items$Clip <- factor(plottable.items$Clip, levels = rev(unique(plottable.items$Clip)))

category.list <- c("MultiUnit", "DurationSec", "UtteranceType",
  "NGT-only Q marker", "NGT & Dutch manual Q marker",
  "Brow raise", "Brown lowering",
  "Head tilt", "Blink",
  "Backchannel", "Nonmanual prosodic cue", "Manual prosodic cue",
  "Tag")

categories.plottable.items <- filter(
  plottable.items, Feature %in% category.list)

categories.plottable.items <- filter(categories.plottable.items, Feature != "MultiUnit") %>%
  mutate(
    Value.text = case_when(
      Feature == "DurationSec" ~ as.character(Value),
      Feature == "UtteranceType" & Value == 1 ~ "polar",
      Feature == "UtteranceType" & Value == 2 ~ "alt.",
      Feature == "UtteranceType" & Value == 3 ~ "wh",
      Feature == "UtteranceType" & Value == 0 ~ "",
      TRUE ~ ""
    ))
categories.plottable.items$MultiUnit <- factor(categories.plottable.items$MultiUnit,
  levels = c("Single-unit", "Multi-unit"))

# Single unit
item.overview.STCU <- ggplot(
  subset(categories.plottable.items, MultiUnit == "Single-unit"),
  aes(x = Feature, y = Clip, alpha = Value, label = Value.text)) +
  geom_tile(fill = "blue") +
  geom_text(alpha = 1, size = 10) +
  scale_alpha(range = c(0, 1)) +
  ggtitle("Single-unit") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(size = 40, face = "bold")
  ) +
  plot.style

# Multi unit
item.overview.MTCU <- ggplot(
  subset(categories.plottable.items, MultiUnit == "Multi-unit"),
  aes(x = Feature, y = Clip, alpha = Value, label = Value.text)) +
  geom_tile(fill = "blue") +
  geom_text(alpha = 1, size = 10) +
  scale_alpha(range = c(0, 1)) +
  ggtitle("Multi-unit") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(size = 40, face = "bold")
  ) +
  plot.style

layout.SM.plot2 <- c(
  area(t = 1, l = 1, b = 6, r = 2),
  area(t = 1, l = 3, b = 2, r = 4)
)

item.overview.STCU + item.overview.MTCU + plot_layout(design = layout.SM.plot2)

ggsave("Item-overview.png", dpi = 300,
       units = "cm", width = 100, height = 120)
ggsave("Item-overview.pdf", device = "pdf",
       units = "cm", width = 100, height = 120)


# Create visual overview of exclusions ----
# exclusion 1 is exclusion on the basis of a priori issues noted -- no need to visualize
# exclusion 2 is Response > 3 (i.e., >5%; 3/60 trials)
# NB: 0.5 added to vlines below so that the line appears on the
# right edge of the relevant bar
excl2.data <- read_csv("exclusion2.allptcps.csv")
excl2.fig <- ggplot(excl2.data) +
  geom_histogram(aes(x = Response), binwidth = 1) +
  geom_vline(xintercept = 3.5, color = "red", lwd = 2) +
  annotate(geom = "text", label = "5%", x = 5, y = 25, color = "red", size = 15) +
  geom_vline(xintercept = 6.5, color = "red", lty = "dashed", lwd = 2) +
  annotate(geom = "text", label = "10%", x = 8, y = 15, color = "red", size = 15) +
  geom_vline(xintercept = 9.5, color = "red", lty = "dotted", lwd = 2) +
  annotate(geom = "text", label = "15%", x = 11, y = 5, color = "red", size = 15) +
  ggtitle("Button presses during context videos") +
  ylim(0,30) +
  xlim(0,40) +
  ylab("# Participants") +
  xlab("# Trials with responses during context videos") +
  plot.style +
  theme(plot.title = element_text(size = 40, face = "bold"))

# exclusion 3 is VeryLate > 6 (i.e., >10%; 6/60 trials)
# NB: 0.5 added to vlines below so that the line appears on the
# right edge of the relevant bar
excl3.data <- read_csv("exclusion3.allptcps.csv")
excl3.fig <- ggplot(excl3.data) +
  geom_histogram(aes(x = VeryLate), binwidth = 1) +
  geom_vline(xintercept = 6.5, color = "red", lwd = 2) +
  annotate(geom = "text", label = "10%", x = 7.5, y = 25, color = "red", size = 15) +
  geom_vline(xintercept = 9.5, color = "red", lty = "dashed", lwd = 2) +
  annotate(geom = "text", label = "15%", x = 10.5, y = 15, color = "red", size = 15) +
  geom_vline(xintercept = 12.5, color = "red", lty = "dotted", lwd = 2) +
  annotate(geom = "text", label = "20%", x = 13.5, y = 5, color = "red", size = 15) +
  ggtitle("Late button presses") +
  ylim(0,30) +
  xlim(0,20) +
  ylab("# Participants") +
  xlab("# Trials with late responses") +
  plot.style +
  theme(plot.title = element_text(size = 40, face = "bold"))

excl2.fig + excl3.fig

ggsave("Exclusions-overview.png", dpi = 300,
       units = "cm", width = 90, height = 45)
ggsave("Exclusions-overview.pdf", device = "pdf",
       units = "cm", width = 90, height = 45)

# grayscale versions made in ColorSync Utility > Match to Profile > Display > Generic Gray Profile
