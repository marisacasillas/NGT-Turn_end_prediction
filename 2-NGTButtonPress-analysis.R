# set working directory
# setwd(choose.dir())

#install packages
# install.packages("party")
# install.packages("REEMtree")

source("0-NGTButtonPress-helper.R")
source("1-NGTButtonPress-prep.R")
library(tidyverse)

# Read in the data
test.targets <- read.csv(paste0(processed.data.path,
                                "prepped.data.for.analysis.csv"))
# Re-order the groups
test.targets$Group <- factor(test.targets$Group,
                             levels=c("EL", "LL", "HD"))
# Make a version of item duration in seconds
test.targets$DurSec <- round(test.targets$Duration/1000, 0)

# Define our target measures
test.targets$Anticipatory <- ifelse(
  test.targets$RTms - test.targets$Duration <= 0, 1, 0)
test.targets$RelRTms <- test.targets$RTms - test.targets$Duration
test.targets$Accuracy <- log(abs(test.targets$RelRTms) + 1)

# Focus in on trials that:
# 1 - have a button press response
# 2 - after seeing at least 720 ms of the stimulus
#     (the time needed for turn-end informative information
#     on the shortest item)
# 3 - limited to turns with duration between 1 and 10 seconds
test.targets.sub <- subset(test.targets,
	Response == 1 & RTms > 720 &
	Duration >= 1000 & Duration <= 10000)
# 94.3% of the original data
# nrow(test.targets.sub)/nrow(test.targets)

# For a more limited analysis subset to trials where:
# 4 - the response was between -2000 and +2000
test.targets.sub.0 <- subset(test.targets.sub,
	RelRTms >= -2000 & RelRTms <= 2000)
# 88.5% of the original data
# nrow(test.targets.sub.0)/nrow(test.targets)

# For a more limited analysis subset to trials where:
# 5 - the turn only had a single TCU
test.targets.sub.STCU <- subset(test.targets.sub,
  MultiUnit == -1)
# 74.6% of the original data
# nrow(test.targets.sub.STCU)/nrow(test.targets)

###### Descriptive data ######
by.ptcp.anticipation.rate <- test.targets.sub %>%
  group_by(Subject, Group) %>%
  summarize(mean.ant = mean(Anticipatory)) %>%
  ungroup()
overall.anticipation.rate <- mean(by.ptcp.anticipation.rate$mean.ant)
by.group.anticipation.rate <- by.ptcp.anticipation.rate %>%
  group_by(Group) %>%
  summarize(group.mean.ant = mean(mean.ant))

###### Statistical analysis ######
#---- Q1: Do participants reliably anticipate turn ends? ----

# Considering all responses (most conservative; reported in manuscript)
sub.meds <- aggregate(RelRTms ~ Subject + Group,
                      test.targets.sub, median)
# ELs
t.test(subset(sub.meds, Group == "EL")$RelRTms)
EL.Q1.all <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets, Group == "EL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(EL.Q1.all)
# LLs
t.test(subset(sub.meds, Group == "LL")$RelRTms)
LL.Q1.all <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets, Group == "LL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(LL.Q1.all)
# HDs
t.test(subset(sub.meds, Group == "HD")$RelRTms)
HD.Q1.all <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets, Group == "HD"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(HD.Q1.all)
# Summary: Strong evidence for difference from 0
# in all three groups


# Considering all responses within 2 seconds of 0
sub.0.meds <- aggregate(RelRTms ~ Subject + Group,
  test.targets.sub.0, median)
# ELs
t.test(subset(sub.0.meds, Group == "EL")$RelRTms)
EL.Q1.close <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.0, Group == "EL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(EL.Q1.close)
# LLs
t.test(subset(sub.0.meds, Group == "LL")$RelRTms)
LL.Q1.close <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.0, Group == "LL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(LL.Q1.close)
# HDs
t.test(subset(sub.0.meds, Group == "HD")$RelRTms)
HD.Q1.close <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.0, Group == "HD"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(HD.Q1.close)
# Summary: Strong evidence for difference from 0
# in all three groups

# Considering all responses to single-TCU turns
sub.meds.stcu <- aggregate(RelRTms ~ Subject + Group,
  test.targets.sub.STCU, median)
# ELs
t.test(subset(sub.meds.stcu, Group == "EL")$RelRTms)
EL.Q1.STCU <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.STCU, Group == "EL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(EL.Q1.STCU)
# LLs
t.test(subset(sub.meds.stcu, Group == "LL")$RelRTms)
LL.Q1.STCU <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.STCU, Group == "LL"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(LL.Q1.STCU)
# HDs
t.test(subset(sub.meds.stcu, Group == "HD")$RelRTms)
HD.Q1.STCU <- glmer(
  Anticipatory ~ (1|Subject) + (1|Item),
  subset(test.targets.sub.STCU, Group == "HD"),
  family = binomial,
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(HD.Q1.STCU)
# Summary: Strong evidence for difference from 0
# in all three groups



#---- Q2: What factors make anticipation more likely? ----
# Predictors to start with:
# Group
# Question

# Nuisance predictors:
# MultiUnit
# Order ("Trial")
# Duration
# Dyad

# Considering all responses
Q2.all <- glmer(
  Anticipatory ~ Group * Question +
    MultiUnit + DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = test.targets.sub,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q2.all)
# Summary: Significant effects of item duration and question status
# Marginal effects of group (EL vs. HD) and group * question status
# (Q effect is marg diff for EL and HD)


# Considering all responses within 2 seconds of 0
Q2.close <- glmer(
  Anticipatory ~ Group * Question +
    MultiUnit + DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = test.targets.sub.0,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q2.close)
# Summary: Significant effects of item duration, question status,
# and group * question status (Q effect is sig diff for EL and HD)
# Marginal effects of group (EL vs. HD) and trial number

# Considering all responses to single-TCU turns (best conceptual control; reported in manuscript)
Q2.STCU <- glmer(
  Anticipatory ~ Group * Question +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = test.targets.sub.STCU,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q2.STCU)
# Summary: Significant effects of item duration, question status,
# and group * question status (Q effect is sig diff for EL and HD)
# Marginal effect of group (EL vs. HD)

# Now the re-leveled version with LL as reference
test.targets.sub.STCU$GroupLL <- factor(
  test.targets.sub.STCU$Group, levels = c("LL", "EL", "HD"))
Q2.STCU.LL <- glmer(
  Anticipatory ~ GroupLL * Question +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = test.targets.sub.STCU,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q2.STCU.LL)

#---- Q3: What factors contribute to earlier anticipation in Qs? ----
# Signs specific to SL associated with speaker transition:
# Gramm.Q.Assoc.s
# Prediction: Signers > Non-signers

# Discrete manual gestures accessible to HDs associated with speaker
# transition: MGest.Q.Assoc.s
# Prediction: Signers >~ Non-signers

# How strong is the association with questions in our data?
Q.features <- test.targets.sub.STCU %>%
  group_by(Item, Question) %>%
  summarize(
    Gramm.Q.Assoc.s = max(Gramm.Q.Assoc.s),
    MGest.Q.Assoc.s = max(MGest.Q.Assoc.s)) %>%
  group_by(Question) %>%
  summarize(
    Gramm.Q.Assoc.s = mean(Gramm.Q.Assoc.s),
    MGest.Q.Assoc.s = mean(MGest.Q.Assoc.s))
Q.NQ.diffs <- Q.features[2,] - Q.features[1,]

# -- Anticipation ----

# Prediction: Signers > Non-signers
L.advantage.data <- filter(test.targets.sub.STCU,
  Gramm.Q.Assoc.s == 1 | MGest.Q.Assoc.s == 0)
Q3.STCU.Lad <- glmer(
  Anticipatory ~ Group +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = L.advantage.data,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q3.STCU.Lad)

# More conservative version and has more trials; reported in manuscript
L.p.advantage.data <- filter(test.targets.sub.STCU,
  Gramm.Q.Assoc.s == 1 | MGest.Q.Assoc.s == 1)
# nrow(L.p.advantage.data)/nrow(test.targets.sub.STCU)
# nrow(L.p.advantage.data)/nrow(test.targets)
Q3.STCU.Lpad <- glmer(
  Anticipatory ~ Group +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = L.p.advantage.data,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q3.STCU.Lpad)
# testing the LL HD contrast
Q3.STCU.Lpad.LL <- glmer(
  Anticipatory ~ GroupLL +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = L.p.advantage.data,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q3.STCU.Lpad.LL)

# Signers ~> Non-signers
no.L.advantage.data.noLad <- filter(test.targets.sub.STCU,
  Gramm.Q.Assoc.s == 0 | MGest.Q.Assoc.s == 1)
# nrow(no.L.advantage.data.noLad)/nrow(test.targets.sub.STCU)
# nrow(no.L.advantage.data.noLad)/nrow(test.targets)
Q3.STCU.noLad <- glmer(
  Anticipatory ~ Group +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = no.L.advantage.data.noLad,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q3.STCU.noLad)
# testing the LL HD contrast
Q3.STCU.noLad.LL <- glmer(
  Anticipatory ~ GroupLL +
    DurSec + Trial + BP + 
    (1|Subject) + (1|Item),
  data = no.L.advantage.data.noLad,
  family = "binomial",
  glmerControl(optimizer="bobyqa",
    optCtrl = list(maxfun = 100000)))
summary(Q3.STCU.noLad.LL)


# -- RT ----

# --- Accuracy ---

# Prediction: Signers > Non-signers
Q3.STCU.Lad.RT <- lmer(
  Accuracy ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = L.advantage.data)
summary(Q3.STCU.Lad.RT)

Q3.STCU.Lpad.RT <- lmer(
  Accuracy ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = L.p.advantage.data)
summary(Q3.STCU.Lpad.RT)

# Signers ~> Non-signers
Q3.STCU.noLad.RT <- lmer(
  Accuracy ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = no.L.advantage.data.noLad)
summary(Q3.STCU.noLad.RT)

# --- RTms relative to stroke turn offset ---

Q3.STCU.Lad.RT2 <- lmer(
  RelRTms ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = L.advantage.data)
summary(Q3.STCU.Lad.RT2)

Q3.STCU.Lpad.RT2 <- lmer(
  RelRTms ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = L.p.advantage.data)
summary(Q3.STCU.Lpad.RT2)

# Signers ~> Non-signers
Q3.STCU.noLad.RT2 <- lmer(
  RelRTms ~ Group +
    DurSec + Trial + BP +
    (1|Subject) + (1|Item),
  data = no.L.advantage.data.noLad)
summary(Q3.STCU.noLad.RT2)


#---- PLOTS ----
by.ptcp.anticipation.rate.q <-  test.targets.sub %>%
  group_by(Subject, Group, Question) %>%
  summarize(mean.ant = mean(Anticipatory)) %>%
  ungroup()
by.ptcp.anticipation.rate.q$Group <- factor(
  by.ptcp.anticipation.rate.q$Group,
  labels = c("Early learner", "Late learner", "Non-signer"))
by.ptcp.anticipation.rate.q$TurnType <- factor(
  by.ptcp.anticipation.rate.q$Question,
  labels = c("Non-question", "Question"))
question.group.plot <- ggplot(
  aes(y = mean.ant, x = Group),
  data = by.ptcp.anticipation.rate.q) +
  facet_wrap(~ TurnType) +
  geom_violin(fill = "gray80") +
  geom_boxplot(width = 0.1) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  ylab("Average proportion anticipations")








