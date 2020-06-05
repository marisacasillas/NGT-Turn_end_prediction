
# Read in experiment data
files <- list.files(path=data.path,pattern="*.log")
all.data <- read.bp.output(files)
all.data <- factorizeDF(all.data)
all.data$Code <- as.character(all.data$Code)

# Read in subject info
sub.info <- read.csv(paste(info.path,"SubjectInfo.csv", sep=""))
drop <- c("Order", "Group", "Number")
sub.info <- sub.info[,-which(names(sub.info) %in% drop)]
sub.info.c <- sub.info

# Exclusion 1: A priori participant exclusion (see log notes)
excluded.set1 <- subset(sub.info, IncludeBP == 0)$Subject
sub.info <- subset(sub.info, IncludeBP == 1)

# Merge in participant info
all.data <- merge(all.data, sub.info, by="Subject")
all.data <- all.data[order(all.data$Subject, all.data$Time),]

# Mark second button presses as "MBP"
# (the output is "2" or the name of the stimulus by default)
for (i in 2:nrow(all.data)) {
	if (all.data$Code[i]==all.data$Code[i-1]) {
		all.data$Code[i] <- "MBP"
	}
}

# all.data$Codes:
# <stimulus name> | 2 (first button press) | 3 (second button press)

# Find single button presses (and stimuli)
all.data.sbp <- subset(all.data, Code != "MBP")
# Move the single responses up to the
# stimulus they were responding to
all.data.sbp$Response <- c(all.data.sbp$Code[2:nrow(all.data.sbp)], 0)
# ... repeat for RT and Uncertainty
all.data.sbp$RT <- c(all.data.sbp$TTime[2:nrow(all.data.sbp)], 0)
all.data.sbp$Uncertainty2 <- c(all.data.sbp
	$Uncertainty[2:nrow(all.data.sbp)], 0)

# Subset the data to the stimuli (and their responses)
all.data.sbp <- subset(all.data.sbp, Code != "instructions" &
    Response != "instructions")
all.data.sbp <- subset(all.data.sbp, Code != 2 & Code != 1)
# Convert the responses into response (1) and non-response (0)
all.data.sbp$Response[grep('*.mpeg', all.data.sbp$Response)] <- 0
all.data.sbp$Response[grep('2$', all.data.sbp$Response)] <- 1
all.data.sbp$Response <- as.numeric(all.data.sbp$Response)

# Mark items for their block (test/practice) and
# stimulus (context/target) type
all.data.sbp$Block <- rep("Test", nrow(all.data.sbp))
all.data.sbp$Block[grep('^P', all.data.sbp$Code)] <- "Practice"
all.data.sbp$Stimulus <- rep("Target", nrow(all.data.sbp))
all.data.sbp$Stimulus[grep('*A.mpeg', all.data.sbp$Code)] <- "Context"

# Clean up unneeded columns
drop <- c("EventType", "TTime", "Uncertainty")
all.data.sbp <- all.data.sbp[,-which(names(all.data.sbp) %in% drop)]

# Exclusion 2: Remove participants who button-pressed
# during test-block context videos >5% of the time (3 of 60 trials):
test.contexts <- subset(all.data.sbp, Block == "Test" &
	Stimulus == "Context")
unwantedbps <- aggregate(Response ~ Subject, test.contexts, FUN=sum)
excluded.set2 <- as.character(subset(unwantedbps, Response > 3)$Subject)
all.data.sbp <- all.data.sbp[!all.data.sbp$Subject %in% excluded.set2,]
all.data.sbp$IncludeBP <- NULL

# Subset out the important trials (test block, target stimulus)
test.targets <- subset(all.data.sbp, Block == "Test" &
	Stimulus == "Target")
# Get rid of trials with bad RT certainty (inaccurate measurements)
test.targets <- subset(test.targets, Uncertainty2 < 400)

# Read in item data and trim to wanted information
item.info <- read.csv(paste(info.path,"ItemInfo.csv", sep=""))
keep <- c("BP", "Code", "Duration", "Signer", "Question",
		"QuestionType", "MultiUnit", "SpeakerOnLeft", "DyadCode")
item.info <- item.info[, which(names(item.info) %in% keep)]

# Read in, format, and combine Elan codes
mmelan <- read.table(paste(info.path,"MM.txt", sep=""),
	sep = "\t", quote="", header=F, comment.char="")
mmelan$V2 <- NULL
mmelan$dyad <- "MM"
yrelan <- read.table(paste(info.path,"YR.txt", sep=""),
	sep = "\t", quote="", header=F, comment.char="")
yrelan$V2 <- NULL
yrelan$dyad <- "YR"
allelan <- rbind(mmelan, yrelan)
names(allelan) <- c("tier", "start", "stop", "dur", "code", "dyad")
allelan <- factorizeDF(trimWhitespaceDF(allelan))
write.csv(allelan, paste(info.path,"combined.elan.csv", sep=""),
	row.names=FALSE)

# Add signer column to Elan info
allelan$Signer <- "none"
signerlist <- c("YN", "RC", "MG", "MZ")
tiernames <- unique(allelan$tier)
for (tn in tiernames) {
	tnparts <- unlist(strsplit(as.character(tn), '_'))
	if (tnparts[length(tnparts)] %in% signerlist) {
		allelan[which(allelan$tier == tn),]$Signer <-	
			tnparts[length(tnparts)]
	}
}

# Add Elan codes to item data
item.info <- merge.ii.elan(item.info, allelan)
write.csv(item.info, paste(info.path,
	"items.with.elan.codes.csv",
	sep=""), row.names=FALSE)

# Add dummy-codes for annotations:
# First add a column for each annotation type
codecols <- grep(".code", names(item.info))
for (col in codecols) {
	codetypes <- sort(unique(item.info[,col]))
	allcoltypes <- c()
	for (type in codetypes) {
		if (type == "") next
		smcodes <- unlist(strsplit(type, split='\\^'))
		for (i in 1:length(smcodes)) {
			if (!(smcodes[i] %in% allcoltypes)) {
				allcoltypes <- append(allcoltypes, smcodes[i])
			}
		}
	}
	for (coltype in allcoltypes) {
		types <- unlist(strsplit(coltype, split='\\#'))
		for (type in types) {
			newtypename <- paste(gsub("[+-]", "..", type), ".ann", sep="")
			if (!(newtypename %in% names(item.info))) {
				item.info[, paste(newtypename)] <-
					rep(0, nrow(item.info))		
			}
			
		}
	}
}

# Then add in ones for the actual annotations in each case
dummycols <- grep(".ann", names(item.info))
for (col in dummycols) {
	ann.lab1 <- paste(unlist(strsplit(
		gsub("\\.\\.", "\\\\\\+", names(item.info)[col]), "\\."))[1],
		"($|[#^])", sep="")
	ann.lab2 <- paste(unlist(strsplit(
		gsub("\\.\\.", "\\\\\\-", names(item.info)[col]), "\\."))[1],
		"($|[#^])", sep="")
	for (codecol in codecols) {
		matchrows <- c(grep(ann.lab1, item.info[,codecol]),
			grep(ann.lab2, item.info[,codecol]))
		item.info[matchrows, col] <- 1
	}
}

# Brows
# raise = AU1+2, AU1+2+4
# frown = AU4, AU9, AU4+5, AU4+9
# all = raise + frown
item.info$Brows_summary_raises <- ifelse(
	item.info$AU1..2.ann +
	item.info$AU1..2..4.ann > 0, 1, 0)
item.info$Brows_summary_frowns <- ifelse(
	item.info$AU4.ann +
#	item.info$AU9.ann + # None in the data (inclusion yields an error)
	item.info$AU4..5.ann +
	item.info$AU4..9.ann > 0, 1, 0)
item.info$Brows_summary_raiseandfrown <- ifelse(
	item.info$Brows_summary_raises +
	item.info$Brows_summary_frowns > 0, 1, 0)

# Head tilts
# all = HT | HPf | HPb
item.info$Head_summary_tilts <- ifelse(
	item.info$HT.ann +
	item.info$HPf.ann +
	item.info$HPb.ann > 0, 1, 0)

# Blinks = blink (from head code summary)
item.info$Head_summary_blinks <- item.info$blink.ann

# Head movements
item.info$Head_summary_mvmts <- ifelse(
	item.info$Head_summary_tilts +
	item.info$NM..BC.ann +
	item.info$NM.ann +
	item.info$nod.ann > 0, 1, 0)
	
# Backchanneling
# NM-BC | NM | nod
item.info$Backchannel <- ifelse(
	item.info$NM..BC.ann +
	item.info$NM.ann +
	item.info$nod.ann > 0, 1, 0)

# Non-manual prosodic cues =
# Brows.all | Head.all | Blinks |
# nod, wiggle (from head code summary) |
# HS | AU5 (from brows code summary) |
item.info$Nonmanual_prosodic_cues <- ifelse(
	item.info$Brows_summary_raiseandfrown +
	item.info$Head_summary_mvmts +
	item.info$Head_summary_blinks +
	item.info$nod.ann +
	item.info$Wiggle.ann +
	item.info$HS.ann +
	item.info$AU5.ann > 0, 1, 0)

# Manual prosodic cues =
# lengthening, fold-hands, self-groom (from main gloss code summary) |
# hold (from main gloss and non dom code summaries)
item.info$Manual_prosodic_cues <- ifelse(
	item.info$lenghtening.ann +
	item.info$fold..hands.ann +
	item.info$self..groom.ann +
	item.info$hold.ann > 0, 1, 0)

# Question words =
# QS, PO, HOE, HOE-LANG, etc. (from main gloss code summary)
item.info$Question_words <- ifelse(
	item.info$QS.ann +
	item.info$PO.ann +
	item.info$HOE.ann +
	item.info$HOE..LANG.ann +
	item.info$WAAROM.ann +
	item.info$WAT.ann +
	item.info$HOEVEEL.ann > 0, 1, 0)

# Lexical cues =
# Question words |
# JIJ, JULLIE-TWEE (from main gloss code summary)
# JA???
item.info$Lexical_cues <- ifelse(
	item.info$Question_words +
	item.info$JIJ.ann +
#	item.info$JA.ann +
	item.info$JULLIE..TWEE.ann > 0, 1, 0)

# Tags =
# NM, tag, nod, wiggle (from head code summary) |
# QS (from main gloss summary)
item.info$Tag_cues <- ifelse(
	item.info$NM.ann +
	item.info$tag1.ann +
	item.info$tag2.ann +
	item.info$tag3.ann +
	item.info$tag4.ann +
	item.info$nod.ann +
	item.info$Wiggle.ann +
	item.info$QS.ann > 0, 1, 0)

# # Late tag cues =
# # NM, tag, nod, wiggle, QS (from late cue code summary)
# item.info$Late_tag_cues <- 0
# item.info$Late_tag_cues[grep("NM|tag|nod|wiggle|QS",
	# item.info$LateCue.code)] <- 1

# Non WH Question cues =
# JIJ, JULLIE-TWEE (from main gloss and non com code summaries)
item.info$NonWHQuestion_words <- ifelse(
	item.info$JIJ.ann +
	item.info$JULLIE..TWEE.ann > 0, 1, 0)

# For stats Q3 --------
# Grammaticalized & question-associated
# (includes one international sign)
item.info$Gramm.Q.Assoc.s <- ifelse(
  item.info$QS.ann +
    item.info$HOE.ann +
    item.info$HOE..LANG.ann +
    item.info$WAAROM.ann +
    item.info$WAT.ann +
    item.info$HOEVEEL.ann > 0, 1, 0)

# Gesture-assoc. & question-associated
# PO, Jij/jullie, frowning, raising, blinks, tags,
item.info$Gest.Q.Assoc.s <- ifelse(
  item.info$PO.ann +
    item.info$NonWHQuestion_words +
    item.info$Brows_summary_raiseandfrown +
    item.info$Head_summary_blinks +
    item.info$tag1.ann +
    item.info$tag2.ann +
    item.info$tag3.ann +
    item.info$tag4.ann > 0, 1, 0)

item.info$MGest.Q.Assoc.s <- ifelse(
  item.info$PO.ann +
    item.info$NonWHQuestion_words > 0, 1, 0)

# Write out completed item.info
write.csv(item.info, paste(info.path,
	"items.codes.all.csv",
	sep=""), row.names=FALSE)

# Merge item info into main data
test.targets <- merge(test.targets, item.info, by=c("Code","BP"))
test.targets <- test.targets[order(test.targets$Subject,
	test.targets$Time),]

# Correct and round reaction time and
# create an index for item order
test.targets$RTms <- round((test.targets$RT)/10,0)
subs <- unique(test.targets$Subject)
for (sub in subs) {
	subdata <- which(test.targets$Subject == sub)
	# Index for item order
	trialseq <- round(seq(1,length(subdata),1) / length(subdata),2)
	meanRT <- sub.info[which(sub.info$Subject == sub),]$Mean
	test.targets$RTms[subdata] <- round(
		test.targets$RTms[subdata]-meanRT,0)
	test.targets$Trial[subdata] <- trialseq
}

# Set up Item and Signer columns
test.targets$Item <- paste(test.targets$DyadCode,
	test.targets$Code, sep="-")
test.targets$Signer <- as.factor(test.targets$Signer)

# Make sure that non-responses have no RT
test.targets$RTms[which(test.targets$Response == 0)] <- 0

# Exclusion 3: Exclude participants who frequently pressed the
# button late (>10%; 6/60 trials):
test.targets$VeryLate <- ifelse(
	test.targets$RTms - test.targets$Duration > 500,1,0)
excluded.set3 <- subset(aggregate(
	VeryLate ~ Subject, test.targets, sum), VeryLate > 6)$Subject
test.targets <- test.targets[!test.targets$Subject %in% excluded.set3,]
write.csv(test.targets, paste0(
  processed.data.path, "prepped.data.for.analysis.csv"),
  row.names = FALSE)

# Output a summary of the excluded participants
excluded <- c(as.character(excluded.set1),
	as.character(excluded.set2), as.character(excluded.set3))
reasons <- c(rep("pre-analysis",length(excluded.set1)),
	rep("context-presses", length(excluded.set2)),
	rep("freq-late", length(excluded.set3)))
excluded.summary <- data.frame(Subject=excluded,
	Reason=reasons, stringsAsFactors=FALSE)
excluded.summary <- merge(excluded.summary, sub.info.c, by="Subject")
write.csv(excluded.summary, "excluded.participants.csv", row.names=FALSE)

# # Add summary columns for the Elan codes
# colnames <-names(item.info)
# coltypes <- sort(unique(sapply(strsplit(colnames,
	# split='_', fixed=TRUE), getFirstElement)))
# colheads <- sort(unique(sapply(strsplit(colnames,
	# split='.', fixed=TRUE), getFirstElement)))
# # Summarize common codes across signers in the stimuli
# tocollapse <- sort(colheads[grep('_', colheads)])
# collapsetypes <- sort(unique(sapply(strsplit(tocollapse,
	# split='_', fixed=TRUE), getFirstElement)))
# for (i in collapsetypes) {
	# groups <- grep(i, names(item.info))
	# if (length(groups) > 2) {
		# codes <- item.info[,grep(paste(i,"[A-Za-z_]*.code", sep=""),
			# names(item.info))]
		# starts <- item.info[,grep(paste(i,"[A-Za-z_]*.relStart", sep=""),
			# names(item.info))]
		# stops <- item.info[,grep(paste(i,"[A-Za-z_]*.relStop", sep=""),
			# names(item.info))]
		# durs <- item.info[,grep(paste(i,"[A-Za-z_]*.dur", sep=""),
			# names(item.info))]
		# item.info$NewCodes <- apply(codes, 1, paste, collapse="")
		# item.info$NewStarts <- apply(starts, 1, sum)
		# item.info$NewStops <- apply(stops, 1, sum)
		# item.info$NewDurs <- apply(durs, 1, sum)
		# names(item.info)[(length(item.info)-3):length(item.info)] <-
			# c(paste(i, "_summary.code", sep=""),
			# paste(i, "_summary.start", sep=""),
			# paste(i, "_summary.stop", sep=""),
			# paste(i, "_summary.dur", sep=""))
	# }
# }
# # Replace dashes with dots in column names
# names(item.info) <- gsub('-', '.', names(item.info))
