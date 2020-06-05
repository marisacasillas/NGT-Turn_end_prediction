# library(party)
# library(REEMtree)
library(ggplot2)
library(grid)
library(ggplot2)
library(viridis)
library(lme4)
library(grid)
library(stringr)

# Set paths
data.path <- "exp_output/" # experiment output files
info.path <- "supp_info/" # supplementary input files
processed.data.path <- "processed_data/" # processed data (merged exp + supp info)
model.path <- "processed_data/models/" # statistical output
plot.path <- "plots/" # output plots

# Style elements for plotting
theme_set(theme_bw())

plot.style <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(2,"lines"),
  panel.background = element_rect(
    fill = "transparent",colour = NA),
  plot.background = element_rect(
    fill = "transparent",colour = NA),
  legend.background = element_rect(
    fill="transparent"),
  legend.text = element_text(size=30),
  legend.title = element_text(size=30),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.key.height = unit(2, "lines"),
  axis.text.x = element_text(size=30),
  axis.title.x = element_text(size=30, vjust=-.5),
  axis.text.y = element_text(size=30),
  axis.title.y = element_text(size=30,vjust=0.25),
  axis.line = element_line(colour="black",size=.5),
  axis.ticks = element_line(size=.5),
  strip.text = element_text(size=30))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### Custom helper functions ##########
# Trim trailing spaces
trim.whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)

# Return the first element in a list
getFirstElement <- function(x) (x[1])

# Factorize character columns in a data frame
factorizeDF <- function(df) {
	idxs <- sapply(df, is.character)
	df[idxs] <- lapply(df[idxs], as.factor)
	return(df)
}

# Rounding function
round100mean <- function(n) {
  return(round(mean(n), 2))
}

# Make numeric start/stop/dur columns in a data frame
trimWhitespaceDF <- function(df) {
	idxs <- !sapply(df, is.integer)
	df[idxs] <- lapply(df[idxs], trim.whitespace)
	return(df)
}

# Return the column index of the column name that matches
# the input string
findColIdx <- function(colname, item.info) {
	return(which(unlist(lapply(names(item.info), function(x, y)
		as.character(x) == as.character(y), y=colname))))
}

# Standard error of the mean
sem <- function(x) {
    sd(x) / sqrt(length(x))
}

# Read in the experiment output files from the BP task
read.bp.output <- function(filelist) {
	all.data <- data.frame(stringsAsFactors=FALSE)
	for (file.name in filelist) {
	#	print(file.name)
	    text <- scan(paste(data.path, file.name, sep=""),
	    		character(0), sep = "\n", quiet=TRUE)
	    group <- unlist(strsplit(file.name, "[-]"))[1]
	    number <- unlist(strsplit(file.name, "[-]"))[2]
	    date <- unlist(strsplit(text[2], "[ ]"))[4]
	    testtime <- unlist(strsplit(text[2], "[ ]"))[5]
		n <- length(text)-3
		BPdata <- data.frame(
			Trial=rep(NA, n),
	    		EventType=rep("", n),
	    		Code=rep("", n),
	    		Time=rep(NA, n),
	    		TTime=rep(NA, n),
	    		Uncertainty=rep(NA, n),
	    		stringsAsFactors=FALSE)
	    		bp_idx <- 1
	    for (i in 4:length(text)) {
	    	newrow <- strsplit(as.character(text[i]),'\t',fixed=TRUE)[[1]][2:7]
	    	BPdata[bp_idx, ] <- list(
			Trial=as.numeric(newrow[1]),
	    		EventType=newrow[2],
	    		Code=newrow[3],
	    		Time=as.numeric(newrow[4]),
	    		TTime=as.numeric(newrow[5]),
	    		Uncertainty=as.numeric(newrow[6]))
			bp_idx <- bp_idx + 1
	    } 
	    BPdata$Subject <- rep(paste(group,number,sep="-"), nrow(BPdata))
	    BPdata$Group <- rep(group, nrow(BPdata))
	    BPdata$Number <- rep(as.numeric(number), nrow(BPdata))
	    BPdata$Date <- rep(date, nrow(BPdata))
	    BPdata$TestTime <- rep(testtime, nrow(BPdata))
		all.data <- rbind(all.data, BPdata)
	}
	return(all.data)
}

# Merge the Elan codes into the item info df
merge.ii.elan <- function(item.info, elan.info) {
	for (i in 1:nrow(item.info)) {
		item <- unlist(strsplit(as.character(item.info$Code[i]), "[.]"))[1]
		idyad <- item.info$DyadCode[i]
		isigner <- as.character(item.info$Signer[i])
		stimrow <- subset(elan.info, code == item & dyad == idyad)
		annots <- subset(elan.info,
			Signer == isigner &
			start < stimrow$stop & 
			stop > stimrow$start &
			dyad == idyad & code != item &
			tier != "Potential turn ends")
		if (nrow(annots) > 0) {
			# Create new columns if necessary
			uniqtiers <- unique(annots$tier)
			tierfreqs <- data.frame(table(annots$tier))
			for (j in uniqtiers) {
				# check to see if the tier name is already a column name in
				# item.info. If not, make the associated cols for that tier
				if (length(grep(j, names(item.info))) == 0) {
					tier <- as.character(j)
					item.info[[paste(tier, ".code", sep="")]] <- ""
					item.info[[paste(tier, ".relStart", sep="")]] <- 0
					item.info[[paste(tier, ".relStop", sep="")]] <- 0
					item.info[[paste(tier, ".dur", sep="")]] <- 0
				}
			}
			# Add data into extra columns
			for (k in 1:nrow(annots)) {
				tier <- as.character(annots$tier[k])
				tierfreq <- subset(tierfreqs, Var1 == tier)$Freq
				codetier <- findColIdx(paste(tier, ".code", sep=""),
					item.info)
				item.info[i, codetier] <- ifelse(
					item.info[i, codetier] == "",
					as.character(annots$code[k]),
					paste(item.info[i, codetier],
						as.character(annots$code[k]), sep="^"))
				corrstart <- annots$start[k] - stimrow$start
				corrstart <- ifelse(corrstart < 0,
					0, corrstart)
				starttier <- findColIdx(paste(tier, ".relStart", sep=""),
					item.info)
				item.info[i, starttier] <- ifelse(
					item.info[i, starttier] == 0,
					ifelse(tierfreq == 1,
						corrstart,
					ifelse(corrstart == 0,
						paste(corrstart,"", sep="^"),
						corrstart)),
					ifelse(str_sub(item.info[i, starttier], -1) == "^",
						paste(item.info[i, starttier],
							as.character(corrstart), sep=""),
						paste(item.info[i, starttier],
							as.character(corrstart), sep="^")))
				corrstop <- annots$stop[k] - stimrow$start
				corrstop <- ifelse(corrstop > stimrow$dur,
					stimrow$dur, corrstop)
				stoptier <- findColIdx(paste(tier, ".relStop", sep=""),
					item.info)
				item.info[i, stoptier] <- ifelse(
					item.info[i, stoptier] == 0,
					corrstop,
					paste(item.info[i, stoptier],
						as.character(corrstop), sep="^"))
				durtier <- findColIdx(paste(tier, ".dur", sep=""),
					item.info)
				act.dur <- corrstop - corrstart
				item.info[i, durtier] <- ifelse(
					item.info[i, durtier] == 0,
					act.dur,
					paste(item.info[i, durtier],
						as.character(act.dur), sep="^"))
			}
		}
	}
	return(item.info)
}

# Extract individual TCUs from the Elan coding
getAllTCUs <- function(itemlist, elan.info) {
	TCUs <- data.frame(stringsAsFactors=FALSE)
	for (item in itemlist) {
		idyad <- unlist(strsplit(item, "-"))[1]
		turn <- unlist(strsplit(unlist(strsplit(item, "-"))[2], "\\."))[1]
		itemrow <- elan.info[which(
			elan.info$code == turn & elan.info$dyad == idyad),]
		# Find potential turn ends
		pTCUs <- subset(elan.info, start < itemrow$stop & 
			stop > itemrow$start & dyad == idyad & code != turn &
			tier == "Potential turn ends")
		nends <- nrow(pTCUs)
		if (nends > 0) {
			for (i in 1:nends) {
				if (i < nends) {
					TCUs <- rbind(TCUs, data.frame(
						dyad=idyad, turn=turn, endings=nends, endnum=i,
						endtime=pTCUs$stop[i]-itemrow$start,
						itemendtime=itemrow$dur))
				} else {
					TCUs <- rbind(TCUs, data.frame(
						dyad=idyad, turn=turn, endings=nends, endnum=i,
						endtime=itemrow$dur,
						itemendtime=itemrow$dur))					
				}
			}
			
		} else {
			TCUs <- rbind(TCUs, data.frame(
				dyad=idyad, turn=turn, endings=1,
				endnum=1, endtime=itemrow$dur,
				itemendtime=itemrow$dur))
		}
	}
	return(TCUs)
}