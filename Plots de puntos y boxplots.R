
install.packages("data.table")

library(data.table)

library(ggplot2)

niceplot <- fread(input = "data.csv")

head(niceplot)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
                if (na.rm) sum(!is.na(x))
                else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                               c(N    = length2(xx[[col]], na.rm=na.rm),
                                 mean = mean   (xx[[col]], na.rm=na.rm),
                                 sd   = sd     (xx[[col]], na.rm=na.rm)
                               )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
}


df <- summarySE(data = niceplot , measurevar = "errors" , groupvars = c("trial","treatment") ,  na.rm=FALSE, conf.interval=.95, .drop=TRUE)

df$treatment <- as.factor(df$treatment)

pd = position_dodge(0.2)

d <- ggplot(data = df, aes(x = trial, y = errors, col = treatment)) +
        
        geom_errorbar(aes(ymin = errors - ci, ymax = errors + ci), width=0.2, size=1 , position=pd) +
        
        geom_line(position=pd) +
        
        geom_point(shape=15, size=4, position=pd) +

        scale_y_continuous("errors",
                           limits = c(0, 20),
                           breaks = seq(0, 20, 5)) +
        
        scale_x_discrete("Trial", limits=c(0,1,2,3)) +
        
        theme_classic() +
        
        theme(axis.line = element_line(colour = 'black', size = 1.5), axis.ticks = element_line(colour = "black", size = 1.5), axis.text=element_text(size=12, face="bold", color="black"))

d




niceplot$trial <- as.factor(niceplot$trial)

d <- ggplot(data = niceplot, aes(x = trial, y = errors, fill = treatment)) +
        
        geom_boxplot(alpha=0.7) +
       
        theme_classic() +
        
        theme(axis.line = element_line(colour = 'black', size = 1.5), axis.ticks = element_line(colour = "black", size = 1.5), axis.text=element_text(size=12, face="bold", color="black"))

d



d <- ggplot(data = niceplot, aes(x = trial, y = errors, fill = treatment)) +
        
        geom_boxplot(alpha=0.7 , position = position_dodge(0.8)) +
        
        geom_dotplot(binaxis='y', stackdir='center' , position = position_dodge(0.8) , alpha = 0.7) +
        
        theme_classic() +
        
        theme(axis.line = element_line(colour = 'black', size = 1.5), axis.ticks = element_line(colour = "black", size = 1.5), axis.text=element_text(size=12, face="bold", color="black"))

d





d <- ggplot(data = niceplot, aes(x = trial, y = errors, fill = treatment)) +
        
        geom_dotplot(binaxis='y', stackdir='center' , position = position_dodge(0.8) , alpha = 0.7) +
        
        theme_classic() +
        
        theme(axis.line = element_line(colour = 'black', size = 1.5), axis.ticks = element_line(colour = "black", size = 1.5), axis.text=element_text(size=12, face="bold", color="black"))

d





d <- ggplot(data = df, aes(x = trial, y = errors, col = treatment)) +
        
        #geom_errorbar(aes(ymin = errors - ci, ymax = errors + ci), width=0.2, size=1 , position=pd) +
        
        geom_line(position=pd) +
        
        geom_area(aes(fill = treatment, group = treatment), alpha = 0.3, position = pd) +
        
        geom_point(shape=15, size=4, position=pd) +
        
        scale_y_continuous("errors",
                           limits = c(0, 15),
                           breaks = seq(0, 15, 5),
                           expand = c(0, 0)) +
        
        scale_x_discrete("Trial", limits=c(0,1,2,3)) +
        
        theme_classic() +
        
        theme(axis.line = element_line(colour = 'black', size = 1.5), axis.ticks = element_line(colour = "black", size = 1.5), axis.text=element_text(size=12, face="bold", color="black"))

d


