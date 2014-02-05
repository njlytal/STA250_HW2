STA250 - HW 2 Notes

# Game Plan

# Take HW1 code for freq table and modify it to
# work as a function rather than free-form.
# Make a list with all relevant files, then group
# it into proper chunks.
# Run the cluster methods described in class, making
# modifications as necessary

# First test parallelization on a handful of the files

setwd("~/Desktop/STA_250_HW1")
stopCluster(cl) # Stop the cluster & free up memory
library(parallel)


filesplit[1]

# *** GREP TEST ***
test.filesplit = filesplit
test.files = files

test.old = test.files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv',
                           test.files)]
test.new = test.files[grep('[a-z].csv', files)]
test.old = test.filesplit[[1]][grep('1[0-9]{3}.csv|200[0-7]{1}.csv',
                                    test.filesplit[[1]])]



test.old = c("2005.csv", "2006.csv", "2007.csv")
test.new = c("2009_May.csv", "2010_May.csv")

con = pipe(paste("cat", paste(test.old, collapse = " "),
                 "| cut -f 15 -d, | egrep -v '^$' | 
                    egrep -v 'ArrDelay' \
                    cat", paste (test.new, collapse = " "),
                 "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
           open(con, open="r") # Opens the defined connection to read
           test = readLines(con) # contains all arrival delays
           close(con) # Closes defined connection

# *** APPLYING STRATEGY TO WHOLE DATA SET

# This captures everything at once. We want to divide this
# over all clusters though.
con = pipe(paste("cat", paste(files.old, collapse = " "),
                    "| cut -f 15 -d, | egrep -v '^$' | 
                    egrep -v 'ArrDelay' \
                    cat", paste (files.new, collapse = " "),
                    "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))


           

# GOAL: clusterApply gives a set of 20 filenames to each of
# the four clusters. Once filenames are passed to a cluster,
# it should grep the appropriate filenames, then pass

delays = function(files){
    # Splits the nodes files into old and new formats
    files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
    files.new = files[grep('[a-z].csv', files)]
    
    # Subjects each type of file to the appropriate regex
    # commands, using paste to insert filenames to cat
    con = pipe(paste("cat", paste(files.old, collapse = " "),
                     "| cut -f 15 -d, | egrep -v '^$' | 
                    egrep -v 'ArrDelay' \
                    cat", paste (files.new, collapse = " "),
                     "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    delays = as.numeric(delays) # returns delay values
    delays
}

files = list.files(pattern="csv$")
cl = makeCluster(4, "FORK")
filesplit = clusterSplit(cl, files)
end = clusterApply(cl, filesplit, delays.notable)


# *** TEST RUN with 1987-1994

ff = listfiles(pattern="csv$", full.names=TRUE)

# Here, let "files" be ff

# Assignment 1 in a nutshell (retool your code!)
# files
f = function(files){
    # Call shell/C/Java code to do things. Examples:
    getFreqTable(fs) # where getFreq works
    for(f in fs)
        getFreqTable(f)
}

f(ff) # This should produce our results

# ******
# EXECUTED BELOW

# NOTE: Alter delays to just COLLECT the data, NOT
# to merge them into frequency tables yet! 
delays.table = function(files){
    con = pipe(paste("cat", paste(files, collapse = " "),
                     "| cut -f 15 -d, | egrep -v '^$' | egrep -v 'ArrDelay'"))
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    #removes NA counts, contained in last row of table
    delays = data.frame(table(delays))
    delays = delays[1:nrow(delays)-1,] # All possible delay times
    delays # returns delay table
}

delays = function(files){
    con = pipe(paste("cat", paste(files, collapse = " "),
                     "| cut -f 15 -d, | egrep -v '^$' | egrep -v 'ArrDelay'"))
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    delays = as.numeric(delays) # returns delay values
    delays
}

# STRATEGY
test.files = c("1987.csv", "1988.csv")
end2 = lapply(files, delays.notable)
test = lapply(test.files, delays.notable)

# Combine all lists into one big vector, then
# take the table, as before
test.2 = c(unlist(test[1]),unlist(test[2]))

test.2 = rapply(test,c) # This appears to work!


cl = makeCluster(4, "FORK")
filesplit = clusterSplit(cl, files)
end = clusterApply(cl, filesplit, delays.notable)
# This produces a list of four elements, where
# each element contains individual delays.

# Condenses the whole list in a single vector
end = rapply(end,c)
end = data.frame(table(end))
end = end[1:nrow(end)-1,] # All possible delay times
end # returns delay table



stopCluster(cl) # Stop the cluster & free up memory

# ******
# SINGLE FILE SORT TESTER

con = pipe("cat 1987.csv | cut -f 15 -d, | egrep -v '^$' |
           egrep -v 'ArrDelay'")
open(con, open="r") # Opens the defined connection to read
test = readLines(con) # contains all arrival delays
close(con) # Closes defined connection
test = as.numeric(test)



con = pipe("cat 1987.csv | cut -f 15 -d, | egrep -v '^$' |
           egrep -v 'ArrDelay'")
open(con, open="r") # Opens the defined connection to read
test.1 = readLines(con) # contains all arrival delays
close(con) # Closes defined connection
test.1 = data.frame(table(test.1))
test.1 = delays[1:nrow(test.1)-1,] # All possible delay times
test.1 # returns delay table
names(test.1) = c('delay','Freq')

con = pipe("cat 1988.csv | cut -f 15 -d, | egrep -v '^$' |
           egrep -v 'ArrDelay'")
open(con, open="r") # Opens the defined connection to read
test.2 = readLines(con) # contains all arrival delays
close(con) # Closes defined connection
test.2 = data.frame(table(test.2))
test.2 = delays[1:nrow(test.2)-1,] # All possible delay times
test.2 # returns delay table
names(test.2) = c('delay','Freq')


# TECHNIQUE REMINDER

test = c("1987.csv","1988.csv","1989.csv","1990.csv")
filesplit[1] # file names for first cluster

# This takes a vector of filenames and collapses it into
# a single argument of filenames with spaces, to be inserted
# into a pipe expression
paste(test, collapse = " ")

# By using paste, we can enter file names into a pipe
# expression
con = pipe(paste("cat", paste(test, collapse = " "), "| cut -f 15 -d, | egrep -v '^$' |
           egrep -v 'ArrDelay'"))

open(con, open="r") # Opens the defined connection to read
delays = readLines(con) # contains all arrival delays
close(con) # Closes defined connection

delays = data.frame(table(delays))
#removes NA counts, contained in last row of table
delays = delays[1:nrow(delays)-1,] 
# All possible delay times
