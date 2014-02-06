STA250 - HW 2 Notes

# Game Plan

# Take HW1 code for freq table and modify it to
# work as a function rather than free-form.
# Make a list with all relevant files, then group
# it into proper chunks.
# Run the cluster methods described in class, making
# modifications as necessary

# NEW IDEAS: Parallelizing the Tables

# Establish a STANDARDIZED FORM for all the tables so you
# can add values appropriately (For instance, -5000 to 5000)

# read through frequency table "delays" column,
# identify associated counts, and add them

# This turns "delay.tab" into a standardized table that
# takes entries from -4999 to 5000, all with Freq = 0
Time = (-4999:5000)
delay.tab = (data.frame(table(Time)))
delay.tab[[2]] = 0

delays.table = function(files, table){
    # Splits the nodes files into old and new formats
    files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
    files.new = files[grep('[a-z].csv', files)]
    
    # Subjects each type of file to the appropriate regex
    # commands, using paste to insert filenames to cat
    
    # If a mix of old and new files exists
    if(length(files.old) != 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                         egrep -v 'ArrDelay' \
                         cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                         egrep -v 'ARR_DEL15'"))
    }
    # If only old files exist
    else if(length(files.old) != 0 & length(files.new) == 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                         egrep -v 'ArrDelay'"))
    }
    # If only new files exist
    else if(length(files.old) == 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                         egrep -v 'ARR_DEL15'"))
    }
    # The above distinctions must be made, or else shell will
    # attempt to operate on empty space and hang
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    delays = as.numeric(delays) # removes parentheses
    # Forms frequency table
    delays.all = data.frame(table(delays))
    # List all possible delay times, removing NAs
    delays.all = delays.all[1:nrow(delays.all)-1,] 
    delays.all # returns delay table
}

# First test parallelization on a handful of the files

setwd("~/Desktop/STA_250_HW1")
stopCluster(cl) # Stop the cluster & free up memory
library(parallel)

# *** RECENT NOTES ***
# The delays can fit in a table that runs -1500 to 2500


# *** CLUSTER TABLE TEST ***
t.files = files[1:12] # Contains old only - 1 min read, 40 sec table
t.files = files[69:80] # Contains new only - 16 sec read, 5 sec table
t.files = files[14:23] # Contains both types - 40 sec read, 40 sec table

cl = makeCluster(4, "FORK") # Create cluster of 4 nodes
t.filesplit = clusterSplit(cl, t.files) # split files among nodes

# This generates a list of size 4, with each one containing
# many files' worth of delay times in numeric form
delays.list = clusterApply(cl, t.filesplit, delays.table)

# IDEA:
# names = unique(x1,x2,x3,x4) # Where x1-x4 are "delay"
# vector = apply(names, x1$count[names[1]] + x2$count[names[]])

lvls = function(list, n)
{
    x = as.numeric(levels(list[[n]]))
    x
}

# Extracts unique Time Delay Values
names = unique(c(delays.list[[1]][1], delays.list[[2]][1],
                 delays.list[[3]][1], delays.list[[4]][1]))
# Merges all unique time delay values into a
# single common set of values
names = unique(c(lvls(names,1), lvls(names,2),
               lvls(names,3), lvls(names,4)))
names = sort(names) # sorts them numerically

del.table = data.frame(names,)

# Look at values of names. If a value in names matches
# the value in delays, extract the frequency associated
# with that delay value



# *** CLUSTER TEST ***

t.files = files[1:12] # Contains old only - 1 min read, 40 sec table
t.files = files[69:80] # Contains new only - 16 sec read, 5 sec table
t.files = files[14:23] # Contains both types - 40 sec read, 40 sec table

cl = makeCluster(4, "FORK") # Create cluster of 4 nodes
t.filesplit = clusterSplit(cl, t.files) # split files among nodes

# This generates a list of size 4, with each one containing
# many files' worth of delay times in numeric form
delays.list = clusterApply(cl, t.filesplit, delays)

# Condenses the whole list into a single vector
delays.all = rapply(delays.list,c)
# Forms frequency table
delays.all = data.frame(table(delays.all)) # About 40 sec for first 12...
# List all possible delay times, removing NAs
delays.all = delays.all[1:nrow(delays.all)-1,] 
delays.all # returns delay table

plot(delays.all[,1],as.numeric(delays.all[,2]))

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

# *** DELAY FUNCTIONS ***
delays.table = function(files){
    # Splits the nodes files into old and new formats
    files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
    files.new = files[grep('[a-z].csv', files)]
    
    # Subjects each type of file to the appropriate regex
    # commands, using paste to insert filenames to cat
    
    # If a mix of old and new files exists
    if(length(files.old) != 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                         egrep -v 'ArrDelay' \
                         cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
    }
    # If only old files exist
    else if(length(files.old) != 0 & length(files.new) == 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                    egrep -v 'ArrDelay'"))
    }
    # If only new files exist
    else if(length(files.old) == 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
    }
    # The above distinctions must be made, or else shell will
    # attempt to operate on empty space and hang
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    delays = as.numeric(delays) # removes parentheses
    # Forms frequency table
    delays.all = data.frame(table(delays))
    # List all possible delay times, removing NAs
    delays.all = delays.all[1:nrow(delays.all)-1,] 
    delays.all # returns delay table
}

delays = function(files){
    # Splits the nodes files into old and new formats
    files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
    files.new = files[grep('[a-z].csv', files)]
    
    # Subjects each type of file to the appropriate regex
    # commands, using paste to insert filenames to cat
    
    # If a mix of old and new files exists
    if(length(files.old) != 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                         egrep -v 'ArrDelay' \
                         cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
    }
    # If only old files exist
    else if(length(files.old) != 0 & length(files.new) == 0)
    {
        con = pipe(paste("cat", paste(files.old, collapse = " "),
                         "| cut -f 15 -d, | egrep -v '^$' | 
                    egrep -v 'ArrDelay'"))
    }
    # If only new files exist
    else if(length(files.old) == 0 & length(files.new) != 0)
    {
        con = pipe(paste("cat", paste(files.new, collapse = " "),
                         "| cut -f 45 -d, | egrep -v '^$' |
                    egrep -v 'ARR_DEL15'"))
    }
    # The above distinctions must be made, or else shell will
    # attempt to operate on empty space and hang
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    delays = as.numeric(delays) # removes parentheses
    delays # outputs delay values
}



# ******
# PREVIOUS FUNCTIONS

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
