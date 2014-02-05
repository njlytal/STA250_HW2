# STA 250 HW 2 - METHOD 1: Clustering in R


# GOAL: Take HW1 code for freq table and modify it to
# work as a function rather than free-form.
# Make a list with all relevant files, then group
# it into proper chunks.
# Run the cluster methods described in class, making
# modifications as necessary

# Basic setup
setwd("~/Desktop/STA_250_HW1")
library(parallel)
# stopCluster(cl) # Stop the cluster & free up memory

# First identify the files to subject to clustering
files = list.files(pattern="csv$")
# We will use clusterSplit later to divide the files
# among the clusters

# Once the files are selected, we must create a function
# that reads their contents and outputs the individual
# delay values. It must account for the two file formats.
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


cl = makeCluster(4, "FORK") # Create cluster of 4 nodes
filesplit = clusterSplit(cl, files) # split files among nodes

# This generates a list of size 4, with each one containing
# many files' worth of delay times in numeric form
delays.all = clusterApply(cl, filesplit, delays.notable)