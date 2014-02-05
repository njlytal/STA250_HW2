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

# Start time calculation
start = proc.time()

cl = makeCluster(4, "FORK") # Create cluster of 4 nodes
# filesplit = clusterSplit(cl, files) # split files among nodes

# This generates a list of size 4, with each one containing
# many files' worth of delay times in numeric form
# delays.list = clusterApply(cl, filesplit, delays) (non-LB way)
delays.list = clusterApplyLB(cl, files, delays)

# Condenses the whole list into a single vector
delays.all = rapply(delays.list,c)
# Forms frequency table
delays.all = data.frame(table(delays.all))
# List all possible delay times, removing NAs
delays.all = delays.all[1:nrow(delays.all)-1,] 
delays.all # returns delay table

stopCluster(cl) # Cluster computation is over

# All possible delay times
d.time = as.numeric(as.matrix(delays.all[1]))
# Frequency of each time
d.count = as.numeric(as.matrix(delays.all[2]))

n = sum(d.count) # Number of entries
sum.prod = sum(d.time*d.count) # sum of products
sum.prod2 = sum((d.time^2)*d.count) # sum of counts by time squared

# Mean of the values
# Takes sum of all products and divides by total # entries
mu = mean((sum.prod)/n)

# Median of the values
# Orders all values and takes middle one
med = sort(rep(d.time,d.count))[n/2]

# Std. dev. of the values
# Uses formula for variance (with n-1 correction),
# then takes square root
sd = sqrt((sum.prod2 - (sum.prod^2)/n)/(n-1))

# End time calculation
time = proc.time()-start


results.calb = list(time = time, results = c(mean = mu, median = med, sd = sd),
                system = Sys.info(),  session = sessionInfo(),
                computer = c(RAM = "16 GB 1600 MHz DDR3",
                             CPU = "2.6 GHz Intel Core i7",
                             Software = "OS X 10.8.5 (12F45)"))


# Compare:
# OLD FREQ TABLE: (6.5665, 0, 31.5563), time(610.358, 24.485, 431.815)
# clusterApply: (6.566486, 0, 31.555595), time(107.533, 7.204, 331.719) n = 145574557
# clusterApplyLB: (6.566486, 0, 31.555595), time(110.464, 8.988, 269.008) n = same
# clusterApplyLb takes 5/8 the time to complete.
# Better but not ideal yet. In a perfect world, would cut
# down to ~1/3, but this requires the table construction
# to be parallelized.
# I can get lists of frequency tables in delays(), BUT there
# doesn't seem to be a reliable way to combine them WITHOUT
# using the given Rthreads code.

# Currently, gain is noticeable but NOT as big as expected.
# Mainly, this is because the table construction itself is
# as of yet NOT parallelized, and takes up a couple minutes
# of computing time. Furthermore, the files are NOT balanced
# in an ideal fashion -- the first node has the 20 biggest files
# compared to the 60 smaller ones on the other nodes. Ideally,
# I would divided these up more smartly, or use clusterApplyLB.

