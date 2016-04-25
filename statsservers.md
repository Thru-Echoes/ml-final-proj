# Statistics Server README

### Login:

    ssh s133-27@arwen.berkeley.edu

### Log out:

    exit

### Transfer local file to the server:

    scp file.R s133-27@arwen.berkeley.edu:/accounts/class/s133/s133-27/.

### Transfer file on server to local computer:

    scp s133-27@arwen.berkeley.edu:/accounts/class/s133/s133-27/file.RData ~/Desktop

### Run R file in Batch mode:

    R CMD BATCH file.R file.Rout &
