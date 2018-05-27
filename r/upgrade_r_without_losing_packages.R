

# Before you upgrade, build a temp file with all of your old packages
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
installedpkgs
save(installedpkgs, file="installed_old.rda")

# Once you've got the new version up and running, reload the saved packages and re-install them from CRAN.
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

# Note: If you had any packages from BioConductor, you can update those too!
chooseBioCmirror()
biocLite() 
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
for (i in 1:length(missing)) biocLite(missing[i])