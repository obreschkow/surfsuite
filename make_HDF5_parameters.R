#filebase = "/Users/do/Data/SURFS/L40_N512/velociraptor/snapshot_199"
#filebase = "/Users/do/Data/SURFS/L210_N512/velociraptor/snapshot_199"
#filebase = "/Users/do/Data/SURFS/L210_N1024/velociraptor/snapshot_199"
#filebase = "/Users/do/Data/SURFS/L210_N1024-Hydro/velociraptor/snapshot_199"
#filebase = "/Users/do/Data/SURFS/L210_N1536/velociraptor/snapshot_199"
filebase = "/Users/do/Data/SURFS/L900_N2048/velociraptor/snapshot_008"

extension = {}
extension[[1]] = ".VELOCIraptor.hdf.catalog_groups."
extension[[2]] = extension[[1]]
extension[[3]] = ".VELOCIraptor.hdf.catalog_particles."
field = {}
field[[1]] = "Offset"
field[[2]] = "Parent_halo_ID"
field[[3]] = "Particle_IDs"
variable = {}
variable[[1]] = "hdf5_groups_ofst  "
variable[[2]] = "hdf5_groups_phid  "
variable[[3]] = "hdf5_particles    "

library(rhdf5)
library(bit64)

H5close()

for (mode in seq(3)) {
  p = rep(NA,5)
  val = rep(NA,5)
  filename = sprintf('%s%s%d',filebase,extension[[mode]],0)
  filesize = file.info(filename)$size
  dat = h5read(filename,field[[mode]],bit64conversion='int')
  n = length(dat)
  p[2] = floor(filesize/n/2)*2
  p[1] = filesize-n*p[2]
  index = 0
  shift = F
  while (index==0) {
    fileid <- file(filename, "rb")
    if (shift) {readBin(fileid, integer(), size=4)}
    for (i in seq(filesize/8-16)) {
      val[1:4] = val[2:5]
      val[5] = readBin(fileid, integer(), size=8)
      if (all(val==dat[1:5])) {
        if (shift) {
          index = (i-5)*8+4
        } else {
          index = (i-5)*8
        }
        break
      }
    }
    close(fileid)
    if (index==0 & shift) {stop('Not found')}
    shift = T
  }
  p[4] = floor(index/n/2)*2
  p[3] = index-n*p[4]
  p[5] = 8
  cat(sprintf('%s%d,%d,%d,%d,%d\n',variable[[mode]],p[1],p[2],p[3],p[4],p[5]))
}