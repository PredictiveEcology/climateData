## Docker arguments for one ClimateNA run in the climatena-wine image (docker/climatena/).
##
## ClimateNA is mounted read-only; the DEM tile's folder and the tile's raw-output folder are
## mounted at /data/dem and /data/out, and ClimateNA writes <outDir>/<period folder>/. The
## container runs as the image's user (`--user`), with one CPU and a memory cap.
.climatenaDockerArgs <- function(
  name,
  image,
  climatenaDir,
  demFile,
  outDir,
  exe,
  msy,
  periodArg,
  uid,
  gid,
  cpus = 1,
  memory = "1g"
) {
  c(
    "run",
    "--rm",
    "--name",
    name,
    "--user",
    paste0(uid, ":", gid),
    "--cpus",
    as.character(cpus),
    "--memory",
    memory,
    "-v",
    paste0(climatenaDir, ":/opt/climatena:ro"),
    "-v",
    paste0(dirname(demFile), ":/data/dem:ro"),
    "-v",
    paste0(outDir, ":/data/out"),
    image,
    exe,
    msy,
    periodArg,
    file.path("/data/dem", basename(demFile)),
    "/data/out"
  )
}
