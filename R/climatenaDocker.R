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

## Docker arguments for one ClimateNA point-mode run: ClimateNA reads `inCsv` (columns id1, id2,
## lat, long, elev) and writes `outCsv`, both in `workDir`, mounted at /data/w. The image's
## climatena-run wrapper would create the output path as a folder, so `sh` runs Wine the same
## way here: the user check, ClimateNA's own directory, xvfb-run, then waiting for wineserver.
## The script is shell-quoted, ready for system2().
.climatenaPointDockerArgs <- function(
  name,
  image,
  climatenaDir,
  workDir,
  exe,
  msy,
  periodArg,
  inCsv,
  outCsv,
  uid,
  gid,
  cpus = 1,
  memory = "1g"
) {
  winPath <- function(f) paste0('"/Z:\\data\\w\\', f, '"')
  script <- paste(
    'owner=$(stat -c %u "$WINEPREFIX")',
    'if [ "$(id -u)" != "$owner" ]; then',
    '  echo "run with --user matching the image (Wine prefix owned by uid $owner)" >&2; exit 64',
    "fi",
    "cd /opt/climatena",
    paste(
      "xvfb-run -a wine",
      exe,
      paste0("/", msy),
      paste0("/", periodArg),
      winPath(inCsv),
      winPath(outCsv)
    ),
    "rc=$?",
    "wineserver -w || true",
    "exit $rc",
    sep = "\n"
  )
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
    paste0(workDir, ":/data/w"),
    "--entrypoint",
    "sh",
    image,
    "-c",
    shQuote(script) # system2() passes arguments to the host shell as they are
  )
}
