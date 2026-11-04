# ClimateNA (a 32-bit Visual Basic 6 Windows program) under Wine, for running on Linux nodes.
# ClimateNA itself is not in the image (its download requires registration): mount an unpacked
# ClimateNA directory at /opt/climatena at run time.
#
# Base pinned by digest (debian:bookworm-slim, 2026-11-04) so rebuilds are reproducible.
FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3

ARG UID=1000
ARG GID=1000
ENV DEBIAN_FRONTEND=noninteractive

# 32-bit Wine only (ClimateNA is a 32-bit program). winetricks (Debian contrib) installs the
# Visual Basic 6 runtime ClimateNA needs (MSVBVM60.DLL); winetricks checks the SHA-256 of
# every file it downloads, so a changed or corrupted download fails the build.
RUN sed -i 's/^Components: main$/Components: main contrib/' /etc/apt/sources.list.d/debian.sources \
  && dpkg --add-architecture i386 \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
    wine wine32:i386 winetricks cabextract unzip wget xvfb xauth ca-certificates \
  && rm -rf /var/lib/apt/lists/*

COPY climatena-run /usr/local/bin/climatena-run
RUN chmod 0755 /usr/local/bin/climatena-run \
  && groupadd -g "${GID}" cna \
  && useradd -m -u "${UID}" -g "${GID}" cna

# Wine refuses a prefix owned by another user, so the prefix is made for the run-time user:
# run containers with --user <UID>:<GID> matching the build args.
USER cna
ENV WINEPREFIX=/home/cna/.wine \
  WINEDEBUG=-all \
  WINEDLLOVERRIDES="mscoree,mshtml="
RUN xvfb-run -a wineboot --init && wineserver -w \
  && xvfb-run -a winetricks -q vb6run && wineserver -w \
  && rm -rf /home/cna/.cache/winetricks

WORKDIR /opt/climatena
ENTRYPOINT ["climatena-run"]
