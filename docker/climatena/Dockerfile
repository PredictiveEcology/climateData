# ClimateNA (a 32-bit Windows program) under Wine, for running on Linux nodes.
# ClimateNA itself is not in the image: mount its directory at /opt/climatena at run time.
FROM debian:bookworm-slim

ARG UID=1000
ARG GID=1000
ENV DEBIAN_FRONTEND=noninteractive

# winetricks (Debian contrib) installs the Visual Basic 6 runtime ClimateNA needs (MSVBVM60.DLL)
RUN sed -i 's/^Components: main$/Components: main contrib/' /etc/apt/sources.list.d/debian.sources \
  && dpkg --add-architecture i386 \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
    wine wine32:i386 wine64 winetricks cabextract unzip wget xvfb xauth ca-certificates procps \
  && rm -rf /var/lib/apt/lists/*

COPY climatena-run /usr/local/bin/climatena-run
RUN chmod 0755 /usr/local/bin/climatena-run \
  && groupadd -g "${GID}" cna \
  && useradd -m -u "${UID}" -g "${GID}" cna

# Wine refuses a prefix owned by another user, so the prefix is made for the run-time user.
USER cna
ENV WINEPREFIX=/home/cna/.wine \
  WINEDEBUG=-all \
  WINEDLLOVERRIDES="mscoree,mshtml="
RUN xvfb-run -a wineboot --init && wineserver -w \
  && xvfb-run -a winetricks -q vb6run && wineserver -w \
  && rm -rf /home/cna/.cache/winetricks

WORKDIR /opt/climatena
ENTRYPOINT ["climatena-run"]
