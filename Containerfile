FROM alpine:latest AS build

RUN apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl \
        zlib zlib-dev zlib-static ncurses-static gmp-static

RUN cd / && git clone https://github.com/brauner/move-mount-beneath.git && \
    cd move-mount-beneath && gcc -static move-mount.c -o move-mount && mv move-mount /move-mount

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh && \
    "$HOME/.ghcup/bin/ghcup" install ghc --set && \
    "$HOME/.ghcup/bin/ghcup" install cabal --set

COPY CHANGELOG.md /hald-build/CHANGELOG.md
COPY hald.cabal /hald-build/hald.cabal
COPY LICENSE /hald-build/LICENSE
COPY src /hald-build/src

RUN cd /hald-build && \
    source "$HOME/.ghcup/env" && \
    cabal update && \
    cabal install --installdir="/" \
        --enable-optimization=2 --install-method=copy --overwrite-policy=always && \
    strip -s /hald

FROM alpine:latest AS target

COPY --from=build /hald /app/hald
COPY --from=build /move-mount /app/move-mount
COPY utils/boot /app/boot
COPY utils/tools /app/tools
