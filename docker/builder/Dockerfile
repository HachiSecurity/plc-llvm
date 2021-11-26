FROM alpine:edge

# install GHC
RUN apk update --no-cache && \
    apk add --no-cache gcc g++ bash make binutils-gold \
        perl curl file automake autoconf dpkg \
        fakeroot findutils shadow

ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
RUN curl --fail -o /bin/ghcup \
        'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
        && chmod +x /bin/ghcup

# An alpine build of GHC is available, so this is fairly quick
# ENV GHC_VERSION=${{GHC_VERSION}}
ARG GHC_VERSION=9.2.1
RUN ghcup upgrade --target /bin/ghcup
RUN ghcup install ghc ${GHC_VERSION} && ghcup set ghc ${GHC_VERSION}

ENV PATH="/usr/local/.ghcup/bin:$PATH"

# install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# install LLVM
RUN apk add --no-cache git llvm12-dev gmp-dev ncurses-dev

RUN ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6
