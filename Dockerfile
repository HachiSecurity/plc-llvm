FROM ghcr.io/hachisecurity/plutus-stack:main AS builder
WORKDIR /src
# First, we install things that are unlikely to change frequently
RUN apt-get update && \
    apt-get install -y lsb-release wget software-properties-common libsodium-dev && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 12
# we copy only the files needed to compile the dependencies first
# so that code changes don't invalidate the Docker cache
COPY ["stack*.yaml", "lts*.yaml", "/src/"]
COPY ["compiler/package.yaml", "/src/compiler/package.yaml"]
RUN stack build --system-ghc --only-dependencies
# then we copy the rest and compile that
COPY ["compiler/", "/src/compiler/"]
RUN stack build --system-ghc && \
    stack install --system-ghc --local-bin-path=/

FROM debian:buster-slim
RUN apt-get update && \
    apt-get install -y gnupg lsb-release wget software-properties-common pkg-config libsodium-dev libgmp-dev && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 12
COPY ["compiler/rts/rts.c", "compiler/rts/rts.h", "/"]
COPY compiler/rts/tiny_sha3 /tiny_sha3
COPY --from=builder /plc-llvm /plc-llvm
ENV PATH="/usr/lib/llvm-12/bin:${PATH}"
ENV LIBRARY_PATH="/usr/lib/x86_64-linux-gnu:${LIBRARY_PATH}"
ENTRYPOINT [ "/plc-llvm", "--rts", "/rts.c" ]
CMD []
