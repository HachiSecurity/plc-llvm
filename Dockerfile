FROM haskell:8.10.4 AS builder
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
RUN ls -la /src/compiler/ && stack build --system-ghc && \
    stack install --system-ghc --local-bin-path=/ 

FROM debian:buster-slim
RUN apt-get update && \
    apt-get install -y gnupg lsb-release wget software-properties-common libsodium-dev && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 12
COPY --from=builder /plc-llvm /plc-llvm
ENTRYPOINT [ "/plc-llvm" ]
CMD []
