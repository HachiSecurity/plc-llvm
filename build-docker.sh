set -e

# 1. We create an image that provides a suitable environment for us to build
#    our program in.
docker build -f ./docker/builder/Dockerfile -t plc-llvm-builder:head ./docker/builder/

# 2. Run the builder image with the code mounted into the container and use it
#    to compile our program.
docker run --rm -it -v $HOME/.stack:/root/.stack -v $PWD:/src -w /src plc-llvm-builder:head sh /src/docker/builder-entry.sh
