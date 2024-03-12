FROM haskell:9.2.5-slim

RUN cabal v2-update

# Add bare assignment files
ADD *.cabal     /template/
ADD app         /template/app
ADD src         /template/src
ADD test        /template/test

# Set workdir to template assignment
WORKDIR /template

RUN cabal v2-build

RUN echo 'cabal v2-run shapes-test' > /test.sh
RUn chmod +x /test.sh

ENTRYPOINT bash