FROM fpco/stack-build:lts-24.65 AS build
WORKDIR /src
COPY stack.yaml stack.yaml.lock nrs-parser.cabal Setup.hs ./
COPY src src
COPY app app
COPY test test
RUN stack build --copy-bins --local-bin-path /out --ghc-options="-O2"

FROM ubuntu:24.04
RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates wget libgmp10 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /out/nrs-parser-exe /usr/local/bin/nrs-parser-exe
COPY scripts/download-and-parse.sh /download-and-parse.sh
WORKDIR /
CMD ["./download-and-parse.sh"]
