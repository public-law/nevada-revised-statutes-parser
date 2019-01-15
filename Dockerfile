FROM ubuntu
RUN apt-get update
RUN apt-get install -y wget

WORKDIR /app
COPY app/download-and-parse.sh /app
COPY .stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin/nrs-parser-exe /app
CMD ["download-and-parse.sh"]
