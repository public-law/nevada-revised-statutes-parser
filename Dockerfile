FROM ubuntu
RUN apt-get update
RUN apt-get install -y wget

COPY scripts/download-and-parse.sh .
COPY .stack-work/install/x86_64-linux-tinfo6/lts-12.13/8.4.3/bin .

CMD ["./download-and-parse.sh"]
