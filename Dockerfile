FROM ubuntu
WORKDIR /app
COPY . /app
CMD ["app/download-and-parse.sh"]
