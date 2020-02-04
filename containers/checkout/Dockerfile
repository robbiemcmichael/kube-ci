FROM alpine:latest

RUN apk add --update \
        git

COPY run.sh /run.sh

WORKDIR /src
CMD ["/run.sh"]
