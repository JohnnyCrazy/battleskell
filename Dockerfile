FROM ubuntu:latest

ENV WORKDIR=/code
WORKDIR ${WORKDIR}

RUN apt-get update -y && apt-get install -y libpq-dev

# Normally I would build inside docker, but building a haskell project
# inside docker takes ages --> build externally and copy binaries
COPY ./dist/battleskell .
COPY ./static ./static
COPY ./config ./config

CMD [ "./battleskell" ]
