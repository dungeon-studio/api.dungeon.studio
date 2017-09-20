FROM scratch
MAINTAINER Alex Brandt <alunduil@alunduil.com>

EXPOSE 33080

COPY dist/build/collection-server/collection-server /

ENTRYPOINT [ "/collection-server" ]
