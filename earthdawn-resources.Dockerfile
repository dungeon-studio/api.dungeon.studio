FROM scratch
MAINTAINER Alex Brandt <alunduil@alunduil.com>

EXPOSE 33080

COPY dist/build/collection-server/collection-server /

COPY resources/earthdawn /srv/earthdawn
ENV COLLECTION_SERVER_RESOURCE_PATH /srv

ENTRYPOINT [ "/collection-server" ]
