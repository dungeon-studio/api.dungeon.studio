FROM alpine:3.7
MAINTAINER Alex Brandt <alunduil@alunduil.com>

RUN apk add --no-cache musl-dev zlib-dev
RUN apk add --no-cache cabal ghc

RUN cabal update

WORKDIR /usr/local/src/api.dungeon.studio

COPY ./api-dungeon-studio.cabal /usr/local/src/api.dungeon.studio/api-dungeon-studio.cabal
RUN cabal install -j --only-dependencies

COPY . /usr/local/src/api.dungeon.studio
RUN cabal build -j --ghc-options="-static -optc-static -optl-static -optl-pthread"

FROM alpine:3.7
MAINTAINER Alex Brandt <alunduil@alunduil.com>

RUN apk add --no-cache ca-certificates

COPY --from=0 /usr/local/src/api.dungeon.studio/dist/build/api-dungeon-studio/api-dungeon-studio /

ENTRYPOINT [ "/api-dungeon-studio" ]
CMD [ "Thanks Heroku!" ]
