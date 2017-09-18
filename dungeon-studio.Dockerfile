FROM scratch
MAINTAINER Alex Brandt <alunduil@alunduil.com>

EXPOSE 45753

COPY dist/build/dungeon-studio/dungeon-studio /dungeon-studio

ENTRYPOINT [ "/dungeon-studio" ]
