FROM postgres:17.0-bullseye

ENV POSTGRES_DB=user
ENV POSTGRES_USER=admin
ENV POSTGRES_PASSWORD=admin

COPY ./init.sql /docker-entrypoint-initdb.d/

EXPOSE 5432