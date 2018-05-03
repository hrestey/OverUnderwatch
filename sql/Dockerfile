FROM mysql:8.0

ENV MYSQL_ROOT_PASSWORD taprootofthetree
ENV MYSQL_DATABASE db
ENV MYSQL_USER admin
ENV MYSQL_PASSWORD admin

RUN mkdir /usr/sql
RUN chmod 644 /usr/sql
COPY source.sql /usr/sql/source.sql
COPY run-db.sh /usr/sql/run-db.sh
