FROM mysql:8.0

ENV MYSQL_ROOT_PASSWORD taprootofthetree
ENV MYSQL_DATABASE db
ENV MYSQL_USER admin
ENV MYSQL_PASSWORD admin

RUN mkdir /usr/sql
RUN chmod 644 /usr/sql
COPY sql/source.sql /usr/sql/source.sql
COPY sql/run-db.sh /usr/sql/run-db.sh

EXPOSE 3306

CMD ["/usr/sql/run-db.sh"]
