FROM php:7.0-apache

RUN apt-get update
RUN apt-get install -y swi-prolog

COPY installcomposer.sh /usr/sbin

ENV TERM xterm-256color

ADD app /var/www/html

