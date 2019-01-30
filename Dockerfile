FROM php:7.1-apache

RUN apt-get update
RUN apt-get install -y swi-prolog

ENV TERM xterm-256color

ADD app /var/www/html

