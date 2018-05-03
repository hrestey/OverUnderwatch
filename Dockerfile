FROM php:7.0-apache

RUN apt-get update
RUN apt-get install -y swi-prolog

ENV TERM xterm-256color

ADD src /var/www/html

