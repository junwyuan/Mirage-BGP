FROM haproxy:1.7-alpine
MAINTAINER Tecnativa <info@tecnativa.com>

COPY haproxy.cfg /usr/local/etc/haproxy/haproxy.cfg
EXPOSE 8001
ENV LISTEN=:8001 TALK=172.17.0.2:8001

# Metadata
ARG VCS_REF
ARG BUILD_DATE
LABEL org.label-schema.schema-version="1.0" \
      org.label-schema.vendor=Tecnativa \
      org.label-schema.license=Apache-2.0 \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/Tecnativa/docker-tcp-proxy"
