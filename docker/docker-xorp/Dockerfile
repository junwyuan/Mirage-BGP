FROM docker-xorp:latest

USER root
WORKDIR /root

RUN apt-get update && apt-get install -y psmisc
RUN mkdir /etc/xorp
COPY config.boot /etc/xorp/config.boot
COPY init.sh /etc/init.d/xorp
RUN chmod +x /etc/init.d/xorp

# RUN apt-get update && apt-get install -yq --no-install-recommends \
#   build-essential \
#   git \
#   scons \
#   libboost-all-dev \
#   libssl-dev \
#   libncurses5-dev \
#   libpcap-dev \
#   traceroute \
#   net-tools \
#   vim \
#   flex \
#   bison \
#   tcpdump \
#   && git clone git://github.com/greearb/xorp.ct.git
# RUN cd xorp.ct/xorp \
#   && scons \
#   && scons install \
#   && groupadd xorp \
#   && usermod -a -G xorp root




