FROM ubuntu:latest

RUN apt-get update; apt-get upgrade -y
RUN apt-get install -y quagga
RUN apt-get install -y vim
RUN apt-get install -y telnet
RUN apt-get install -y net-tools
RUN apt-get install -y tcpdump

COPY config/* /etc/quagga/
RUN chown -R quagga:quagga /etc/quagga/*
RUN chmod 640 /etc/quagga/*


# COPY quagga-init /usr/local/bin/

# ENV PATH "/usr/lib/quagga/:/sbin:/bin:/usr/sbin:/usr/bin"
# ENTRYPOINT ["/bin/bash", "-er", "/usr/local/bin/quagga-init"]
ENTRYPOINT ["/bin/bash"]

# For building dependent images with baked-in config.
ONBUILD ADD config/* /etc/quagga/
ONBUILD RUN chown -R quagga /etc/quagga
