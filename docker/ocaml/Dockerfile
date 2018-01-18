FROM ocaml/opam:ubuntu

RUN opam init
RUN opam install mirage -y
RUN opam install cstruct -y
RUN opam install ppx_cstruct -y
RUN opam install lwt -y
RUN oapm install yojson -y

RUN sudo apt-get update; sudo apt-get upgrade -y
RUN sudo apt-get install -y vim
RUN sudo apt-get install -y net-tools
RUN sudo apt-get install -y tcpdump

# ENV PATH "/usr/lib/quagga/:/sbin:/bin:/usr/sbin:/usr/bin"
# ENTRYPOINT ["/bin/bash", "-er", "/usr/local/bin/quagga-init"]
ENTRYPOINT ["/bin/bash"]

