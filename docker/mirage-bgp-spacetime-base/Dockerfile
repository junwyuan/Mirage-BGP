FROM ubuntu:16.04

RUN apt-get update \
&& apt-get install opam -y

RUN opam init -y \
&& opam switch 4.05.0+spacetime

RUN eval `opam config env` \ 
&& opam depext conf-m4.1 \
&& opam install yojson -y \
&& opam install mirage -y \
&& opam install lwt  -y \
&& opam install cstruct -y \
&& opam install ppx_cstruct -y

RUN git clone https://github.com/jimyuan1995/Mirage-BGP.git
RUN git clone https://github.com/jimyuan1995/mrt-format.git \
&& cd Mirage-BGP/src/common \
&& ln -s ../../../mrt-format/lib

RUN cd Mirage-BGP/src/bgpd \
&& eval `opam config env` \ 
&& mirage configure -t unix --net socket \
&& make depend 

RUN cd Mirage-BGP/src/bgpd \
&& eval `opam config env` \
&& make

