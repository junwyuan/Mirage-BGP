set -o errexit

cd ~/Desktop/Bgp4/src/bgpd

opam switch 4.05.0+spacetime
eval `opam config env`

mirage clean
mirage configure -t macosx --net socket
make depend

make