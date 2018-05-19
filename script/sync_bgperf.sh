set -o errexit

cd
rsync -Cav --exclude=_build --exclude=*.byte /home/opam/host/Bgp4 /home/opam/host/mrt-format .

cp config/bgpd.json /home/opam/Bgp4/src/bgpd/data/bgpd.json

eval $(opam config env)
cd Bgp4/src/bgpd
mirage clean
mirage configure -t unix --net socket
make depend
make

sudo ./bgpd -l *:debug



