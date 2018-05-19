set -o errexit

cd
rsync -Cav --exclude=_build --exclude=*.byte /mnt/host/Bgp4 /mnt/host/mrt-format .

eval $(opam config env)
cd Bgp4/src/collision
mirage clean
mirage configure -t unix --net socket
make depend

make



