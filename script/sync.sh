set -o errexit

cd
rsync -Cav --exclude=_build --exclude=*.byte /mnt/host/Bgp4 /mnt/host/mrt-format /mnt/host/ocaml-lazy-trie .

eval $(opam config env)
cd Bgp4/src/bgpd

mirage clean
mirage configure -t unix --net socket
make depend
make

# ocamlbuild -use-ocamlfind route_injector.byte
# ./route_injector.byte



