# docker create --privileged -it --network isolated --ip 172.19.0.3 --name dev --memory=1000m --cpus=1  -v ~/Desktop:/mnt/host:ro mirage:latest
docker create --privileged -it --network isolated --ip 172.19.0.3 --name dev -v ~/Desktop:/mnt/host:ro mirage-bgp:latest
docker start dev
