docker create --network isolated --ip $1 -p $2:$2 -e LISTEN=:$2 -e TALK=$3 --name $4 sproxy:latest 
docker start $4
