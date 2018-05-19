FROM golang:1.8
WORKDIR /root
RUN go get -u github.com/golang/dep/cmd/dep
RUN go get github.com/osrg/gobgp
RUN cd $GOPATH/src/github.com/osrg/gobgp && dep ensure
