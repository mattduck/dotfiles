export GOPATH=$HOME/golang
mkdir -p "$GOPATH"
,path-add ~/golang/bin

# Generally I prob won't use <= 1.7 now, but I'll keep this just in case.
export GO15VENDOREXPERIMENT=1