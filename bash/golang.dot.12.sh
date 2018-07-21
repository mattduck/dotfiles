export GOPATH=$HOME/golang
mkdir -p "$GOPATH"
,path ~/golang/bin

# Generally I prob won't use <= 1.7 now, but I'll keep this just in case.
export GO15VENDOREXPERIMENT=1

alias golinux="GOOS=linux GOARCH=amd64 go"
alias godarwin="GOOS=darwin GOARCH=amd64 go"
alias goraspbian="GOOS=linux GOARCH=arm GOARM=7 go"
