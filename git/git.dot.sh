function ,git-cd() {
    # cd to top level of git directory
    cd "$(git rev-parse --show-toplevel)"
}
