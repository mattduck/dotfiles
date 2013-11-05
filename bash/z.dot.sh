# rupa/z.sh and the data file should both be kept in ~/.z
export _Z_DATA="$z_path/.z" # For z program
z_path="$HOME/.z" # For me
if [[ -d $z_path ]]; then
    "$z_path/z.sh"
fi
