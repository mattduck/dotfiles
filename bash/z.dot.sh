# rupa/z.sh and the data file should both be kept in ~/.z
z_path="$HOME/sh/z.sh" # For me
if [ -f $z_path ]; then
     . $z_path
fi
