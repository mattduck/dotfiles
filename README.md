# Install

    $ git clone https://github.com/mattduck.dotfiles.git ~/dotfiles
    $ cd ~/dotfiles
    $ git submodule update --init --recursive


## Environment

    $ source ~/dotfiles/activate.sh

Add the above line to your bashrc or equivalent. This will:

- Add dotfiles/bin to $PATH. 

- First source __\*.dot.XX.sh__ in numeric order.

- Then source __\*.dot.sh__. 


## Symlinks

    $ bin/,install_dotfile_symlinks.py

- Create symlinks to ~/.file for __\*.symlink__.

- Backup any existing (non-link) files to ~/dotfiles_backup/$(date).


# Thanks

- The \*.symlink and \*.sh features were taken from [Zach Holman's
  dotfiles](https://github.com/holman/dotfiles).

- I liked [Brandon Rhodes' idea](https://github.com/brandon-rhodes/homedir) of
  prefixing your custom commands with a comma. 

- Emacs organisation inspired by [Hans Engel](https://github.com/hans/dotfiles).
