# Install

    $ git clone https://github.com/mattduck/dotfiles.git ~/dotfiles
    $ cd ~/dotfiles
    $ git submodule update --init --recursive


## Environment

    $ source ~/dotfiles/activate.sh

The activate script will:

- Export $DOTFILES as the path to the directory of the activation script.

- Add $DOTFILES/**/bin to $PATH.

- First source $DOTFILES/**.dot.xx.sh in numeric order.

- Then source $DOTFILES/**.dot.sh.


## Symlinks

    $ bin/,dotfiles-install

- Create symlinks to ~/.file for $DOTFILES/**.symlink.

- Backup any existing (non-link) files to ~/dotfiles_backup/$(date).


## Third party stuff that lives in dotfiles

- Vim plugins are installed with `:BundleInstall`. 

- Emacs plugins are automatically installed on startup.

- Submodules are used when we're just pulling in other content with git
  eg. the initial Vundle install, some config bits relating to Solarized
  schemes.


# Thanks

- The \*.symlink and \*.sh features were taken from [Zach Holman's
  dotfiles](https://github.com/holman/dotfiles).

- I liked [Brandon Rhodes' idea](https://github.com/brandon-rhodes/homedir) of
  prefixing your custom commands with a comma. 

- My old Emacs organisation was inspired by [Hans Engel](https://github.com/hans/dotfiles).
