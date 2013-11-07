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

    $ bin/,install-dotfile-symlinks

- Create symlinks to ~/.file for __\*.symlink__.

- Backup any existing (non-link) files to ~/dotfiles_backup/$(date).


## Third party stuff

- Vim plugins are installed with `:BundleInstall`. 

- Emacs plugins are installed on startup.

- Submodules are used for other code that belongs in the dotfiles repo:

    - Plugins that can't be installed by eg. Vundle / ELPA.

    - Config-related stuff, eg. for setting up solarized schemes on
      various programs.

- Non-config related programs are installed as part of the ,install-deps scripts.
  Programs that need to be sourced are installed to *~/sh*. 


# Thanks

- The \*.symlink and \*.sh features were taken from [Zach Holman's
  dotfiles](https://github.com/holman/dotfiles).

- I liked [Brandon Rhodes' idea](https://github.com/brandon-rhodes/homedir) of
  prefixing your custom commands with a comma. 

- Emacs organisation inspired by [Hans Engel](https://github.com/hans/dotfiles).
