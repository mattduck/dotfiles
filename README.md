# Install

    $ git clone https://github.com/mattduck/dotfiles.git ~/dotfiles
    $ cd ~/dotfiles
    $ git submodule update --init --recursive


## Environment

    $ source ~/dotfiles/activate.sh

To always run your shell using the dotfiles environment, add the above line to
`~/.bashrc`. The activate script will:

- Export $DOTFILES as the path to this dotfiles directory.

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


## Quick install for dev work

Quick way for me to get started on ubuntu-like dev VMs. **Check the script that
gets downloaded before piping into shell**.

`sh <(curl https://raw.githubusercontent.com/mattduck/dotfiles/master/dev-quickstart/dev-quickstart.sh)`


# Issues

### OS X - $PATH issues

Tmuxs always runs as a login shell, which means /etc/profile gets read. On some
OS X releases, this will run a utility called `path_helper`, which will always
prepend a set of directories to your $PATH /after/ `.bashrc` has run.

To disable this, you can reset your $PATH as part of `.bashrc`, and run the
dotfiles setup afterwards:

``` bash
# .bashrc
if [ -f /etc/profile ]; then
    PATH=""
    source /etc/profile
fi
source ~/dotfiles/activate.sh
```

See https://superuser.com/questions/544989/does-tmux-sort-the-path-variable/583502#583502 for more info.


# Thanks

- Some cool vim / shell / tmux ideas taken from [Nic West](https://github.com/nicwest/.dotfiles).

- The \*.symlink and \*.sh features were taken from [Zach Holman's
  dotfiles](https://github.com/holman/dotfiles).

- I liked [Brandon Rhodes'](https://github.com/brandon-rhodes/homedir) idea of
  prefixing your custom commands with a comma. Also stole some functions.

- My old Emacs organisation was inspired by [Hans Engel](https://github.com/hans/dotfiles).
