" When vimrc is edited, reload it.
autocmd! bufwritepost vimrc source $HOME/.vimrc

set autoread " Autoread external changes.

" Fix crontab's "temp file must be edited in place"
set backupskip=/tmp/*,/private/tmp/*
