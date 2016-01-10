#!/bin/sh
#
# Quick setup script for when using development VMs.
#
# Usage: download and run this script

set -e

PLAYBOOK_FILE=ubuntu-playbook.yml
URL_FOR_THIS_SCRIPT=https://raw.githubusercontent.com/mattduck/dotfiles/master/dev-quickstart/dev-quickstart.sh
URL_FOR_PLAYBOOK=https://raw.githubusercontent.com/mattduck/dotfiles/master/dev-quickstart/$PLAYBOOK_FILE

# dependencies
sudo apt-get install python-pip wget
sudo pip install ansible

if [ ! -f "$PLAYBOOK_FILE" ]; then
    wget "$URL_FOR_PLAYBOOK"
fi

ANSIBLE_INVENTORY="-i localhost,"
ansible-playbook "$PLAYBOOK_FILE" $ANSIBLE_INVENTORY -vvvv --diff --connection=local

echo "Done! You just need to BundleInstall in vim".
