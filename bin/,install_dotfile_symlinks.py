#!/usr/bin/env python
""" 
Usage: ,install_dotfile_symlinks.py

Creates ~/.foo symlinks for all foo.symlink files in $DOTFILES. Any existing
dotfiles are backed up to ~/dotfiles_backup/$(date).
"""

import sys
import os
import subprocess
import fnmatch
from datetime import datetime

HOME_PATH = os.path.expanduser("~")
BACKUP_PATH = os.path.realpath(os.path.join(HOME_PATH, "dotfiles_backup"))
DOTFILES_PATH = os.path.realpath(os.environ["DOTFILES"])

def main(argv):
    if not os.path.exists(BACKUP_PATH):
        subprocess.Popen(["mkdir", "-pv", BACKUP_PATH])
    date = datetime.now().strftime("%Y-%m-%d-%H-%M-%s")
    this_backup_path = os.path.join(BACKUP_PATH, date)
    subprocess.Popen(["mkdir", "-pv", this_backup_path])

    backup_made = False

    symlink_paths = []
    for root, dirnames, filenames in os.walk(DOTFILES_PATH, topdown=True):
        # You can mutate os.walk values to prune the search
        if ".git" in dirnames:
            dirnames.remove(".git")

        for dirname in fnmatch.filter(dirnames, "*.symlink"):
            symlink_paths.append([root, dirname])
        for fname in fnmatch.filter(filenames, "*.symlink"):
            symlink_paths.append([root, fname])

    for root, symlink_name in symlink_paths:
        home_name = "." + symlink_name.split(".symlink")[0]
        home_path = os.path.join(HOME_PATH, home_name)
        dotfiles_path = os.path.join(root, symlink_name)

        if os.path.exists(home_path) and not os.path.islink(home_path):
            subprocess.Popen(["mv", "-v", home_path, this_backup_path])
            backup_made = True

        # Use -n so that directories are not entered, stops the directories
        # getting a recursive symlink that points to itself.
        # Use -f to overwrite existing symlinks, so can just run the script
        # again if the location of the config directory changes.
        subprocess.Popen(["ln", "-vnsf", dotfiles_path, home_path])
            
    if not backup_made:
        print "No files to backup, removing timestamp dir"
        subprocess.Popen(["rmdir", this_backup_path])

if __name__ == "__main__":
    sys.exit(main(sys.argv))
