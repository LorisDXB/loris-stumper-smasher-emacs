#!/bin/bash

source_file="emacs.d"
destination_folder="$HOME"
destination_file=".emacs.d"
backup_file=".emacs"

if [ -e "$source_file" ]; then
    mv "$source_file" "$destination_file"
    cp -r "$destination_file" "$destination_folder"
    echo "emacs.d renamed and copied to ~/.emacs.d"
else
    echo "Error: File emacs.d not found."
    exit 1
fi

if [ -e "$backup_file" ]; then
    mv "$backup_file" ".emacs.bak"
    echo ".emacs renamed to .emacs.bak"
fi

echo "Script execution complete."
