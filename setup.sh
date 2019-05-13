#!/bin/bash

DOTFILES=$(ls -a | grep -v "\.$" | grep -v "setup.sh$" | grep -v "\.git$")

for f in ${DOTFILES[@]}
do
    echo $f
done
