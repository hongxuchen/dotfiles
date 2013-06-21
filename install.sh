#!/bin/bash

function link_file {
    original="${PWD}/$1"
    softlink="${HOME}/${1/_/.}"
    #backup original file if exists and not a symbolic link
    if [ -e "${softlink}" ] && [ ! -L "${softlink}" ]; then
        mv $softlink $softlink.df.bak
        #remove original link, otherwise a recursive link would be generated
    elif [ -e "${softlink}" ] && [ -L "${softlink}" ]; then
        rm $softlink
    fi
    ln -sf ${original} ${softlink}
}


function unlink_file {
    original="${PWD}/$1"
    softlink="${HOME}/${1/_/.}"
    #if current rc file is symbolic link and original file exists
    if [ -e "${softlink}.df.bak" ] && [ -L "${softlink}" ]; then
        unlink ${softlink}
        mv $softlink.df.bak $softlink
    fi
}

# never use it with submodule since vundle needs to manage itself
function init_vim {
    if ! [ -d ~/.vim/bundle/vundle ]; then
        git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
        vim -c 'BundleInstall' -c 'q'
    fi
}

if [ "$1" = "restore" ]; then
    for index in _*
    do
        #echo "unlink_file $index"
        unlink_file $index
    done
    exit
else
    for index in _*
    do
        echo "link_file $index"
        link_file $index
    done
fi

init_vim

# find ~ -maxdepth 1 -xtype l -delete
# doxymacs
# emacs
