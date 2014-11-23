#!/bin/bash

failed_items=""
function install_package() {
echo EXECUTING: brew install $1 $2
brew install $1 $2
[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.
}
brew tap staticfloat/julia
install_package ack ''
install_package astyle ''
install_package autoconf ''
install_package automake ''
install_package bazaar ''
install_package boost ''
install_package cloog ''
install_package cmake ''
install_package colordiff ''
install_package colormake ''
install_package colorsvn ''
install_package ctags ''
install_package doxymacs ''
install_package fish ''
install_package git-extras ''
install_package git-imerge ''
install_package gmp ''
install_package isl ''
install_package jenkins ''
install_package libmpc ''
install_package lzlib ''
install_package makedepend ''
install_package mercurial ''
install_package mpfr ''
install_package pcre ''
install_package pkg-config ''
install_package poco ''
install_package the_silver_searcher ''
install_package vim ''
install_package xz ''
install_package zsh-history-substring-search ''
install_package zsh-syntax-highlighting ''
[ ! -z $failed_items ] && echo The following items were failed to install: && echo $failed_items
