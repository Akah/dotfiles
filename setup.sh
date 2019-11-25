#!/bin/bash
apt install emacs numix-gtk-theme papirus-icon-theme
git clone https://github.com/Akah/dotfiles.git dotfiles
cd dotfiles
cp * ~/
rm -rf dotfiles
reboot
