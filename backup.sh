#!/bin/sh

cd ~/.org
git add .
git commit -m "Org"
git push origin
git push upstream

cd ~/.emacs.d
git add .
git commit -m "Emacs"
git push origin
git push upstream
