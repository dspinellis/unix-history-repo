@echo off

sed -e "s/y\.tab\./parse_tab\./" -e "/sed/ s/'/\"/g" < Makefile.in > Makefile
sed -f MISC/MSDOS/djgpp.sed Makefile.in > Makefile

update initscan.c scan.c
