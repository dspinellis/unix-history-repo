: This file is a complement to gzip.prj for Turbo C 2.0 users.
: To construct gzip, first transform all files to msdos CR LF format.
: (Use utilities such as flip or do file transfers in ascii mode.)
: Then invoke this file. Then enter Turbo C, set the the
: compilation model to compact, and the project file to gzip.prj.
: Change the compilation flags if you wish (add SMALL_MEM
: to reduce the memory requirements), and press F9...
: WARNING: you must use the compact or large model in this version.
: To use the large model, add -D__LARGE__ in the tasm command below.
: To get the file wildargs.obj, do:
:
:   pkunpak \tcc\startup wildargs.obj
:
: pkunpak is available in wuarchive.wustl.edu:/mirrors/msdos/starter/pk361.exe
:
: After compiling gzip.exe, do: copy gzip.exe gunzip.exe
:
tasm -ml -DDYN_ALLOC -DSS_NEQ_DS msdos\match, match;
