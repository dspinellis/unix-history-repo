date
set nfutil	= /usr/spool/notes/.utilities
echo " ---- Sending Updates to CONVEX Computer Corporation"
/usr/bin/nfxmit -dconvex -a "net.*" "fa.*" "mod.*" -f ${nfutil}/short.names
date
