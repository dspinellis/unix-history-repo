set noglob
setenv SHELL /bin/csh
setenv REMOTE /a/staff/bostic/.remote
eval `tset -e"^?" -Q -s -m dialup:?5620 -m network:?xterm`
unset noglob
stty kill "^X" intr "^C" crt -tostop
if ($TERM != "dialup")then
	stty nohang
endif
setenv SHELL /bin/sh
msgs -q -f
echo
/usr/games/fortune -a
