#
# Change vi defaults
#
setenv EXINIT 'set autoindent'
#
# Is this a remote login? 
#
if (`tty | egrep -c "ttyp|ttyq"` == 1) then
    # ----- Don't ask for terminal type if we know it
    set noglob 
    eval `tset -Q -I -s $TERM`
else
    while (1)
	set noglob 
	eval `tset -s -m dialup:?vt100 -m :?$TERM`
	if ($TERM != "unknown") then
	    break
	endif
    end
endif 
stty dec
set mail=/usr/spool/mail/$USER
msgs -p -f
