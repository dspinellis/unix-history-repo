set ignoreeof
set mail=(/usr/spool/mail/bill)
switch(`tty`)
	case /dev/ttyd*:
		setenv TERM 3a
		breaksw
endsw
tset -Q
echo "${prompt}users" ; users
set time=15
msgs -f
if (-e $mail) then
	echo "${prompt}mail"
	mail
endif
