set history=20
set cdpath=(/usr/src/new /usr/src/sys /usr/src/cmd /usr/man)
alias ec \
    'echo \!-1:q > /tmp/$$ ; vi /tmp/$$ ; typein `cat /tmp/$$` ; rm /tmp/$$'
alias cd 'set old=$cwd; chdir \!*'
alias back 'set back=$old; set old=$cwd; cd $back; unset back; dirs'
