: run this script through /bin/sh

echo ''
echo ''
echo ''
echo 'Welcome to the PSI White Pages Pilot Project'
echo ''

C=fred D=fred E=
if [ \( "x$USER" = "xxwp" -o "x$LOGNAME" = "xxwp" \) -a -x @(BINDIR)xwp ]; then
    TERM="xterm"
fi
if [ "x$TERM" = "xxterm" ]; then
    if [ -x @(BINDIR)xwp ]; then
	D="xwp -D"
    fi

    X="`who am i | sed -e 's%.*(\(.*\))%\1%'`"
    if [ -l "$X" -ge 16 ]; then
	X=
    fi
    echo 'If you want X window access, please enter your DISPLAY name,'
    if [ "x$X" != "x" ]; then
	if [ "x$DISPLAY" = "x" ]; then
	    DISPLAY="$X:0.0"
	fi
	echo 'otherwise, if you do not wish to use X, enter "none"'
	echo ''
	echo -n 'DISPLAY (default='$DISPLAY')='
	read X x
	case "x$X" in
	    x)	    export DISPLAY
		    C="$D"
		    ;;

	    xnone)  ;;

	    *)	    DISPLAY="$X"
		    export DISPLAY
		    C="$D"
		    ;;
	esac
    else
	echo 'e.g., "192.33.4.21:0.0"'
	echo ''
	echo -n 'DISPLAY='

	read X x
	case "x$X" in
	    x|xnone) ;;

	    *)	    DISPLAY="$X"
		    export DISPLAY
		    C="$D"
		    ;;
	esac
    fi
    echo ''
fi

if [ "$C" = "fred" ]; then
    echo 'Try   "help" for a list of commands'
    echo '     "whois" for information on how to find people'
    echo '    "manual" for detailed documentation'
    echo '    "report" to send a report to the white pages manager'
    echo ''
    echo 'To find out about participating organizations, try'
    echo '    "whois -org *"'
    echo ''
    echo '  accessing service, please wait...'
else
    echo 'To find out about participating organizations,'
    echo '    click on "US"'
fi
echo ''

DISHDRAFT=/tmp/fred$$ export DISHDRAFT

exec /bin/csh -ic "@(BINDIR)$C -a -l -r"
