: run this script through /bin/sh

# for a non standard base directory, set MANDIR

# set MANOPTS in config/CONFIG.make -- may be one of:
# -bsd42  man<n>/<file>
# -bsd44  cat<n>/thing.0
# -ros    man<n>/<file>			 -- using /etc/install
# -sys5   <a|p|u>_man/man<n>/<file>
# -aix    <a|p|u>_man/man<n>/<file>
# -local  manl/<base>.l
# -l      man<n>/<base>.<n>l
# -hpux   hpux

BINDIR="@(BINDIR)"
ETCDIR="@(ETCDIR)"
INCDIR="@(INCDIR)"
LOGDIR="@(LOGDIR)"
SBINDIR="@(SBINDIR)"
MANDIR="@(MANDIR)"

M=BSD42

if test ! -d ${MANDIR}
then
    echo "inst-man: ${MANDIR} non-existant directory" 1>&2
    exit 0
fi

for A in $*
do
    case $A in
	-bsd42)	M=BSD42
		;;

	-bsd44)	M=BSD44
		;;

	-ros)	M=ROS
		;;

	-local)	M=LOCAL
		;;

	-l)	M=L
		;;

	-sys5)	M=SYS5
		;;

	-aix)	M=AIX
		;;

	-hpux)  M=HPUX
		;;

	-*)	echo "inst-man: $A unknown" 1>&2
		exit 1
		;;

	*)	if test ! -f  $A
		then
		    exit 0
		fi
		F=`basename $A`
		E=`echo $F | sed -e "s%^.*\.\([1-8]\).*%\1%"`

		X=/tmp/$F
		rm -f $X
		echo '.ds BD @(BINDIR)'   > $X
		echo '.ds ED @(ETCDIR)'  >> $X
		echo '.ds ID @(INCDIR)'  >> $X
		echo '.ds LD @(LOGDIR)'  >> $X
		echo '.ds SD @(SBINDIR)' >> $X
		cat $A >> $X

		case $M in
		    BSD42)
			echo install -m 0644 -c $X ${MANDIR}man$E/$F
			install -m 0644 -c $X ${MANDIR}man$E/$F
			;;

		    BSD44)
			echo "nroff -man $X > ${MANDIR}cat$E/$F" | \
			sed -e 's%\.\([1-8]\)[1-8cn]*$%.0%' | \
			sh -ve
			;;

		    LOCAL)
			(cd /tmp ; \
			    echo $F | \
			    sed -e "s%.*%install -m 0644 -c & ${MANDIR}manl/&%" | \
			    sed -e 's%\.[1-8cn]*$%.l%' | \
			    sh -ve)
			;;

		    L)
			(cd /tmp ; \
			    echo "install -m 0644 -c $F ${MANDIR}man$E/$F" | \
			    sed -e 's%\.\([1-8]\)[1-8cn]*$%.\1l%' | \
			    sh -ve)
			;;

		    SYS5|AIX)
			case $E in
			    3)      D=p_man     ;;
			    5)	    D=p_man E=4 ;;
			    8)	    D=a_man E=1 ;;
			    *)	    D=u_man     ;;
			esac
			echo /etc/install -m 0644 -f ${MANDIR}$D/man$E $X
			/etc/install -m 0644 -f ${MANDIR}$D/man$E $X
			case $D in
			    a_man)
				F=`basename $A .8c`.1m
				echo mv ${MANDIR}$D/man$E/$A \
				     ${MANDIR}$D/man$E/$F
				mv ${MANDIR}$D/man$E/$A ${MANDIR}$D/man$E/$F
				;;

			    p_man)
				if [ "$E" = "4" ]; then
				    F=`basename $A .5`.4
				    echo mv ${MANDIR}$D/man$E/$A \
				         ${MANDIR}$D/man$E/$F
				    mv ${MANDIR}$D/man$E/$A \
					 ${MANDIR}$D/man$E/$F
				fi
				;;
			esac
			;;

		    HPUX)
			case $E in
			    5)  E=4 ;;
			    8)  E=1m ;;
			esac
			echo /etc/install -m 0644 -f ${MANDIR}man$E $X
			/etc/install -m 0644 -f ${MANDIR}man$E $X
			case $E in
			    4)  F=`basename $A .5`.4
				echo mv ${MANDIR}man$E/$A ${MANDIR}man$E/$F
				mv ${MANDIR}man$E/$A ${MANDIR}man$E/$F
				;;

			    1m)	F=`basename $A .8c`.1m
				echo mv ${MANDIR}man$E/$A ${MANDIR}man$E/$F
				mv ${MANDIR}man$E/$A ${MANDIR}man$E/$F
				;;
			esac
			;;
  
		    ROS)
			echo /etc/install -m 0644 -c $X ${MANDIR}man$E/$F
			/etc/install -m 0644 -c $X ${MANDIR}man$E/$F
			;;

		    *)  echo "inst-man: mode botch" 1>&2
			exit 1
			;;
		esac
		rm -f $X
		;;
    esac
done

exit 0
