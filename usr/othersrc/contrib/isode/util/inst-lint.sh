: run this script through /bin/sh

M=BSD42	O= L=/usr/lib/lint

for A in $*
do
    case $A in
	-bsd42)
		M=BSD42
		;;

	-bsd44)
		M=BSD44
		;;

	-sys5)	M=SYS5
		exit 0
		if test ! -f ${L}/lint1; then
		    L=/usr/lib
		    if test ! -f ${L}/lint1; then
			echo "inst-lint: unable to find lint1" 1>&2
			exit 0
		    fi
		fi
		;;

	-mips)	M=SYS5
		;;

	-ros)	M=ROS
		;;

	-*)	O="$O $A"
		;;

	*)	case $M in
		    BSD42)  echo /lib/cpp -C -Dlint $O $A \| \
				/usr/lib/lint/lint1 -v \> $A.ln
			    /lib/cpp -C -Dlint $O $A | \
				/usr/lib/lint/lint1 -v > $A.ln
			    ;;

		    BSD44)  echo lint -Clint $O $A
			    lint -Clint $O $A
			    echo mv llib-lint.ln $A.ln
			    mv llib-lint.ln $A.ln
			    ;;

		    SYS5)   echo /bin/cc -E -C -Dlint $O $A \| \
				${L}/lint1 -v \> $A.ln
			    /bin/cc -E -C -Dlint $O $A | \
				${L}/lint1 -v > $A.ln
			    ;;

		    MIPS)   echo lint -o /usr/lib/cmplrs/cc/lint/$A.ln $O $A
			    lint -o /usr/lib/cmplrs/cc/lint/$A.ln $O $A
			    ;;

		    ROS)    echo lint -c -v $O $A
			    lint -c -v $O $A
			    F="`basename $A`"
			    if [ $F != $A ]; then
				echo mv $F.ln $A.ln
				mv $F.ln $A.ln
			    fi
			    ;;

		    *)	    echo "inst-lint: mode botch" 1>&2
			    exit 1
			    ;;
		esac
		;;
    esac
done

exit 0
