case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting makedir (with variable substitutions)"
$spitshell >makedir <<!GROK!THIS!
$startsh
# $Header: makedir.SH,v 4.3 85/05/01 11:42:31 lwall Exp $
# 
# $Log:	makedir.SH,v $
# Revision 4.3  85/05/01  11:42:31  lwall
# Baseline for release with 4.3bsd.
# 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

case \$# in
  0)
    $echo "makedir pathname filenameflag"
    exit 1
    ;;
esac

: guarantee one slash before 1st component
case \$1 in
  /*) ;;
  *)  set ./\$1 \$2 ;;
esac

: strip last component if it is to be a filename
case X\$2 in
  X1) set \`$echo \$1 | $sed 's:\(.*\)/[^/]*\$:\1:'\` ;;
  *)  set \$1 ;;
esac

: return reasonable status if nothing to be created
if $test -d "\$1" ; then
    exit 0
fi

list=''
while true ; do
    case \$1 in
    */*)
	list="\$1 \$list"
	set \`echo \$1 | $sed 's:\(.*\)/:\1 :'\`
	;;
    *)
	break
	;;
    esac
done

set \$list

for dir do
    $mkdir \$dir >/dev/null 2>&1
done
!GROK!THIS!
$eunicefix makedir
chmod 755 makedir
