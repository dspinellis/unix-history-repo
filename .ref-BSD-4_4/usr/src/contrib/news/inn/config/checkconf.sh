#! /bin/sh
##  $Revision: 1.2 $
##
##  Check the config.data file to make sure there is a tab on every
##  non-comment line.
case $# in
*)
    ;;
0)
    echo "Can't check config.data:  wrong number of arguments." 1>&2
esac

##  Get the lines we care about.
CONF="$1"
shift
sed -e '/^#/d' -e '/^$/d' "${CONF}" >config.x$$

##  Find the lines with tabs.
grep '	' <config.x$$ >config.y$$

##  If the lines we care about don't all have tabs, complain.
diff config.x$$ config.y$$ >config.z$$
if [ -s config.z$$ ] ; then
    echo The following config parameters have no tabs:
    ##  Work around lack of "-n" in some sed's.
    grep '<' config.z$$ | sed -e 's/</    /'
    rm config.[xyz]$$
    exit 1
fi

##  Everything's cool.
rm config.[xyz]$$
exit 0
