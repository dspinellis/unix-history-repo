#! /bin/sh
##  $Revision: 1.3 $
##  Perform a test(1) and execute a command string if the test is true;
##  otherwise exit 0.  Usage:
##	testit <test> <command>
##  On some systems (those with a /bin/sh from BSD4.2), the following
##  line in a Makefile will always fail:
##	if [ ! -f x ] ; then echo No x -- stop. ; exit 1 ; else exit 0 ; fi
case $# in
2)
    ;;
*)
    echo "Can't perform test:  wrong number of arguments." 1>&2
    exit 1
    ;;
esac
TEST="$1"
COMMAND="$2"

if eval "test ${TEST}" ; then
    eval "${COMMAND}"
    exit $?
fi
exit 0
