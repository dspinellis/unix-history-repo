#!./perl

# $Header: sleep.t,v 4.0 91/03/20 01:54:34 lwall Locked $

print "1..1\n";

$x = sleep 2;
if ($x >= 2 && $x <= 10) {print "ok 1\n";} else {print "not ok 1 $x\n";}
