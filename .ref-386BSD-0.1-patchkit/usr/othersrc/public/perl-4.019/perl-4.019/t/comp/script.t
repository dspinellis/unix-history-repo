#!./perl

# $Header: script.t,v 4.0 91/03/20 01:50:26 lwall Locked $

print "1..3\n";

$x = `./perl -e 'print "ok\n";'`;

if ($x eq "ok\n") {print "ok 1\n";} else {print "not ok 1\n";}

open(try,">Comp.script") || (die "Can't open temp file.");
print try 'print "ok\n";'; print try "\n";
close try;

$x = `./perl Comp.script`;

if ($x eq "ok\n") {print "ok 2\n";} else {print "not ok 2\n";}

$x = `./perl <Comp.script`;

if ($x eq "ok\n") {print "ok 3\n";} else {print "not ok 3\n";}

`/bin/rm -f Comp.script`;
