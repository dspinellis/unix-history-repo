here is the fast 'grep/egrep' package sent to comp.sources and u. c. berkeley.
included are the prerequisite routines developed by henry spencer of
univ. of toronto -- these are also part of the comp.sources archive.

i've already updated spencer's care package to incorporate three fixes
which have appeared in the same forum.

the makefiles are configured for bsd 4.3 and sys5 unix.
they assume that the spencer regexp() is not already in a system library --
read the makefile comments if this is not the case.

for stock 4.3 sites, apply the diff 'diff.egrep.y.bsd' to the existing
source in /usr/src/usr.bin/egrep.y and re-make.  this adds full support
for the -i option.  the procedure is then:

	make
	sh eg.shell	# amusement
	make install
	
ames!jaw
