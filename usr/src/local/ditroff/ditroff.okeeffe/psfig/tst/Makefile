Psfig= psfig -D../doc/figs
Macs= ../lib/psfig.tmac
Ditroff= /usr/src/local/local/ditroff/ditroff -Tpsc $(Macs) -
#
# A Modified psdit, of course. 
Psdit= psdit

eqn: eqn.tst
	dieqn -Tpsc eqn.tst | $(Psfig) | $(Ditroff)  | psdit > eqn.ps
eqn.out: eqn.tst
	dieqn -Tpsc eqn.tst | $(Psfig) | $(Ditroff)  > eqn.out

test.ps: test.out
	PSLIBDIR=/usr/src/local/local/psfig/tmp/; export PSLIBDIR; $(Psdit) test.out > test.ps
test.out: test.trf
	PSLIBDIR=/usr/src/local/local/psfig/tmp/; export PSLIBDIR; $(Psfig) test.trf | $(Ditroff) -ms $(Macs) -  > test.out
