Ditroff= /usr/local/ditroff -Tpsc
Psfig= ../src/psfig -D../doc/figs
Macs= ../lib/psfig.tmac
#
# A Modified psdit, of course. 
Psdit= /usr/bin/psdit  

test.ps: test.trf
	$(Psfig) test.trf | $(Ditroff) -ms $(Macs) - | $(Psdit) > test.ps
