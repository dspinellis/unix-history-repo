# A sample Makefile for psfig stuff

P	= paper
Draft	=

# Change these to reflect your installation
Ditroff	= /usr/src/local/local/ditroff/ditroff
Eqn	= dieqn -Tpsc
Tbl	= tbl -Tpsc

Macros	= -mns
MyMacs	= paper.tmac ../lib/psfig.tmac

# This should be the *modified* psdit
Psdit	= psdit

Psfig	= psfig $(Draft) -Dfigs
Docs	= $P.ps eqn.ps pro.ps

whattodo:
	@echo "Make what?"

pages: $P.ps
	getpages $P.ps

$P.pr: $P.ps
	psprint -e -r $P.ps

$P.ps: $P.pfg
	$(Ditroff) -Tpsc $(Macros) $(MyMacs) $P.pfg > $P.dtrf
	$(Psdit) $P.dtrf > $P.ps

$P.tek: $P.pfg
	$(Ditroff) -Ttc $(Macros) $(MyMacs) $P.pfg > $P.tc
	tc $P.tc
	
$P.pfg: $P.eqn
	$(Psfig) <$P.eqn >$P.pfg

$P.eqn: $P.tbl
	$(Eqn) -Tpsc <$P.tbl >$P.eqn

$P.tbl: $P.pic
	$(Tbl) <$P.pic >$P.tbl

$P.pic: $P.ms
	pic <$P.ms >$P.pic
	
count: $P.ms
	deroff $P.ms | wc -w

spell: #spell
#spell: $P.ms
	spell $P.ms >'#'spell

tidy:
	-rm -f *.BAK *.CKP core a.out
clean: tidy
	-rm -f *.tc *.dtrf *.pfg *.eqn *.tbl *.pic '#'* $P.ps.* $P.ps *.out
