progs = cppp cross csgen deal delam delamc edscript farb fset gcomp\
   gset i-psort i-split i-trfil i-xref labels lam ll loadmap parens queens\
   roffcmds rsg shuffile tablc tablw trim worm 

ibin = /usr/new/lib/icon/5.9.0

what:
		@echo "what do you want to make?"

All:		$(progs)
		rm -f ../../ibin/*
		mv $(progs) ../../ibin

cppp:		cppp.icn
		$(ibin)/icont -u -s cppp.icn

cross:		cross.icn
		$(ibin)/icont -u -s cross.icn

csgen:		csgen.icn
		$(ibin)/icont -u -s csgen.icn

deal:		deal.icn
		$(ibin)/icont -u -s deal.icn

delam:		delam.icn
		$(ibin)/icont -u -s delam.icn

delamc:		delamc.icn
		$(ibin)/icont -u -s delamc.icn

edscript:	edscript.icn
		$(ibin)/icont -u -s edscript.icn

farb:		farb.icn
		$(ibin)/icont -u -s -Sc200 -Ss7000 -St2290 farb.icn

fset:		fset.icn
		$(ibin)/icont -u -s fset.icn

gcomp:		gcomp.icn
		$(ibin)/icont -u -s gcomp.icn

gset:		gset.icn
		$(ibin)/icont -u -s gset.icn

i-psort:	i-psort.icn
		$(ibin)/icont -u -s i-psort.icn

i-split:	i-split.icn
		$(ibin)/icont -u -s i-split.icn

i-trfil:	i-trfil.icn
		$(ibin)/icont -u -s i-trfil.icn

i-xref:		i-xref.icn
		$(ibin)/icont -u -s i-xref.icn

labels:		labels.icn
		$(ibin)/icont -u -s labels.icn

lam:		lam.icn
		$(ibin)/icont -u -s lam.icn

ll:		ll.icn
		$(ibin)/icont -u -s ll.icn

loadmap:	loadmap.icn
		$(ibin)/icont -u -s loadmap.icn

parens:		parens.icn
		$(ibin)/icont -u -s parens.icn

queens:		queens.icn
		$(ibin)/icont -u -s queens.icn

roffcmds:	roffcmds.icn
		$(ibin)/icont -u -s roffcmds.icn

rsg:		rsg.icn
		$(ibin)/icont -u -s rsg.icn

shuffile:	shuffile.icn
		$(ibin)/icont -u -s shuffile.icn

tablc:		tablc.icn
		$(ibin)/icont -u -s tablc.icn

tablw:		tablw.icn
		$(ibin)/icont -u -s tablw.icn

trim:		trim.icn
		$(ibin)/icont -u -s trim.icn

worm:		worm.icn ../../ilib/gpack.u1
		$(ibin)/icont -u -s worm.icn ../../ilib/gpack.u1

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm `gcomp Makefile *.icn *.gen`
