all clean:;	@echo "You must configure MH first -- see READ-ME for details"

distribution unconfig:	links READ-ME CHANGES

READ-ME:	conf/mh-gen.8
		nroff -man conf/mh-gen.8 > read-me.tmp
		mv read-me.tmp $@

CHANGES:	papers/changes/mh-changes.ms
		nroff -ms papers/changes/mh-changes.ms > changes.tmp
		mv changes.tmp $@

links:;		-rm -f support/pop/mmdfII/pop/lock.c
		-rm -f support/bboards/mmdfII/bboards/lock.c
		-rm -f zotnet/tws/tws.h
		-rm -f uip/pshsbr.c
		ln zotnet/mts/lock.c support/pop/mmdfII/pop/lock.c
		ln zotnet/mts/lock.c support/bboards/mmdfII/bboards/lock.c
		ln zotnet/tws.h zotnet/tws/tws.h
		ln uip/popsbr.c uip/pshsbr.c
