# the documentation to the Berkeley network
#
# Eric Schmidt Feb. 1980
#
# users are given netintro, mannnet, refcard
# system managers are given that plus systemmanual
# professors are given all
#
TROFF = /usr/ucb/nettroff
all:	coverpage masters netintro mannet systemmanual refcard	done
coverpage:
	$(TROFF) -ms coverpage.n;
masters:
	tbl masters.n | $(TROFF) -ms
netintro:
	tbl netintro.n | $(TROFF) -ms
mannet:
	cd /usr/man/man1; $(TROFF) -man net*.1
systemmanual:
	$(TROFF) -ms systemmanual.n;
refcard:
	tbl refcard.n | $(TROFF) -ms
done:
	echo DONE
