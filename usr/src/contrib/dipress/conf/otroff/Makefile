#
# (c) Copyright Xerox, 1986
#
#
# *** Add your extra description files and metric master files here ***
DESCFILES=8044.desc troffClassic.desc 
IPFILES=../ip-metrics/troffClassic.ip

#
DESTBIN=../../bin

fonts-made: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -c otroff -d ../../lib $(IPFILES)
	touch fonts-made

list: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -c toc -d ../../lib $(IPFILES)

# some tastelessness follows
install: fonts-made
	(PATH=$$PATH:../../../../bin; export PATH; install-fonts)

clean:
	rm -f fonts-made
	-cleanup-fonts
