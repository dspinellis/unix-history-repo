#
# (c) Copyright Xerox, 1986
#
#
# *** Add your extra description files and metric master files here ***
DESCFILES=8044.desc troffClassic.desc modern.desc
IPFILES=troffClassic.ip modern.ip

#
DESTBIN=../bin

fonts-made: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -d ../lib $(IPFILES)
	touch fonts-made

# some tastelessness follows
install: fonts-made
	(PATH=$$PATH:../../../../bin; export PATH; install-fonts)

clean:
	rm -f fonts-made
	-cleanup-fonts
