#
# (c) Copyright Xerox, 1986
#
#
# *** Add your extra description files and metric master files here ***
DESCFILES=8044.desc troffClassic.desc
IPFILES=troffClassic.ip

#
DESTBIN=../bin

fonts-made: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -T -d ../lib $(IPFILES)
	touch fonts-made

install: fonts-made
	install-fonts

clean:
	rm -f fonts-made
	-cleanup-fonts
