#
# (c) Copyright Xerox, 1986
#
#
# *** Add your extra description files and metric master files here ***
DESCFILES=8044.desc troffClassic.desc classic.desc modern.desc terminal.desc \
	titan.desc
IPFILES=../ip-metrics/troffClassic.ip \
	../ip-metrics/classic.ip \
	../ip-metrics/modern.ip \
	../ip-metrics/terminal.ip \
	../ip-metrics/titan.ip

#
DESTBIN=../../bin

fonts-made: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -c troff -d ../../lib $(IPFILES)
	touch fonts-made

list: $(DESCFILES) $(IPFILES)
	cat $(DESCFILES) | $(DESTBIN)/ipmetrics -c toc -d ../../lib $(IPFILES)

# some tastelessness follows
install: fonts-made
	(PATH=$$PATH:../../../../bin; export PATH; install-fonts)

clean:
	rm -f fonts-made
	-cleanup-fonts
