MAKEFLAGS = k

FTP = /home/serv1/ftp/etc

anonymous = $(FTP)/cps.hosts $(FTP)/decadministrators \
	$(FTP)/hosts $(FTP)/hosts.msu $(FTP)/ipad.ser $(FTP)/ipadministrators \
	$(FTP)/named.msu.hosts $(FTP)/networks $(FTP)/txt.msu

files = .cshrc .login .exrc Makefile \
	bridges.named bridges.txt \
	decad.header decad.mgrlist decad.sublist \
	dup-names ethers.msu host2txt.awk hostmail.awk hosts \
	hosts.make hosts.misc hosts.msu hosts.nic hosts.umich \
	in.named \
	ipad.awk ipad.domlist ipad.email ipad.header ipad.mgrlist \
	ipad.named ipad.out ipad.rev ipad.ser ipad.sublist ipad.txt \
	lcc.txt merit2.named microlab.txt named-xfer \
	named.bitnet named.boot named.ca \
	named.lcc.hosts named.msu.hosts named.soa named.uucp \
	nd2host.awk networks setupserver txt.msu \
	txt2host.awk txt2named.awk \
	$(txtfiles) $(hostfiles) $(namedfiles) \
	$(revfiles) $(revocfiles)

serv1:	/etc/aliases hosts.made \
	mail-decad mail-hosts mail-ipad mail-ipad-list $(anonymous)
	touch serv1

all:	syssun serv1 serv2 convex

#serv2:	hosts.made
#	tar cf - $(files) | (rsh serv2 'cd /home/serv1/net && tar xvfpB -') && \
#	rsh -n serv2 'cd /home/serv1/net && make -f hosts.make' && \
#	touch serv2
#	echo '=====  END serv2  ====='
#
serv2:	hosts.made
	rdist && \
	rsh -n serv2 'cd /home/serv1/net && make -f hosts.make' && \
	touch serv2
	echo '=====  END serv2  ====='

serv3:	hosts.made
	tar cf - $(files) | (rsh serv3 'cd /home/serv1/net && tar xvfpB -') && \
	rsh -n serv3 'cd /home/serv1/net && make -f hosts.make' && \
	touch serv3
	echo '=====  END serv3  ====='

oldserv1:	hosts.made
	tar cf - $(files) | (rsh oldserv1 'cd /home/serv1/net && tar xvfpB -') && \
	rsh -n oldserv1 'cd /home/serv1/net && make -f hosts.make' && \
	touch oldserv1
	echo '=====  END oldserv1  ====='

syssun:	hosts.made
	tar cf - $(files) | (rsh syssun 'cd /home/serv1/net && tar xvfpB -') && \
	rsh -n syssun 'cd /home/serv1/net && make -f hosts.make' && \
	touch syssun
	echo '=====  END syssun  ====='

convex: hosts
	ftp -n -v convex <.cvx.ftpin && \
	touch convex

um:	msuhl-telnet
	ftp -n -v um.cc.umich.edu <.um.ftpin && \
	touch um

include hosts.make

hosts.umich:	umich.named_dump
	nslookup < umich_nddump.in
	( echo '#	hosts.umich from serv1.cl.msu.edu ' `date +%d-%h-%y`; \
	  echo '#'; \
	  echo '#	Contains partial listings of umich.edu and merit.edu domains'; \
	  echo '#'; \
	  awk -f nddump2host.awk umich.named_dump ) | \
	  sort -t. -n +0 -1 +1 -2 +2 -3 +3 -4 >hosts.umich

hosts.txt.Z: nic
	rm -f hosts.txt-z
	ftp -n -v nic.ddn.mil <nic.ftpin && \
	mv hosts.txt-z hosts.txt.Z

networks.txt:	hosts.txt.Z
	zcat hosts.txt.Z | grep ^NET > networks.txt

hosts.nic:	hosts.txt.Z
	zcat hosts.txt.Z | awk -f nictxt2host.awk | tr A-Z a-z > hosts.nic

networks.nic:	networks.txt
	awk -f nicnet.awk networks.txt | tr A-Z a-z > networks.nic

networks:	networks.msu networks.nic
	( echo '# Last revision:  ' `date +%d-%h-%y`; \
	  echo '#'; \
	  cat networks.msu networks.nic) >networks

/etc/aliases: /etc/aliases.header ipad.out
	cat /etc/aliases.header ipad.aliases > /etc/aliases
