FTP = /home/serv1/ftp/etc
SHELL = /bin/csh

.SUFFIXES:	.txt .hosts .named .rev

oldfiles = named.rev

txtfiles = other.txt offcampus.txt \
	adm.txt aec.txt ais.txt baker.txt bch.txt \
	berkey.txt bessey.txt bim.txt \
	cal.txt canr.txt case.txt cem.txt chm.txt cl.txt \
	cl250.txt cl251.txt cl252.txt cl253.txt \
	clc.txt com.txt commarts.txt cps.txt css.txt cvm.txt \
	educ.txt egr.txt eppley.txt eustace.txt \
	fshn.txt giltner.txt he.txt hfs.txt holmes.txt \
	int-center.txt kedzie.txt kellogg.txt lib.txt lifesci.txt \
	merit.txt mth.txt nat-res.txt \
	nisbet.txt ns.txt nscl.txt olds.txt \
	pa.txt pp.txt prc.txt prl.txt \
	stt.txt student-serv.txt union.txt univ-rel.txt \
	uud.txt wells.txt wilson.txt

hostfiles = other.hosts offcampus.hosts \
	adm.hosts aec.hosts ais.hosts baker.hosts bch.hosts \
	berkey.hosts bessey.hosts bim.hosts \
	cal.hosts canr.hosts case.hosts cem.hosts chm.hosts cl.hosts \
	cl250.hosts cl251.hosts cl252.hosts cl253.hosts \
	clc.hosts com.hosts commarts.hosts cps.hosts css.hosts cvm.hosts \
	educ.hosts egr.hosts eppley.hosts eustace.hosts \
	fshn.hosts giltner.hosts he.hosts hfs.hosts holmes.hosts \
	int-center.hosts kedzie.hosts kellogg.hosts lib.hosts lifesci.hosts \
	merit.hosts mth.hosts nat-res.hosts \
	nisbet.hosts ns.hosts nscl.hosts olds.hosts \
	pa.hosts pp.hosts prc.hosts prl.hosts \
	stt.hosts student-serv.hosts union.hosts univ-rel.hosts \
	uud.hosts wells.hosts wilson.hosts

namedfiles = other.named offcampus.named \
	adm.named aec.named ais.named baker.named bch.named \
	berkey.named bessey.named bim.named \
	cal.named canr.named case.named cem.named chm.named cl.named \
	cl250.named cl251.named cl252.named cl253.named \
	clc.named com.named commarts.named cps.named css.named cvm.named \
	educ.named egr.named eppley.named eustace.named \
	fshn.named giltner.named he.named hfs.named holmes.named \
	int-center.named kedzie.named kellogg.named lib.named lifesci.named \
	merit.named mth.named nat-res.named \
	nisbet.named ns.named nscl.named olds.named \
	pa.named pp.named prc.named prl.named \
	stt.named student-serv.named union.named univ-rel.named \
	uud.named wells.named wilson.named

revfiles = 35.8.rev 35.12.rev

revocfiles = 35.9.rev 35.10.rev \
	35.194.50.rev 35.129.rev 35.151.rev 35.192.224.rev 35.193.16.rev \
	35.194.48.rev 35.194.64.rev 35.194.80.rev 35.194.96.rev \
	35.202.176.rev 35.202.192.rev \
	192.108.188.rev 192.108.189.rev 192.108.190.rev 192.108.191.rev \
	192.188.100.rev 192.188.118.rev 192.231.113.rev

hosts.made:	setup.done hosts named.msu.hosts networks txt.msu \
	ethers.msu named_dump.db
	rm -f $(oldfiles)
	touch hosts.made

lanz:	hosts.msu
	awk -f lanzhosts.awk hosts.msu >lanztemp
	split -1000 lanztemp lanzhost.
	mv lanzhost.aa hosts1.lanz
	mv lanzhost.ab hosts2.lanz
	mv lanzhost.ac hosts3.lanz
	mv lanzhost.ad hosts4.lanz
	mv lanzhost.ae hosts5.lanz
	-mv lanzhost.af hosts6.lanz
	-mv lanzhost.ag hosts7.lanz
	rm lanztemp
	touch lanz

setup.done:	setupserver
	csh setupserver
	touch setup.done

hosts:	hosts.msu hosts.umich hosts.misc hosts.nic
	rm -f hosts
	( echo '#	/etc/hosts from serv1.cl.msu.edu ' `date +%d-%h-%y`; \
	  echo '#'; echo '#>>>>	/etc/hosts.msu'; echo '#'; \
	  cat hosts.msu; \
	  echo '#'; echo '#>>>>	/etc/hosts.umich'; echo '#'; \
	  cat hosts.umich; \
	  echo '#'; echo '#>>>>	/etc/hosts.misc'; echo '#'; \
	  cat hosts.misc; \
	  echo '#'; echo '#>>>>	/etc/hosts.nic'; echo '#'; \
	  cat hosts.nic ) >hosts

mail-hosts:	hosts.msu hosts.lastmail
	rm -f mail-temp hosts.temp
	sort hosts.msu >hosts.temp
	-diff hosts.lastmail hosts.temp >hosts.diff
	( set newver = `head -1 hosts.msu`; \
	  set oldver = `head -1 hosts.lastver`; \
	  echo 'The hosts file for MSU has been updated.  The latest version,' ; \
	  echo 'dated' $$newver[$$#newver], 'may be retrieved from serv1.cl.msu.edu via' ; \
	  echo 'anonymous FTP from file etc/hosts.msu.' ; \
	  echo ' '; \
	  echo 'This version of the hosts file contains the following changes'; \
	  echo 'since version' $$oldver[$$#oldver]':'; \
	  echo ' '; \
	  nawk -f hostmail.awk hosts.diff | sort; \
	  echo ' ' ; \
	  echo 'Doug Nelson' ) >mail-temp
	head -2 mail-hosts >mail-old-temp
	-head -2 mail-temp | cmp -s - mail-old-temp || ( \
	  su net -c "/usr/ucb/mail -s 'Hosts list for MSU.EDU' hosts-update-list" <mail-temp; \
	  mv hosts.temp hosts.lastmail; \
	  mv mail-temp mail-hosts; \
	  head -1 hosts.msu >hosts.lastver )
	rm -f mail-old-temp hosts.diff hosts.temp
	touch hosts.lastmail
	touch mail-hosts

mail-decad-list:	ipad.out
	sort decad.email > mail-decad-list

mail-decad:	$(FTP)/decadministrators mail-decad-list
	rm -f mail-temp decad-temp
	( echo 'The Decnet address assignment list for MSU has been updated.  The latest'; \
	  echo 'version, dated' `date +%d-%h-%y`, 'may be retrieved from serv1.cl.msu.edu via'; \
	  echo 'anonymous FTP from file etc/decadministrators.'; \
	  echo ' '; \
	  echo 'Doug Nelson' ) >mail-temp
	grep '^[0-9]*\.' decad.sublist > decad-temp
	-cmp -s decad-temp decad.lastmail || \
	  cmp -s mail-decad mail-temp || \
	  ( su net -c "/usr/ucb/mail -s 'Decnet Address assignment list for MSU.EDU' decadmin" <mail-temp; \
	  cp decad-temp decad.lastmail )
	mv mail-temp mail-decad
	rm -f decad-temp

ipad.out:	ipad.txt
	( echo -n "Date: "; \
	  date; \
	  sed 's/[ 	]*:[ 	]*/:/g' ipad.txt ) | \
	nawk -f ipad.nawk > ipad.out

mail-ipad-list:	ipad.out
	sort ipad.email > mail-ipad-list

mail-ipad:	$(FTP)/ipadministrators
	rm -f mail-temp
	( echo 'The IP address assignment list for MSU has been updated.  The latest'; \
	  echo 'version, dated' `date +%d-%h-%y`, 'may be retrieved from serv1.cl.msu.edu via'; \
	  echo 'anonymous FTP from file etc/ipadministrators.'; \
	  echo ' '; \
	  echo 'Doug Nelson') >mail-temp
	-cmp -s mail-ipad mail-temp || \
	  su net -c "/usr/ucb/mail -s 'IP Address assignment list for MSU.EDU' 11600ld@msu.edu parnell@msu.edu 12479che@msu.edu" <mail-temp
	mv mail-temp mail-ipad

txt.msu:	$(txtfiles) dup-names
	if (! -z dup-names)  exit 1
	echo ';	/etc/txt.msu from serv1.cl.msu.edu ' `date +%d-%h-%y` >txt.msu
	( sh -c 'for file in $(txtfiles) ; do \
	  echo ";"; \
	  echo ";>>	/etc/$$file"; \
	  echo ";"; \
	  cat $$file; \
	  done' ) >>txt.msu

.txt.hosts:
	rm -f $@
	( echo '#'; echo '#>>	/etc/$@'; echo '#'; \
	  nawk -f txt2host.awk $< ) > $@

hosts.msu:	$(hostfiles) dup-names
	if (! -z dup-names)  exit 1
	rm -f hosts.msu hosts.msu.1
	( echo '#	/etc/hosts.msu from serv1.cl.msu.edu ' `date +%d-%h-%y`; \
	  cat $(hostfiles) ) >hosts.msu.1
	( egrep '^#|[ 	][-0-9A-Za-z]*\.msu\.edu' hosts.msu.1; \
	  echo '#'; echo '#	Local (PC/workstation) systems'; echo '#'; \
	  egrep -v '^#[^>]|[ 	][-0-9A-Za-z]*\.msu\.edu' hosts.msu.1 ) >hosts.msu
	  rm hosts.msu.1

msuhl-telnet:	txt.msu
	awk -f txt2hl.awk txt.msu > msuhl-telnet

$(FTP)/cps.hosts:	cps.hosts
	cp cps.hosts $(FTP)/cps.hosts
	chmod 644 $(FTP)/cps.hosts

$(FTP)/hosts:	hosts
	cp hosts $(FTP)/hosts
	chmod 644 $(FTP)/hosts

$(FTP)/hosts.msu:	hosts.msu
	cp hosts.msu $(FTP)/hosts.msu
	chmod 644 $(FTP)/hosts.msu

$(FTP)/txt.msu:	txt.msu
	cp txt.msu $(FTP)/txt.msu
	chmod 644 $(FTP)/txt.msu

$(FTP)/decadministrators:	decad.header ipad.out
	( echo 'Last revision:  ' `date +%d-%h-%y`; \
	  echo ''; \
	  cat decad.header decad.mgrlist decad.sublist ; \
	  ) >$(FTP)/decadministrators
	chmod 644 $(FTP)/decadministrators

$(FTP)/ipad.ser:	ipad.out
	echo '# Last revision:  ' `date +%d-%h-%y` >$(FTP)/ipad.ser
	echo '#' >>$(FTP)/ipad.ser
	sort -t. -n +0 -1 +1 -2 +2 -3 +3 -4 ipad.ser >>$(FTP)/ipad.ser
	chmod 644 $(FTP)/ipad.ser

$(FTP)/ipadministrators:	ipad.header ipad.out
	( echo 'Last revision:  ' `date +%d-%h-%y`; \
	  echo ''; \
	  cat ipad.header ipad.mgrlist ipad.domlist ipad.sublist ; \
	  ) >$(FTP)/ipadministrators
	chmod 644 $(FTP)/ipadministrators

/usr/tmp/named_dump.db:	dup-names named.bitnet named.boot named.ca \
	named.uucp \
	named.lcc.hosts $(revfiles) $(revocfiles)
	if (! -z dup-names)  exit 1
	rm -f /usr/tmp/named_dump.db
	kill -HUP `cat /etc/named.pid`
	sleep 15
	kill -INT `cat /etc/named.pid`
	if (! -f /usr/tmp/named_dump.db) sleep 10
	@if (! -f /usr/tmp/named_dump.db) sleep 10
	@if (! -f /usr/tmp/named_dump.db) sleep 20
	@if (! -f /usr/tmp/named_dump.db) sleep 20
	@if (! -f /usr/tmp/named_dump.db) sleep 30
	@if (! -f /usr/tmp/named_dump.db) sleep 30
	@if (! -f /usr/tmp/named_dump.db) sleep 60
	@if (! -f /usr/tmp/named_dump.db) sleep 60
	@if (! -f /usr/tmp/named_dump.db) echo 'Dump still not found....'
	sleep 10

named_dump.db:	/usr/tmp/named_dump.db
	head -1 /usr/tmp/named_dump.db >named_dump.db

$(revfiles):	named.soa named.msu.hosts ipad.out dup-names
	if (! -z dup-names)  exit 1
	( set date=`date +%d-%h-%y`; \
	  set serial=`date +%y%m%d`; \
	  sed "s/%file%/$@/" named.soa| \
	  sed "s/%date%/$$date/"| \
	  sed "s/%serial%/$$serial/"; \
	  grep -v ' NOPTR' named.msu.hosts | \
	  ndrev $(@:.rev=) -d msu.edu; \
	  grep ';$(@:.rev=)$$' ipad.rev ) >$@

$(revocfiles):	named.soa offcampus.named merit.named merit2.named ipad.out
	( set date=`date +%d-%h-%y`; \
	  set serial=`date +%y%m%d`; \
	  sed "s/%file%/$@/" named.soa| \
	  sed "s/%date%/$$date/"| \
	  sed "s/%serial%/$$serial/"; \
	  grep -v ' NOPTR' offcampus.named | \
	  ndrev $(@:.rev=) -d msu.edu; \
	  grep -v ' NOPTR' merit.named | \
	  ndrev $(@:.rev=) -d msu.edu; \
	  grep -v ' NOPTR' merit2.named | \
	  ndrev $(@:.rev=) -d merit.edu; \
	  grep ';$(@:.rev=)$$' ipad.rev ) >$@

merit2.named:	merit.txt
	( echo ';'; echo ';>>	/etc/$@'; echo ';'; \
	  nawk -f txt2named.awk origin=merit.edu merit.txt ) > $@

.txt.named:
	( echo ';'; echo ';>>	/etc/$@'; echo ';'; \
	  nawk -f txt2named.awk $< ) > $@

bridges.named:	bridges.txt
	( echo ';'; echo ';>>	/etc/$@'; echo ';'; \
	  sed 's/ *: */:/g' $< | awk -f brtxt2named.awk ) > $@

dup-names:	named.msu.hosts
	rm -f dup-names
	( egrep '	IN A|	IN CNAME' named.msu.hosts | \
	  grep -v ' DUPOK' | \
	  sed 's/\(.*\)	IN .*/*** Duplicate name: \1.msu.edu/' | \
	  sort | \
	  uniq -d ) >dup-names
	cat dup-names
	if (! -z dup-names)  exit 1

named.msu.hosts:	named.soa $(namedfiles) bridges.named ipad.out
	( set date=`date +%d-%h-%y`; \
	  set serial=`date +%y%m%d`; \
	  sed "s/%file%/named.msu.hosts/" named.soa| \
	  sed "s/%date%/$$date/"| \
	  sed "s/%serial%/$$serial/"; \
	  cat $(namedfiles) ipad.named bridges.named ) >named.msu.hosts

named.lcc.hosts:	named.soa lcc.txt
	( set date=`date +%d-%h-%y`; \
	  set serial=`date +%y%m%d`; \
	  sed "s/%file%/named.lcc.hosts/" named.soa| \
	  sed "s/%date%/$$date/"| \
	  sed "s/%serial%/$$serial/"; \
	  nawk -f txt2named.awk origin=lcc.edu lcc.txt ) >named.lcc.hosts

$(FTP)/named.msu.hosts:	named.msu.hosts dup-names
	cp named.msu.hosts $(FTP)/named.msu.hosts
	chmod 644 $(FTP)/named.msu.hosts

$(FTP)/networks:	networks
	cp networks $(FTP)/networks
	chmod 644 $(FTP)/networks
