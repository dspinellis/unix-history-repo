From cbosgd!ucbvax!usenet Mon Oct 13 05:39:17 1986
Received: by beno.CSS.GOV (5.54/5.17)
	id AA01253; Mon, 13 Oct 86 05:39:12 EDT
Received: from cbosgd.UUCP by seismo.CSS.GOV (5.54/1.14)
	id AA03513; Mon, 13 Oct 86 05:39:11 EDT
Received: by cbosgd.ATT.COM (4.12/UUCP-Project/rel-1.0/06-28-86)
	id AA08778; Mon, 13 Oct 86 03:44:14 edt
Received: by ucbvax.Berkeley.EDU (5.53/1.17)
	id AA15536; Sun, 12 Oct 86 23:51:25 PDT
Date: Sun, 12 Oct 86 23:51:25 PDT
From: ucbvax!usenet (USENET News Administration)
Message-Id: <8610130651.AA15536@ucbvax.Berkeley.EDU>
To: cbosgd!backbone
Subject: a handy awk script for netnews log reports
Status: R

Since Mark didn't have a copy of this, I will assume that most of the
rest of you don't either, and send it along. The comments should be
explanation enough... If not, ask me.

	Erik E. Fair	ucbvax!fair	fair@ucbarpa.berkeley.edu
-------------------------------------------------------------------------------
#  USAGE: awk -f report_awk /usr/lib/news/log
#
#  AWK script which eats netnews log files and produces a summary of USENET
#  traffic and errors over the period of time that the log was collected.
#
#  August 31, 1986
#
#  Erik E. Fair <dual!fair>
#  Original Author, May 22, 1984
#
#  Brad Eacker <onyx!brad>
#  Modified to simplify the record processing and to sort the output.
#
#  Erik E. Fair <dual!fair>
#  Modifed to provide information about control messages.
#
#  Erik E. Fair <dual!fair>
#  Bug in system name extraction fixed. It was assumed that the forth field
#  (system name) always had a dot. local is one that doesn't. Some others
#  (including 2.9 sites) don't either.
#
#  Earl Wallace <pesnta!earlw>
#  The "sent" field was changed from $5 to $6 in 2.10.2 (beta)
#  named "newstats" and called with no arguments.
#
#  Erik E. Fair <dual!fair>
#  Remove support for 2.10.1, revise for 2.10.2 to provide information
#  about junked articles, garbled articles, and bad newsgroups
#
#  Erik E. Fair <ucbvax!fair>
#  Minor bug fix to bad newsgroup reporting, also now counting ``old''
#  articles as junked, with counter for number that are `old'.
#
#  Erik E. Fair <ucbvax!fair>
#  Fix up the domain & local hosts support
#
#  Erik E. Fair <ucbvax!fair>
#  Fix up the counting of gatewayed material, add counting of "linecount"
#  problems. Additional cleanup to make things faster.
#
BEGIN{
#
#	this is the prefix that your site uses in hostnames to identify your
#	hosts (e.g. ucbarpa, ucbvax, su-score, mit-mc, mit-ai)
#	You will probably want to change (or add to) the following line
#
	lprefix = "ucb";
	lplen = length(lprefix);
#
#	If you do bi-directional USENET gatewaying (e.g. mailing list
#	to newsgroup where the material flows both ways freely), this
#	should be the name in the sys file that you use to mail stuff
#	to the mailing lists.
#
	pseudo = "internet";
	rptname = "(GATEWAY)";
#
#	Top level domain names and what network they represent
#	(for use in counting stuff that is gatewayed)
#
	domains["ARPA"] = rptname;
	domains["arpa"] = rptname;
	domains["EDU"] = rptname;
	domains["edu"] = rptname;
	domains["GOV"] = rptname;
	domains["gov"] = rptname;
	domains["COM"] = rptname;
	domains["com"] = rptname;
	domains["MIL"] = rptname;
	domains["mil"] = rptname;
	domains["ORG"] = rptname;
	domains["org"] = rptname;
	domains["NET"] = rptname;
	domains["net"] = rptname;
	domains["UK"] = rptname;
	domains["uk"] = rptname;
	domains["DEC"] = rptname;
	domains["dec"] = rptname;
	domains["CSNET"] = rptname;
	domains["csnet"] = rptname;
	domains["BITNET"] = rptname;
	domains["bitnet"] = rptname;
	domains["MAILNET"] = rptname;
	domains["mailnet"] = rptname;
	domains["UUCP"] = rptname;
	domains["uucp"] = rptname;
	domains["OZ"] = rptname;
	domains["oz"] = rptname;
	domains["AU"] = rptname;
	domains["au"] = rptname;
#
#	tilde chosen because it is ASCII 126 (don't change this)
#
	invalid = "~~~~~~";
#
	accept[invalid]   = 0;
	reject[invalid]   = 0;
	xmited[invalid]   = 0;
	control[invalid]  = 0;
	junked[invalid]   = 0;
	neighbor[invalid] = 0;
	badgrp  = 0;
	garbled = 0;
	lcount  = 0;
	canfail = 0;
	candup  = 0;
	insfail = 0;
	old     = 0;
}
#
#	Skip some things that we won't bother with
#
/^$/				{ next }
$5 == "from"			{ next }
$5 == "make"			{ next }
$5 == "Cancelling"		{ next }
#
#	Or that we just count
#
$5 == "Inbound"			{ garbled++; next }
$6 == "cancel"			{ canfail++; next }
$6 == "Cancelled"		{ candup++; next }
$6 == "install"			{ insfail++; next }
#
#	Articles sent to remote systems (this is what 2.10.2 (beta) says)
#
$6 == "sent"	{
	for(j = 8; j <= NF; j++) {
		comma = index( $(j), ",");
		if (comma != 0) $(j) = substr( $(j), 1, (comma - 1));
		if ($(j) == pseudo) $(j) = rptname;
		else neighbor[$(j)] = 1;
		xmited[$(j)]++;
	}
	next;
}
#
#	Articles sent to remote systems (this is what 2.11 says)
#
$5 == "sent"	{
	for(j = 7; j <= NF; j++) {
		comma = index( $(j), ",");
		if (comma != 0) $(j) = substr( $(j), 1, (comma - 1));
		if ($(j) == pseudo) $(j) = rptname;
		else neighbor[$(j)] = 1;
		xmited[$(j)]++;
	}
	next;
}
#
#	Get the name of the system that did this,
#	taking into account that not everyone believes in domains.
#
{
#	if we get a route addr (we shouldn't, but...), take the last one
#
	nhosts = split($4, hosts, "@");
	hostname = hosts[nhosts];
#
#	get the root domain name, and the hostname
#
	ndoms = split(hostname, doms, ".");
	domain = doms[ndoms];
	sys = doms[1];
#
#	check for local system, and if not that, then internet sites.
#	special case the network name replacement of specific host names,
#	such that the network name is there only on a `local' posting
#	(which is really gatewaying in disguise)
#
	if ($5 == "posted") {
		prefix = substr(sys, 1, lplen);
		if (prefix == lprefix) {
			sys = "local";
		} else {
			dom = domains[domain];
			if (dom) sys = dom;
		}
	}
}
#  
#	Duplicates & receiveds/posted & control messages
# 
$5 == "posted" || $5 == "received" {
	accept[sys]++;
	if ($5 == "received") neighbor[sys] = 1;
	nng = split($8, ngl, ",");
	for(i = 1; i <= nng; i++) {
		dot = index(ngl[i], ".");
		if (dot) ng = substr(ngl[i], 1, (dot - 1));
		else ng = ngl[i];
		if (ng) newsgcnt[ng]++;
	}
	next;
}
$5 == "Duplicate"	{ reject[hostname]++; next }
$6 == "valid"		{ junked[sys]++; next }
$6 == "too"		{ junked[sys]++; old++; next }
$5 == "Unknown"		{
	x = length($7) - 2;
	ng = substr($7, 2, x);
	badng[ng]++;
	badgrp++;
	next;
}
#
#	articles who actual line count differs from the Line: header count
#
$5 == "linecount"	{
	expect = $7;
# awk does very strange things with non-numeric characters in numbers
	comma = index(expect, ",");
	if (comma != 0) expect = substr(expect, 1, (comma - 1));
	got = $9;
	diff = got - expect;
	lcount++;
	alc_host[sys] = 1;
	neighbor[sys] = 1;
	if (diff < 0) {
		diff = 0 - diff;
		a_nshort[sys]++;
		a_short[sys] += diff;
		if (a_smax[sys] < diff) a_smax[sys] = diff;
	} else {
		a_nlong[sys]++;
		a_long[sys] += diff;
		if (a_lmax[sys] < diff) a_lmax[sys] = diff;
	}
	next;
}
#
#	articles who actual line count is Zero
#
$7 == "linecount"	{
	lcount++;
	a_zero[sys]++;
	reject[sys]++;
	next;
}
#
#	Control messages
#
$5 == "Ctl"	{
	ctot++;
	control[sys]++;
	ctlcnt[$(10)]++;
	next;
}
#
#	Print anything we didn't recognize, it's probably an error message.
#	For the submitted report to USENET, do sed -e '1,/^$/d' file | inews
#	so that this cruft doesn't get out the door.
#
{
	print;
}
#
#	Summarize and print the report
#
END{
#	special processing for Duplicates, because we can't tell if
#	they came from a netnews neighbor or from the gatewaying
#	activities until we have processed the entire log.
#
	for( hostname in reject ) {
#
#	get the root domain name, and the hostname
#
		ndoms = split(hostname, doms, ".");
		domain = doms[ndoms];
		sys = doms[1];
		if (! neighbor[sys]) {
			prefix = substr(sys, 1, lplen);
			if (prefix == lprefix) {
				sys = "local";
			} else {
				dom = domains[domain];
				if (dom) sys = dom;
			}
		}
		i = reject[hostname];
		reject[hostname] = 0;
		reject[sys] += i;
	}

	rtot = 0;
	for( i in reject ) {
		if (reject[i] > 0) {
			list[i] = 1;
			rtot += reject[i];
		}
	}

	atot = 0;
	for( i in accept ) {
		list[i] = 1;
		atot += accept[i];
	}

	xtot = 0;
	for( i in xmited ) {
		list[i] = 1;
		xtot += xmited[i];
	}

	ctot = 0;
	for( i in control ) {
		list[i] = 1;
		ctot += control[i];
	}

	jtot = 0;
	for( i in junked ) {
		list[i] = 1;
		jtot += junked[i];
	}
#
# ctot is part of rtot, so we don't add it in to the grand total.
#
	totarticles = atot + rtot;
	if (totarticles == 0) totarticles = 1;

	printf("\nSystem       \tAccept\tReject\tJunked\tXmit to\tControl\t%% total\t%% rejct\n");
	for( ; ; ) {
# selection sort
		i = invalid;
		for( j in list ) {
			if ( list[j] > 0 && j < i ) i = j;
		}
		if ( i == invalid ) break;
		list[i] = 0;
#
#	control & junked are counted under accept.
#
		sitetot = accept[i] + reject[i];
		if (sitetot == 0) sitetot = 1;
		articles[i] = sitetot;
#
# What an 'orrible printf spec
#
		printf("%-14s\t%6d\t%6d\t%6d\t%7d\t%7d\t%6d%%\t%6d%%\n", i, accept[i], reject[i], junked[i], xmited[i], control[i], (sitetot * 100) / totarticles, (reject[i] * 100) / sitetot);
#
	}
	printf("\nTOTALS        \t%6d\t%6d\t%6d\t%7d\t%7d\t%6d%%\t%6d%%\n", atot, rtot, jtot, xtot, ctot, 100, (rtot * 100) / totarticles);
	printf("\nTotal Articles processed %d", totarticles);
	if (old)	printf(", old %d", old);
	if (garbled)	printf(", garbled %d", garbled);
	if (insfail)	printf(", uninstallable %d", insfail);
	printf("\n");

	if (ctot) {
		printf("\nControl	Invocations\n");
		for( i in ctlcnt ) {
			if (i == "cancel") {
				printf("%-12s %6d", i, ctlcnt[i]);
				if (canfail) printf(", %d failed", canfail);
				if (candup) printf(", %d duplicate", candup);
				printf("\n");
			} else {
				printf("%-12s %6d\n", i, ctlcnt[i]);
			}
		}
	}

	if (lcount) {
		printf("\nReceived Article Length Problems\n");
		printf("System          Zero Short  Smax  Savg  Long  Lmax  Lavg Total %% Tot\n");
		for( i in alc_host ) {
			nlong = a_nlong[i];
			nshort = a_nshort[i];
			if (nlong == 0) nlong = 1;
			if (nshort == 0) nshort = 1;
			lavg = a_long[i] / nlong;
			savg = a_short[i] / nshort;
			sitetot = (a_zero[i] + a_nshort[i] + a_nlong[i]);
			printf("%-14s %5d %5d %5d %5d %5d %5d %5d %5d %4d%%\n", i, a_zero[i], a_nshort[i], a_smax[i], savg, a_nlong[i], a_lmax[i], lavg, sitetot, (sitetot * 100) / articles[i]);
		}
	}

	if (atot) {
		printf("\nNetnews Categories Received\n");
		l = 0;
		for( i in newsgcnt ) {
			if (l < length(i)) l = length(i);
		}
		fmt = sprintf("%%-%ds %%6d\n", l);
		for( ; ; ) {
# selection sort
			max = 0;
			for( j in newsgcnt ) {
				if (newsgcnt[j] > max) {
					i = j;
					max = newsgcnt[j];
				}
			}
			if (max == 0) break;
			printf(fmt, i, newsgcnt[i]);
			newsgcnt[i] = 0;
		}
	}

	if (badgrp) {
		printf("\nBad Newsgroups Received\n");
		l = 0;
		for( i in badng ) {
			if (l < length(i)) l = length(i);
		}
		fmt = sprintf("%%-%ds %%5d\n", l);
		for( ; ; ) {
# selection sort
			i = invalid;
			for( j in badng ) {
				if (badng[j] > 0 && j < i) i = j;
			}
			if (i == invalid) break;
			printf(fmt, i, badng[i]);
			badng[i] = 0;
		}
	}
}

