#To: Paul A Vixie <vixie>
#Subject: Re: send me your tools 
#In-Reply-To: Your message of "Sun, 25 Apr 1993 21:02:34 PDT."
#             <9304260402.AA13384@cognition.pa.dec.com> 
#Date: Wed, 28 Apr 1993 08:48:06 -0400
#From: David Barr <barr@pop.psu.edu>
#
#In message <9304260402.AA13384@cognition.pa.dec.com>, Paul A Vixie writes:
#>wednesday.
#
#Here it is.  :-)
#
#------ dnswalk.1.5.shar

#! /bin/sh
# This is a shell archive.  Remove anything before this line, then unpack
# it by saving it into a file and typing "sh file".  To overwrite existing
# files, type "sh file -c".  You can also feed this as standard input via
# unshar, or by typing "sh <file", e.g..  If this archive is complete, you
# will see the following message at the end:
#		"End of shell archive."
# Contents:  README TIPS TODO dnswalk do-dnswalk
# Wrapped by barr@bosnia on Wed Apr 28 08:46:36 1993
PATH=/bin:/usr/bin:/usr/ucb ; export PATH
if test -f 'README' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'README'\"
else
echo shar: Extracting \"'README'\" \(11267 characters\)
sed "s/^X//" >'README' <<'END_OF_FILE'
X		This is dnswalk 1.5 - Arpil 1993
X
X	Author: David Barr <barr@pop.psu.edu>
X
XINTRO
X
Xdnswalk is a perl script that given a domain address, will do a zone
Xtransfer of the DNS database and perform various checks on it to make
Xsure that things are accurate.
X
Xdnswalk requires perl and dig.  (Tested under perl-4.036 and dig 2.0)
XIf you do not have these tools, get them.  (perl is assumed to be in
X/usr/local/bin, edit the first line of dnswalk if it is not)
X
XThey can be found by anonymous ftp at:
Xftp.uu.net:/networking/ip/dns/dig.2.0.tar.Z
Xftp.uu.net:/systems/gnu/perl-4.035.tar.Z
X
X	I'm planning on rewriting dnswalk so it does not rely on dig.
XLook for newer versions soon.
X
X	dnswalk is not for the faint of heart.  It should NOT be
Xused without a firm knowledge of the DNS RFC's.  If you use this
Xtool for cracking or otherwise evil purposes, the author hereby
Xconsiders you a slime-ball.  See the end of this README file for
Xa list of good reading material.
X
X	dnswalk is not the be-all end-all tool for checking the
Xintegrity of your DNS database.  I would suggest it be used after
Xusing 'doc', written by Steve Hotz (hotz@isi.edu) and Paul Mockapetris
X(pvm@isi.edu).  It can be found via FTP from:
Xftp.uu.net:/networking/ip/dns/doc.2.0.tar.Z
X
X	dnswalk is not a replacement for doc.  This was written to
Xcheck individual database entries, while 'doc' ensures that the overall
Xdatabase structure and authority records are consistent.  dnswalk may
Xnot even function correctly if authority records are missing or
Xincorrect.
X
X	This program may be freely distributed, as long as this notice
Xand documentation are distributed with the program.  This program is
Xreleased as-is, with no warranty expressed or implied.  Some assembly
Xrequired, contents may settle during shipment.  This program can be
Xfound at ftp.pop.psu.edu:/pub/src/networking/dnswalk.1.5.tar.Z,
Xand probably also in ftp.uu.net:/networking/ip/dns.
X
XUSAGE
X
X	Invoke dnswalk as follows:
X
Xdnswalk [-cfradm] domain [> logfile]
X
X	dnswalk tends to produce lots of output, so I'd suggest
Xredirecting this into a file of your choice.  I debated using doc's
Xstrategy of automatically putting it in a logfile, but decided not
Xto.  (The author reserves the right to change his mind)  For small,
Xmostly-correct domains it is pretty manageable, however.  For larger
Xdomains, use the included 'do-dnswalk' script as a guide.
X
XOptions:
X	-c	Enable checking of "CNAME and other data" rule.
X		BIND does not allow this, so if your nameservers
X		all run BIND this can be safely ignored.  dnswalk
X		will run faster without it.
X	-f	Force a zone transfer from an authoritative nameserver.
X		dnswalk normally will look in its saved 'axfr' file
X		for each domain and use that. (if it exists)
X	-r	Recursively descend sub-domains of the specified
X		domain.  Use with caution and care.
X	-a	Turn on warning of duplicate A records.  (see below)
X	-d	Some debugging.  (Use only if redirecting stdout)
X	-m	Perform checks only if the zone has been modified since
X		the previous run.
X	-F	perform "fascist" checking.  When checking an A record,
X		compare the PTR name for each IP address with the forward
X		name and report mismatches.  (see below)  I recommend
X		you try this option at least once to see what sorts of
X		errors pop up - you might be surprised!.
X	-l	Perform "lame delegation" checking.  For every NS record,
X		check to see that the listed host is indeed returning
X		authoritative answers for this domain.  Inspiration for
X		this comes from the great guys at U-M.
X
X
XThe domain name specified on the command line MUST end with a '.'.
XYou can specify a forward domain, such as "dnswalk pop.psu.edu."
Xor a reverse domain, such as "dnswalk 155.118.128.in-addr.arpa."
X
XWhat dnswalk will do is if it is checking "pop.psu.edu.", it will
Xdo a zone transfer of the data from the authoritative nameserver and
Xput it in the file "edu/psu/pop/axfr".  (relative to the current directory)
XIf dnswalk is run again, it will use this file instead of asking the
Xnameserver again.  You can override this with the '-f' switch.  It
Xwill also do a zone transfer again later if the serial number in the
Xfile is less than what is returned from the server.
X
XAs a result, if you use the recursive option, you will get a directory
Xtree corresponding exactly to the DNS hierarchy.  Again, I must stress
Xthat you use the recursive option with care, and the author calls you
Xa slime-ball again if you use this for evil purposes.
X
X
XWHAT DNSWALK CHECKS FOR..
X
X*  all PTR records that look like a full IP address point back to a
Xforward name.  In other words, "3.155.118.128.in-addr.arpa."
Xwill be checked, but "155.118.128.in-addr.arpa." will NOT be checked.
X(For those people who give their subnets a name)  Addresses ending
Xin .0 are also NOT checked.
X[ reports error as "X PTR Y: unknown host" ]
X
X*  PTR records are listed as IP addresses in forward name.  In other
Xwords if 4.3.2.1.in-addr.arpa. points to "foo.org", but "foo.org"
Xdoesn't have "1.2.3.4" listed in its forward A records then it will
X be an error.
X[ reports error as "X PTR Y: forward matching A record not found" ]
X
X*  PTR records do not point to a CNAME.
X[ reports error as "X PTR Y: CNAME (to Z)" ]
X
X*  CNAMEs point to a host with an A record.
X[ reports error as "X CNAME Y: unknown host" ]
X
X*  CNAMEs do not point to another CNAME.
X[ reports error as "X CNAME Y: CNAME (to Z)" ]
X
X*  MXs point to a host with an A record.
X[ reports error as "X MX Y: unknown host" ]
X
X*  MXs do not point to a CNAME.
X[ reports error as "X MX Y: CNAME (to Z)" ]
X
X*  A records have some corresponding PTR record.  (Not necessarily
Xof the same name, of course)
X[ reports error as "X A Y: no PTR record" ]
X
X*  Reports any packet size errors listed in dig zone transfer output
X(Could be caused by a corrupted zone file, or invalid syntax used)
X
X*  That there is more than one authoritative nameserver for a zone.
XIt does not check if the machine is on a separate network (yet).
XA site should ALWAYS have a secondary nameserver.  It SHOULD be
Xon a different network than the primary.
X
X(with -a switch)
X*  duplicate A records listed for a given host.  NOTE: this is most
Xoften caused by the practice of always putting A records for all
Xsecondaries after NS glue records.  While this is not an error, it is
Xusually redundant and makes changing IP addresses later more difficult,
Xsince they occur more than one time in the file (and in multiple
Xfiles).  This checking needs more work.  (Mostly because of a quirk in
XBIND that reports cached A records in a zone transfer even though they
Xdon't exist in the original zone file.  I might just end up skipping this
Xcheck altogether.)
X[ reports error as "X: possible duplicate A record (glue of Z?)" 
Xwhere Z is the previous zone listed in the file ]
X
X(with -F switch)
X* perform "fascist" checking.  When checking an A record, compare the PTR
X  name for each IP address with the forward name.  This WILL result in
X  needless errors (like if you have an A record for your domain name
X  pointing to your main server, or have A records like "mailhost" or
X  "ns" defined to point to your mail or DNS server) but will catch little
X  errors that may have crept in if you have an A record pointing to a host
X  that you didn't intend to.
X[ reports error as "X A Y: points to Z" where Z is the "canonical" name
X  as returned by gethostbyaddr() ]
X
X(with -l switch)
X* Lame delegations.  A lame delegation is when a domain says "this
Xserver is a secondary for zone Z" but the listed server is not giving
Xout authoritative data for zone Z.  This is usually the result of
Xa lack of communication on the part of the respective hostmasters.
XLame delegations are not fatal problems, they just tend to create
Xsignificant increases in DNS traffic.
X[ reports error as "X NS Y: lame NS delegation" where X is the domain,
Xand Y is the lame nameserver ]
X
X
X*** NOTICE ***
X	I fully realize that while some of the above rules are not
Xin violation of an RFC, it might be wise to reconsider their usage anyway.
Xdnswalk was written to be a tool to let the hostmaster decide what are 
Xtroublesome areas, not as a program that has all the answers.
X*** NOTICE ***
X
X
XCAVEATS
X
X	If a domain "foo.edu" lists "ns.bar.foo.edu" as authoritative for
Xa zone "bar.foo.edu", but "ns.bar.foo.edu" isn't, then the the dig of
Xthe zone transfer will hang.  (This was the case here for a subdomain
Xthat moved into a new set of IP addresses, but the parent nameserver still
Xhad the old authority records pointing to their nameservers which weren't
Xanswering to the old reverse domain anymore.)  If this happens, you can
Xhit ^C while the transfer is going on and dnswalk will abort that server.
X(It will also remove the partial axfr file)  Hopefully I can figure a
Xmore elegant way around this.  (or fix dig so that it doesn't hang)
X
XThis program was tested with data from the psu.edu domain.  If your
Xsite does things differently than the way we do things, then you
Xmay see it report things as errors, when in fact they are "okay".
XIf you notice something not being reported, or something reported that
Xis not an error, please send me output!  I fully admit that I'm not
Xan expert in DNS and the requirements.  My rules tend to be skewed to
Xmy personal feelings about what "nice" DNS databases look like.  Others
Xare free to differ.  (and tell me so)
X
XBUGS
X	I should get around to writing a real man page.
X
X	dnswalk will make the directory tree before it has a chance to
Xfind out that you gave it a bogus domain name.
X
X	When checking lots of hosts and lots of options, it is very
Xslow.  Running dnswalk on a machine with a local nameserver helps
Xconsiderably.
X
X	Perl's gethostby{name,addr}() routine doesn't seem to
Xconsistently return an error whenever it is unable to resolve an
Xaddress.  Argh.  This will mean lots of "no PTR record" and "host unknown"
Xerrors if a server is unavailable, or for some reason the lookup fails.
X
X	I really need to rewrite this all to not rely on dig, and use
Xbind.pl instead.
X
XOTHER SOURCES
X
XRFC 1034 - "DOMAIN NAMES - CONCEPTS AND FACILITIES"
X
XRFC 1035 - "DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION"
X
XRFC 1123 - "Requirements for Internet Hosts -- Application and Support"
X
XPaul Albits, Cricket Liu: "DNS and BIND" O'Reilly & Associates.
X
X[the author recommends copies by your favorite bathroom and/or nightstand]
X
XDavid Barr - System Administrator
XThe Pennsylvania State University Population Research Institute
X<barr@pop.psu.edu>
X
XThanks:
X
XBill Fenner - tips with perl
X
XBUGS FIXED
X
Xadded equal() routine and changed all string comparisons to use it
Xinstead.  equal() does case-insensitive comparisons.  This was causing
Xseveral problems with mixed case host and domain names in various
Xplaces.
X
Xchecks for invalid PTR records now skip addresses ending in ".0".
X
Xshortened error messages so they are more likely to fit in 80 columns
X
Xhandles failed zone transfers better.  You can't trust dig to return
Xa non-zero return code if the transfer fails, so I parse the output
Xmyself to see if it contains an SOA record to prove that it is 'valid'.
X
Xsorts output by zone.  Also displays server of authority and the
Xcontact for the zone.  (So you know who to bug :-) )
X
Xproduced erroneous warnings regarding having only one authoratative
Xnameserver for a zone.  It now warns correctly.
END_OF_FILE
if test 11267 -ne `wc -c <'README'`; then
    echo shar: \"'README'\" unpacked with wrong size!
fi
# end of 'README'
fi
if test -f 'TIPS' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'TIPS'\"
else
echo shar: Extracting \"'TIPS'\" \(4138 characters\)
sed "s/^X//" >'TIPS' <<'END_OF_FILE'
X	Here's some tips I've come up with in my months of running
XDNS, as well as in development of dnswalk:
X
X* Every Internet host should have a name.  Enough said.
X
X* You shouldn't have any A records in an in-addr.arpa zone file.
X  This includes NS glue records.  Just put the nameserver name in
X  there and be done with it.  Why?  It's unnecessary, and just makes
X  things harder when that nameserver changes its IP address.  You'll
X  spend hours trying to figure out why random people still see the old
X  address for some machine.
X
X* Verify the data you just entered or changed by querying the
X  resolver with 'dig' (or your favorite DNS tool) after a change.  A
X  few seconds spent double checking can save hours of trouble, lost
X  mail, and headaches.
X
X* Don't forget to change the serial number.  Also, even though BIND
Xallows you to use a decimal in a serial number, don't use them.  If you
Xwant to know why, read "DNS & BIND" (see below).
X
X* Always remember your $ORIGIN.  If you don't put a '.' at the end
X  of an FQDN, it's not an FQDN.  Double check, triple check, those dots.
X
X* BE CONSISTENT!  If your $ORIGIN is "foo.org.", don't have entries
Xlike:
X
Xtron		in	a	1.2.3.1
Xmcp.foo.org.	in	a	1.2.3.2
X
Xor even:
X
Xmcp		in	a	1.2.3.2
X		in	mx	flynn.foo.org.	; why not just "flynn"?
X
XEither use all FQDNs everywhere or used unqualified names everywhere.
XDon't mix the two.  It just adds confusion and needless typing.  (Of
Xcourse this can't be avoided for RRs of hosts outside $ORIGIN)
X
X* Be a good net.neighbor.  Use HINFO records.  Don't believe what you
X  hear about the security concerns.  If you're too busy to worry about
X  fixing known vendor security holes, then you shouldn't be on the
X  Internet.  Don't forget that HINFO _requires_ two tokens, the machine
X  type, and the operating system.  BIND won't complain if the second is
X  missing, but will result in garbage and will confuse resolvers.
X
X* On the other hand, don't use WKS records.  They're useless and obsolete.
X
X* Pick friendly, easy to remember hostnames.  "rm5ws3" may tell you
X  that it's the 3rd workstation in room 5, but what if you move rm5ws1
X  and rm5ws2 to another room?  Also, don't succumb to the "Bond,
X  James Bond" naming scheme.  "psuvm.psu.edu" is no more informative
X  than "vm.psu.edu".  (Perpetuated by inferior networks like BITNET)
X
X* Have a secondary outside your network.  If the secondary isn't under
X  your control, periodically check up on them and make sure they're
X  properly set up to secondary for you.  (queries to their nameserver
X  about your machines should result in an "authoritative" response, etc)
X  Use the 'doc' program for this one.
X
X* make sure your parent domain has the same NS records (and same order)
X  for your zone as you do.  (Don't forget the in-addr.arpa domain
X  too!).  Use the 'doc' program if you're not sure how to check.
X
X* If a site plans to receive mail, give it an MX record, EVEN IF IT
X  POINTS TO ITSELF!  Some mailers will cache MX records, but will
X  ALWAYS query to find an MX before sending mail.  If a site does not
X  have an MX, then EVERY piece of mail will result in one more resolver
X  query.  (most mailers do not implement negative caching)  If you put
X  in an MX, then this data can be cached.  (Yes, Virginia, Internet
X  SMTP mailers are REQUIRED BY RFCs to support the "MX" mechanism.
X  Pound on sites that refuse to comply.)
X
X* Wildcard MX's are only useful for non IP-connected sites.  If
X  a site has any records, a wildcard MX won't apply to it.
Xe.g.
X*.podunk.edu	in	mx	mail.podunk.edu.
Xmary.podunk.edu	in	A	1.2.3.4
X
X  Mail for "mary.podunk.edu" will be sent to mary, while mail for
X  "jane.podunk.edu" will be sent to mail.podunk.edu.
X
X* Don't go overboard with CNAMEs.  Use them when moving/renaming machines,
X  but plan to get rid of them.  (And inform your users)
X
X* If a host is multi-homed, (more than on A record) make sure that all
X  its IP addresses have a corresponding PTR record.  (not just the first
X  one)
X
X* As more useful RRs come into existence, use them.  (Like TXT, RP, etc).
X
X* And of course, above all, use my dnswalk program.  :-)
X
X----
X	Dave Barr  <barr@pop.psu.edu>
END_OF_FILE
if test 4138 -ne `wc -c <'TIPS'`; then
    echo shar: \"'TIPS'\" unpacked with wrong size!
fi
# end of 'TIPS'
fi
if test -f 'TODO' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'TODO'\"
else
echo shar: Extracting \"'TODO'\" \(251 characters\)
sed "s/^X//" >'TODO' <<'END_OF_FILE'
XUse bind.pl instead of dig  (This fixes a whole host of problems,
Xnot the least of which is speed, but puts more trust in Perl's resolver)
X
XBetter error checking so it doesn't create a foo/bar/baz directory
Xwhen the baz/bar/foo domain doesn't exist.
X
END_OF_FILE
if test 251 -ne `wc -c <'TODO'`; then
    echo shar: \"'TODO'\" unpacked with wrong size!
fi
# end of 'TODO'
fi
if test -f 'dnswalk' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'dnswalk'\"
else
echo shar: Extracting \"'dnswalk'\" \(12326 characters\)
sed "s/^X//" >'dnswalk' <<'END_OF_FILE'
X#!/usr/local/bin/perl
X# dnswalk    Walk through a DNS tree, pulling out zone data and
X# dumping it in a directory tree
X#
X# $Id: dnswalk,v 1.1 1993/03/13 18:25:44 barr Exp barr $
X#
X# check data collected for legality using standard resolver
X#
X# invoke as dnswalk domain > logfile
X# Options:
X#    -r    Recursively descend subdomains of domain
X#    -f    Force a zone transfer, ignore existing axfr file
X#    -c    Check "CNAME and other data".  BIND does not let this occur,
X#          so it normally just slows dnswalk down.
X#    -a    turn on warning of duplicate A records.
X#    -d    Debugging
X#    -m    Check only if the domain has been modified.  (Useful only if
X#        dnswalk has been run previously.)
X#    -F    Enable "facist" checking.  (See README)
X#    -l    Check lame delegations
X
Xrequire "getopts.pl";
Xrequire "bind.pl";
X
Xdo Getopts(":rfcadmFl");
X
X
X$basedir = ".";
X($domain = $ARGV[0]) =~ tr/A-Z/a-z/;
Xif ($domain !~ /\.$/) {
X    die "Usage: dnswalk domain\ndomain MUST end with a '.'\n";
X}
Xmkdir($basedir,0777);
X
X&dowalk($domain);
X
Xexit;
X
Xsub dowalk {
X    local (@subdoms);
X    local (@sortdoms);
X    local ($domain)=$_[0];
X    $modified=0;
X#    ($file,@subdoms)=&doaxfr($domain);  /* perl bug */
X    @subdoms=&doaxfr($domain);
X    $file=shift(@subdoms);
X    if (!($opt_m && !$modified)) {
X        &checkfile($file,$domain);
X    }
X    else {
X        print STDERR "skipping...\n";
X    }
X    @sortdoms = sort byhostname @subdoms;
X    local ($subdom);
X    if ($opt_r) {
X        foreach $subdom (@sortdoms) {
X            &dowalk($subdom);
X        }
X    }
X}
X# try to get a zone transfer, trying each listed authoratative server if
X# if fails.
Xsub doaxfr {
X    local ($domain)=@_[0];
X    local (%subdoms)=();
X    local ($serial)=0;
X    local ($foundsoa)=0;    # attempt to make up for dig's poor 
X                # error handling
X    ($path=&host2path($domain)) =~ tr/A-Z/a-z/;
X    local(@servers) = &getauthservers($domain);
X    print "warning: $domain has only one authoratative nameserver\n" if (scalar(@servers) == 1);
X    if ((-f "$basedir/$path/axfr") && (!$main'opt_f)) {
X        open(DIG,"<$basedir/$path/axfr");
X        while (<DIG>) {
X            chop;
X            if (/(\d+)\s*;serial/) {
X                $serial=$1;
X            }
X            if (/(\S+)\s*\d+\s+NS/) {
X                $1 =~ tr/A-Z/a-z/;
X                if ((!&equal($1,$domain)) && ( !$subdoms{$1})) {
X                    $subdoms{$1}=1;
X                }
X            }
X        }
X        # if there's no serial number in file, assume it is corrupt
X        if ($serial) {
X            foreach $server (@servers) {
X                $authserno=&getserno($domain,$server);
X                last if ($authserno);
X            }
X            if ($authserno <= $serial) {
X                print STDERR "Using existing zone transfer info for $domain\n";
X                return ("$basedir/$path/axfr", keys %subdoms);
X            }
X        }
X    }
X    &mkdirpath($path);
X    SERVER:
X    foreach $server (@servers) {
X        $SIG{'INT'}="nop;";
X        print STDERR "Getting zone transfer of $domain from $server...";
X        open(DIG,"dig axfr $domain \@$server 2>/dev/null |");
X        open(DIGOUT,">$basedir/$path/axfr");
X        while (<DIG>) {
X            if (/(\S+)\s*\d+\s+NS/) {
X                $1 =~ tr/A-Z/a-z/;
X                if ((!&equal($1,$domain)) && ( !$subdoms{$1})) {
X                    $subdoms{$1}=1;
X                }
X            }
X            elsif (/\S+\s*\d+\s+SOA/) {
X                $foundsoa=1;
X            }
X            print DIGOUT $_;
X        }
X        if ($? || !$foundsoa) {
X            print STDERR "failed.\n";
X            close(DIGOUT);
X            next SERVER;
X        }
X        print STDERR "done.\n";
X        close(DIGOUT);
X        close(DIG);
X        last SERVER;
X    } # foreach #
X    $SIG{'INT'}=undef;
X    if ($? || !$foundsoa) {
X        print STDERR "All zone transfer attempts of $domain failed\n";
X        unlink("$basedir/$path/axfr");
X        return undef;
X    }
X    $modified=1;
X    return ("$basedir/$path/axfr", keys %subdoms);
X}
X
X# returns "edu/psu/pop" given "pop.psu.edu"
Xsub host2path {
X    join('/',reverse(split(/\./,$_[0])));
X}
X
X# makes sure all directories exist in "foo/bar/baz"
Xsub mkdirpath {
X    local (@path)=split(/\//,$_[0]);
X    local ($dir)=$basedir;
X    foreach $p (@path) {
X        mkdir(($dir .= "/".$p), 0777);
X    }
X}
X
Xsub getserno {
X    local ($serno)="";
X    $SIG{'INT'}="nop;";
X    open(DIG,"dig soa $_[0] \@$_[1] 2>/dev/null|");
X    while (<DIG>) {
X        if (/(\d+)\s*;serial/) { 
X            $serno=$1;
X        }
X    }
X    close(DIG);
X    $SIG{'INT'}=undef;
X    return $serno;
X}
X
X
Xsub getauthservers {
X    local ($domain)=$_[0];
X    open(DIG,"dig +noau ns $_[0] 2>/dev/null|");
X    local(@servers);
X    while (<DIG>) {
X        chop;
X        if (/\S+\s*\d+\s+NS\s+(\S+)/) {
X            push(@servers,$1);
X        }
X    }
X    close(DIG);
X    return @servers;
X}
X
X# open result of zone tranfer and check lots of nasty things
X# here's where the fun begins
Xsub checkfile {
X    open(FILE,"<$_[0]");
X    print "Checking $domain\n";
X    local (%glues)=();	# look for duplicate glue (A) records
X    local ($name, $aliases, $addrtype, $length, @addrs);
X    local ($prio,$mx);
X    local ($soa,$contact);
X    local ($lastns);	# last NS record we saw
X    local (@keys);	# temp variable
X    while (<FILE>) {
X        chop;
X        if (/^;/) {
X            if (/(.*[Ee][Rr][Rr][Oo][Rr].*)/) {
X                # print any dig errors
X                print $1 ."\n";
X                next;
X            }
X        }
X        next if /^$/;	# skip blanks
X        split(/\t/);
X        # 0=key 2=rrtype 3=value (4=value if 2=MX)
X        next if ($_[0] =~ /;/);
X        if ($_[0] =~ /[^-A-Za-z0-9._]/) {
X	    # I know, underscores aren't kosher but ....
X            print " $_[0]: invalid character(s) in name\n";
X        }
X        if ($_[2] eq "SOA") {
X            print STDERR 's' if $opt_d;
X            ($soa,$contact) = $_[3] =~ /(\S+)\s+(\S+)/;
X            print "SOA=$soa	contact=$contact\n";
X        } elsif ($_[2] eq "PTR") {
X            print STDERR 'p' if $opt_d;
X            if (scalar((@keys=split(/\./,$_[0]))) == 6 ) {
X                # check if forward name exists, but only if reverse is
X                # a full IP addr
X                # skip ".0" networks
X                if ($keys[0] ne "0") {
X                    if (!(($name, $aliases, $addrtype, $length, @addrs)=gethostbyname($_[3])) && $!) {
X                        print " gethostbyname($_[3]): $!\n";
X                    }
X                    else {
X                        if (!$name) {
X                            print " $_[0] PTR $_[3]: unknown host\n";
X                        }
X                        elsif (!&equal(($name.'.'),$_[3])) {
X                            print " $_[0] PTR $_[3]: CNAME (to $name)\n";
X                        }    
X                        elsif (!&matchaddrlist($_[0])) {
X                            print " $_[0] PTR $_[3]: A record not found\n";
X                        }
X                    }
X                }
X            }
X        } elsif (($_[2] eq "A") ) {
X            print STDERR 'a' if $opt_d;
X# check to see that a reverse PTR record exists
X            if (!(($name,$aliases,$addrtype,$length,@addrs)=gethostbyaddr(pack('C4', split(/\./,$_[3])),2)) && $!) {
X                print " gethostbyaddr($_[3]): $!\n";
X            }
X            else {
X                if (!$name) {
X                    print " $_[0] A $_[3]: no PTR record\n";
X                }
X                elsif ($opt_F && !&equal($name.".",$_[0])) {
X                    print " $_[0] A $_[3]: points to $name\n" if ((split(/\./,$name,1))[0] ne "localhost");
X                }
X                if ($main'opt_a) {
X                    # keep list in %glues, report any duplicates
X                    if ($glues{$_[3]} eq "") {
X                        $glues{$_[3]}=$_[0];
X                    }
X                    elsif (($glues{$_[3]} eq $_[0]) && (!&equal($lastns,$domain))) {
X                            print " $_[0]: possible duplicate A record (glue of $lastns?)\n";
X                    }
X                }
X            }
X        } elsif ($_[2] eq "NS") {
X            $lastns=$_[0];
X            print STDERR 'n' if $opt_d;
X            # check to see if object of NS is real
X            &checkcnamedata($_[0]) if ($main'opt_c);
X            &checklamer($_[0],$_[3]) if ($main'opt_l);
X            if (!(($name, $aliases, $addrtype, $length, @addrs)=gethostbyname($_[3])) && $!) {
X                print " gethostbyname($_[3]): $!\n";
X            }
X            else {
X                if (!$name) {
X                    print " $_[0] NS $_[3]: unknown host\n";
X                } elsif (!&equal(($name.'.'),$_[3])) {
X                    print " $_[0] NS $_[3]: CNAME (to $name)\n";
X                }
X            }
X        } elsif ($_[2] eq "MX") {
X            print STDERR 'm' if $opt_d;
X            # check to see if object of mx is real
X            ($prio,$mx)=split(/ /,$_[3]);
X            if (!(($name, $aliases, $addrtype, $length, @addrs)=gethostbyname($mx)) && $!) {
X                print " gethostbyname($mx): $!\n";
X            }
X            else {
X                if (!$name) {
X                    print " $_[0] MX $_[3]: unknown host\n";
X                }
X                elsif (!&equal(($name.'.'),$mx)) {
X                    print " $_[0] MX $_[3]: CNAME (to $name)\n";
X                }
X            }
X        } elsif ($_[2] eq "CNAME") {
X            print STDERR 'c' if $opt_d;
X            # check to see if object of cname is real
X            &checkcnamedata($_[0]) if ($main'opt_c);
X            if (!(($name, $aliases, $addrtype, $length, @addrs)=gethostbyname($_[3])) && $!) {
X                print " gethostbyname($_[3]): $!\n";
X            }
X            else {
X                if (!$name) {
X                    print " $_[0] CNAME $_[3]: unknown host\n";
X                } elsif (!&equal(($name.'.'),$_[3])) {
X                    print " $_[0] CNAME $_[3]: CNAME (to $name)\n";
X                }
X            }
X        }
X    }
X    print STDERR "\n" if $opt_d;
X    close(FILE);
X}
X
X# this probably won't catch anything anyway
Xsub checkcnamedata {
X    local ($cname)=$_[0];
X    local ($valid)=0;    # if cname points to known host
X    # must be +aa to avoid partial caching effects
X    open(DIG,"dig +nocmd +aa +noad +nostats +noheader +noHeader +noau +noques any @_[0] 2>/dev/null |");
X    local(@info);
X    while (<DIG>) {
X        chop;
X        next if /^$/;
X        next if /^;/;
X        split(/\t/);
X        next if (!&equal($_[0],$cname));    # skip info on object of cname
X        next if ($_[2] eq "CNAME");    # skip cname itself
X        print " CNAME and other data - $_[2]\n";
X    }
X    close(DIG);
X    return @servers;
X}
X
Xsub equal {
X    # Do case-insensitive string comparisons
X    local ($one)= $_[0];
X    local ($two)= $_[1];
X    $one =~ tr/A-Z/a-z/;
X    $two =~ tr/A-Z/a-z/;
X    return ($one eq $two);
X}
X
Xsub matchaddrlist {
X    local($match)=pack('C4', reverse(split(/\./,$_[0],4)));
X    local($found)=0;
X    foreach $i (@addrs) {
X        $found=1 if ($i eq $match);
X    }
X    return $found;
X}
X
X# there's a better way to do this, it just hasn't evolved from
X# my brain to this program yet.
Xsub byhostname {
X    @c = reverse(split(/\./,$a));
X    @d = reverse(split(/\./,$b));
X    for ($i=0;$i<=(($#c > $#d) ? $#c : $#d) ;$i++) {
X        next if $c[$i] eq $d[$i];
X        return -1 if $c[$i] eq "";
X        return  1 if $d[$i] eq "";
X        if ($c[$i] eq int($c[$i])) {
X            # numeric
X            return $c[$i] <=> $d[$i];
X        }
X        else {
X            # string
X            return $c[$i] cmp $d[$i];
X        }
X    }
X    return 0;
X}
X
Xsub checklamer {
X    local ($isauth)=0;
X    local ($error)=0;
X    # must check twice, since first query may be authoratative
X    # trap stderr here and print if non-empty
X    open(DIG,"dig soa +noaa $_[0] \@$_[1] 2>&1 1>/dev/null |");
X    while (<DIG>) {
X        print " $_[0] NS $_[1]: nameserver error:\n" if !$error;
X	print;
X	$error=1;
X    }
X    close(DIG);
X    return if $error;
X    open(DIG,"dig soa +noaa $_[0] \@$_[1] 2>/dev/null|");
X    while (<DIG>) {
X        if (/;; flags.*aa.*;/) { 
X            $isauth=1;
X        }
X    }
X    if (!$isauth) {
X        print " $_[0] NS $_[1]: lame NS delegation\n";
X    }
X    close(DIG);
X    return;
X}
END_OF_FILE
if test 12326 -ne `wc -c <'dnswalk'`; then
    echo shar: \"'dnswalk'\" unpacked with wrong size!
fi
chmod +x 'dnswalk'
# end of 'dnswalk'
fi
if test -f 'do-dnswalk' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'do-dnswalk'\"
else
echo shar: Extracting \"'do-dnswalk'\" \(466 characters\)
sed "s/^X//" >'do-dnswalk' <<'END_OF_FILE'
X#!/bin/sh
X# Here's an example script for a hostmaster of a large site
X# to automate the process
X# try adding '-F' here once just to see what pops up
Xflags='-r -d'
Xlogfile=podunk.errors
Xtrap "" 2 15;
X./dnswalk ${flags} $* podunk.edu. > ${logfile}
X./dnswalk ${flags} $* 1.1.in-addr.arpa. >> ${logfile}
X./dnswalk ${flags} $* 2.1.in-addr.arpa. >> ${logfile}
X./dnswalk ${flags} $* 1.2.1.in-addr.arpa. >> ${logfile}
X./dnswalk ${flags} $* 2.2.1.in-addr.arpa. >> ${logfile}
END_OF_FILE
if test 466 -ne `wc -c <'do-dnswalk'`; then
    echo shar: \"'do-dnswalk'\" unpacked with wrong size!
fi
chmod +x 'do-dnswalk'
# end of 'do-dnswalk'
fi
echo shar: End of shell archive.
exit 0
