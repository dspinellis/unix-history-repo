/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)version.c	5.61 (Berkeley) 9/20/88";
#endif /* not lint */

char	Version[] = "5.61";

# ifdef COMMENT

SCCS/s.version.c:

D 5.61	88/09/20 22:15:19	eric	484	483	00000/00000/00023
add I option to insist on Internet domain server; add NAMED_BIND
compile option to compile in named routines.

D 5.60	88/06/30 15:00:04	bostic	483	482	00011/00009/00012
install approved copyright notice

D 5.59	88/03/13 19:53:40	bostic	482	481	00016/00010/00005
add Berkeley specific header

D 5.58	87/02/03 20:44:04	eric	481	479	00000/00000/00015
pass sending hostname correctly back via the $s macro (broken sometime
earlier); hack around some name server changes

D 5.57.1.1	86/12/17 16:08:34	eric	480	479	00000/00000/00015
hack around compiler bug that gets confused on isascii

D 5.57	86/10/23 10:11:10	eric	479	478	00000/00000/00015
Fix security bug allowing writing to arbitrary files; from Bart Miller
at U. Wisconsin.

D 5.56	86/10/14 18:06:05	eric	478	477	00000/00000/00015
don't let "locked job" messages remove transcript & lock

D 5.55	86/10/14 17:21:15	eric	477	476	00000/00000/00015
make sure that 0, 1, and 2 are open to avoid confusing syslog;
from Steve Schoch <schoch@orion.arpa>

D 5.54	86/07/21 12:21:11	bloom	476	475	00000/00000/00015
add MX support

D 5.53	86/06/30 16:20:56	bloom	475	474	00000/00000/00015
try multiple addresses in SMTP connect

D 5.52	86/05/06 18:04:16	bloom	474	473	00000/00000/00015
Fix problem with not closing sockets upon errors when opening connections

D 5.51	86/05/02 16:28:28	bloom	473	472	00000/00000/00015
statistics structure moved to its own header file so it may be used by 
aux/mailstats

D 5.50	86/04/17 20:18:12	eric	472	471	00000/00000/00015
don't ignore aliasing during queue runs in conjunction with -bd

D 5.49	86/04/02 16:02:31	eric	471	470	00000/00000/00015
don't run around clearing EF_FATALERRS -- this often applies to another
address.  Drop uid/gid check in sameaddr -- it causes more problems
than it fixes.

D 5.48	86/03/08 14:12:07	eric	470	469	00000/00000/00015
improve debugging in queue routines; don't output error addresses that
are duplicates or otherwise marked as "don't send"

D 5.47	86/03/08 09:28:15	eric	469	468	00000/00000/00015
do dbminit in a reasonable place

D 5.46	86/03/02 14:07:11	eric	468	467	00000/00000/00015
always ignore SIGCHLD in openmailer; get SccsId correct in alias.c

D 5.45	86/01/30 11:02:58	eric	467	466	00000/00000/00015
fix .forward files that include yourself; this was a bug in sameaddr
that may have caused some other problems

D 5.44	86/01/11 00:18:27	eric	466	465	00000/00000/00015
only check the RHS of aliases during newalias if the "n" option is set;
this option should probably be set if you are not running the nameserver

D 5.43	86/01/10 16:34:08	eric	465	464	00000/00000/00015
adjust WkTimeFact so that -q1h will lower relative priorities of
jobs over long periods, rather than leave them toward the top of the queue

D 5.42	86/01/10 15:49:40	eric	464	463	00000/00000/00015
allow multiple words per line in file classes; require a SCANF compilation
flag to use sscanf in F specs in order to reduce image size

D 5.41	86/01/09 15:19:09	eric	463	462	00000/00000/00015
apparently European timezones were wrong; thanks to Piet Beertema
<mcvax!piet@seismo.CSS.GOV> for this one

D 5.40	86/01/09 14:38:45	eric	462	461	00000/00000/00015
allow underscores in login names

D 5.39	86/01/05 18:49:01	eric	461	460	00000/00000/00015
rewrite reply-to and resent-reply-to; save errorqueueu in qf file;
some performance hacking; some alias handling cleanup; delete leading
spaces from SMTP lines

D 5.38	85/12/17 23:54:45	eric	460	459	00000/00000/00015
lint

D 5.37	85/12/17 21:35:28	eric	459	458	00000/00000/00015
patch to clearenvelope to avoid dereferencing garbage pointers

D 5.36	85/12/09 10:29:07	miriam	458	457	00000/00000/00015
Modify maphostname to do a gethostbyaddr if first character in name 
is a bracket.

D 5.35	85/12/07 08:17:56	eric	457	456	00000/00000/00015
several small bugs: don't die if no environment, don't look in
ESM_DEADLETTER state in savemail; lowercase before getpwnam to
allow upper case regular names in alias file

D 5.34	85/11/22 11:38:09	miriam	456	455	00000/00000/00015
Distinguish between temporary failure types.  Now will print the 
message "Host Name Lookup Failure" when h_errno contains TRY_AGAIN error value.

D 5.33	85/11/22 08:27:48	eric	455	454	00000/00000/00015
give error if alias file cannot be opened; log a message when aliases
are rebuilt; consider addresses with different q_uid's different so
that two recipients forwarding to the same program will work

D 5.32	85/11/21 18:49:02	eric	454	453	00000/00000/00015
don't duplicate original envelope into error envelope

D 5.31	85/10/24 10:38:28	eric	453	452	00000/00000/00015
don't create a queue name in syserr; permit trailing blanks and quoted
commas in aliases.

D 5.30	85/10/19 09:55:01	eric	452	451	00000/00000/00015
strip spaces from ends of alias addresses so that blanks at end of line works

D 5.29	85/10/13 15:03:36	eric	451	450	00000/00000/00015
fix botch with reapchild getting queue runs before intended wait

D 5.28	85/09/30 21:34:39	eric	450	449	00000/00000/00015
clean up some aspects of error message display

D 5.27	85/09/30 21:06:04	eric	449	448	00000/00000/00015
fixes in setproctitle to avoid problems with titles longer than
argv + env

D 5.26	85/09/25 11:02:19	eric	448	447	00000/00000/00015
DO use Ruleset 4 when defining $f -- it's very necessary (fix for <>
will have to be done in configuration); pretty up mailq -v slightly

D 5.25	85/09/24 15:49:04	eric	447	446	00000/00000/00015
clean up queue output somewhat (push null jobs to end); set
SO_REUSEADDR and SO_KEEPALIVE on daemon sockets in the hopes of making
dead connections disappear faster

D 5.24	85/09/24 15:09:56	eric	446	445	00000/00000/00015
don't step on user environment

D 5.23	85/09/23 21:19:05	eric	445	444	00000/00000/00015
deliver directly in SMTP if VERB command has been issued; don't
externalize name using ruleset 4 when defining $f macro: this turns "<>"
into "", which confuses local mail

D 5.22	85/09/21 16:35:33	eric	444	443	00000/00000/00015
yet more cleanup to the process title code

D 5.21	85/09/21 16:24:06	eric	443	442	00000/00000/00015
don't include ctime as part of priority, since the value affects the results of
shouldqueue; we go back to adding ctime into the workcmpf in queue.c

D 5.20	85/09/21 15:52:02	eric	442	441	00000/00000/00015
change sign on WkTimeFact so that is closer to what most people want

D 5.19	85/09/21 15:01:31	eric	441	440	00000/00000/00015
fix silly botch in SMTP command decoding

D 5.18	85/09/21 14:45:53	eric	440	439	00000/00000/00015
clean up priority handling, making several of the parameters configurable:
y - WkRecipFact, z - WkClassFact, Z - WkTimeFact, Y - ForkQueueRuns; improve
process title labelling; finish "errors to postmaster" option

D 5.17	85/09/21 10:31:27	eric	439	438	00000/00000/00015
add -v mode to mailq to print priorities as well (this should be extended
in the future); fix some problems in the savemail state machine.

D 5.16	85/09/20 09:43:20	eric	438	437	00000/00000/00015
print cute labels on programs communicating with SMTP

D 5.15	85/09/19 23:16:24	eric	437	436	00000/00000/00015
label child processes more effectively

D 5.14	85/09/19 22:01:02	eric	436	435	00000/00000/00015
use rename instead of link/unlink

D 5.13	85/09/19 17:43:16	eric	435	434	00000/00000/00015
fix botch in clearenvelope

D 5.12	85/09/19 15:57:48	eric	434	433	00000/00000/00015
updates to make it possible to run the queue in one process; this
permits a database of host status to be built

D 5.11	85/09/19 13:41:13	eric	433	432	00000/00000/00015
lint

D 5.10	85/09/19 01:25:45	eric	432	431	00000/00000/00015
incorporate SMI changes -- still experimental

D 5.9	85/09/17 21:46:27	eric	431	430	00000/00000/00015
use SIGCHLD to catch processes as suggested by Serge Granik

D 5.8	85/09/17 19:24:39	eric	430	429	00000/00000/00015
facilities in syslog

D 5.7	85/09/03 20:08:54	eric	429	428	00000/00000/00015
increase MAXNAME and MAXLINE, as requested by Rick Adams, via
George Goble, via Kirk Smith, via Miriam Amos (why????)

D 5.6	85/09/03 19:50:17	eric	428	427	00000/00000/00015
Wander WIZ from weariful DEBUG to wonderful WIZ woption

D 5.5	85/06/17 18:53:09	eric	427	426	00000/00000/00015
From Bill Nowicki: fixes to the statistics

D 5.4	85/06/16 16:04:51	eric	426	425	00000/00000/00015
arrange for a useful error message if the mailer fork fails

D 5.3	85/06/15 18:52:11	eric	425	424	00000/00000/00015
fix overzealous removal of df file

D 5.2	85/06/08 10:30:59	eric	424	423	00000/00000/00015
lint for 4.3 release

D 5.1	85/06/07 15:19:18	dist	423	422	00013/00003/00002
Add copyright

D 4.56	85/06/02 10:54:52	eric	422	421	00000/00000/00005
plug another security hole with command line arguments

D 4.55	85/06/01 15:26:40	eric	421	420	00000/00000/00005
More changes from Bill Nowicki -- file closing and improved logging.

D 4.54	85/05/24 11:00:43	eric	420	419	00000/00000/00005
Changes from Bill Nowicki <sun!rose!nowicki> and Jay Lepreau <lepreau@utah-cs>:
Fix "bad file number" problem; improve error reporting; try to keep messages
closer to their original order.  Also, drop "safe" mode in readcf since we
never run setuid when -C is specified.

D 4.53	85/05/15 20:26:44	eric	419	418	00000/00000/00005
reenable signals in an event that may be called to run the queue; this
allows hung connections to time out properly during a queue run.  This
fix provided by Bill Nowicki.

D 4.52	85/05/06 20:06:04	eric	418	417	00000/00000/00005
check syscall return values in a few questions; thanks go to Ian Darwin's
rudely public ragging on this one.

D 4.51	85/04/29 22:48:37	eric	417	416	00000/00000/00005
lock alias file while rebuilding if flock system call available

D 4.50	85/04/28 10:46:11	eric	416	415	00000/00000/00005
stop collecting message on ferror(InChannel); changes to compile even
if DEBUG isn't defined; avoid sending nonstandard 050 messages unless
requested; recover from trashed DBM files; use recipient rewriting set
on user part after ruleset 0 completes

D 4.49	85/04/25 20:06:08	miriam	415	414	00000/00000/00005
Remove lib/libsys.a references - not used anymore.

D 4.48	85/04/20 15:14:15	eric	414	413	00000/00000/00005
don't assume that all apparently local senders have passwd entries

D 4.47	85/04/04 17:48:45	miriam	413	412	00000/00000/00005
Change serverity of LOG_ERR to LOG_MAIL so syslog will place in 
appropriate log file.

D 4.46	85/02/15 09:28:10	eric	412	411	00000/00000/00005
fix some bugs with -C flag; one with queuing from Teus

D 4.45	85/02/14 22:43:42	eric	411	410	00000/00000/00005
"and" file mode bits with 0777

D 4.44	84/12/06 10:35:01	eric	410	409	00000/00000/00005
back out attempt to use flock in the queue -- we don't in gerneral have
an open file descriptor available.

D 4.43	84/12/05 23:16:18	eric	409	408	00000/00000/00005
Try to use flock call (this doesn't work because we don't always have an
open fd); security and performance fixes from Kirk Smith at Purdue; "a"
option is now the number of minutes to wait for "@:@" alias; fix bug in
$[ $] using -t; random cleanup

D 4.42	84/11/13 12:46:05	eric	408	407	00000/00000/00005
assorted optimizations (no functional changes)

D 4.41	84/09/18 19:53:05	eric	407	406	00000/00000/00005
fix multiline aliases

D 4.40	84/09/08 17:44:21	eric	406	405	00000/00000/00005
fix hostname mapping to be repeatable (as required by some .cf files).

D 4.39	84/08/11 23:19:30	eric	405	404	00000/00000/00005
Add $[ and $] as RHS operators to look up the contents and pass them
to maphostname; maphostname currently looks them up in /etc/hosts and
converts them to canonical form, but could be turned into a general
name server.....   huzzah!!

D 4.38	84/08/11 17:56:24	eric	404	403	00000/00000/00005
changes from Tom Ferrin <ucsfcgl!tef>: don't drop messages on the floor
if no local mailer available; give real "errno" message in syserr.

D 4.37	84/08/11 17:50:27	eric	403	402	00000/00000/00005
Assorted changes from Guy Harris <rlgvax!guy>: mostly lint & USG

D 4.36	84/08/11 16:57:17	eric	402	401	00000/00000/00005
don't add ".ARPA" (or whatever net name) to names that already have
a dot in them; pull NetName out of initialized data space so that
it can be changed in frozen configuration files

D 4.35	84/08/11 16:54:59	eric	401	400	00000/00000/00005
Changes from Greg Couch <ucsfcgl!gregc> for V7 compatibility and
miscellaneous bug fixes; "clear" => "bzero" and "bmove" => "bcopy"
throughout for consistency; bzero is now in bcopy.c (these are
supplied by libc on 4.2bsd)

D 4.34	84/08/11 14:38:46	eric	400	399	00000/00000/00005
fixes from Liudvikas Bukys <bukys@rochester.ARPA>:
allow -M flag to be used more than once;
handle hosts where "gethostname" does not return the canonical name.

D 4.33	84/08/11 13:23:32	eric	399	398	00000/00000/00005
add E mailer flag to > escape From lines (for files)

D 4.32	84/08/05 11:01:18	eric	398	397	00000/00000/00005
add B option to set blank substitution character

D 4.31	84/08/05 10:14:14	eric	397	396	00000/00000/00005
alway reset uid and gid immediately if alternate config file

D 4.30	84/05/13 15:45:35	eric	396	395	00000/00000/00005
remove .mailcf hack -- it's been abused.

D 4.29	84/05/13 14:03:01	eric	395	394	00000/00000/00005
change "returnto" to "returnq" for PDP-11 compilers

D 4.28	84/03/17 16:26:58	eric	394	393	00000/00000/00005
always fold case on host names; fold case on the LHS of aliases

D 4.27	84/03/11 21:21:31	eric	393	392	00000/00000/00005
fix argument to gethostname left over from some old interface....

D 4.26	84/03/11 19:58:20	eric	392	391	00000/00000/00005
disable UPPER->lower case mapping in RHS's of aliases so that upper
case letters can be used in file names and as args to programs.

D 4.25	84/03/11 16:49:25	eric	391	390	00000/00000/00005
changes from Bill Nowicki <nowicki@diablo.ARPA> to avoid sending
errors if a connection is aborted;
changes from Greg Katz <katz@sri-tsc> to help with PDP-11 versions;
allow home network name to be changed;
change macro expansion character from $ to \001 so that $'s can be
used in headers (.cf unchanged).

D 4.24	83/12/27 22:52:50	eric	390	389	00000/00000/00005
don't close files immediately before exec of mailer so that we can log;
use FIOCLEX instead.  Suggested by Tom Ferrin, UCSF CGL.

D 4.23	83/12/27 21:21:47	eric	389	388	00000/00000/00005
fix bug with un-DBM'ed alias files that adds a newline on the end of
the last entry in the alias; found by John Gilmore, SMI

D 4.22	83/11/26 18:52:55	eric	388	387	00000/00000/00005
fix SERIOUS bug allowing anyone to be "wiz" without a password
if the configuration was frozen

D 4.21	83/11/13 18:08:19	eric	387	386	00000/00000/00005
Fixes two nasty problems, both pointed out by Bill Nowicki at Stanford:
I/O errors on input in collect would cause infinite loops, and a protocol
error (or other error that would call smtpquit abnormally) would cause
core dumps

D 4.20	83/11/10 09:05:46	eric	386	385	00000/00000/00005
Be able to override the hostname in the configuration file when frozen

D 4.19	83/10/29 16:46:12	eric	385	384	00000/00000/00005
declare getpwnam in recipient.c for earlier systems

D 4.18	83/10/29 12:01:44	eric	384	383	00000/00000/00005
add newline to "deferred" message in usersmtp.c

D 4.17	83/10/23 17:16:56	eric	383	382	00000/00000/00005
handle dollar signs in headers properly

D 4.16	83/10/16 16:08:08	eric	382	381	00000/00000/00005
Postpone opening the alias DBM file until after the fork in srvrsmtp so
that the alias database is as current as possible; thanks to dagobah!efo
(Eben Ostby) for this one.

D 4.15	83/10/16 15:26:12	eric	381	380	00000/00000/00005
reset errno in parseaddr so that syserr gives a permanent error code and
no extraneous information about non-errors

D 4.14	83/10/02 15:31:56	eric	380	379	00000/00000/00005
Use old environment after the thaw; credit rhc for this.

D 4.13	83/10/01 16:57:57	eric	379	378	00000/00000/00005
clean up error handling in general; make sure that something gets logged
in the transcript if the connection cannot be established; clean up Teus
Hagen's mod to arpadate.c to match the sendmail coding style.

D 4.12	83/09/07 09:45:41	eric	378	377	00000/00000/00005
Increase timeout for greeting message to five minutes; remember to close
the connection properly if we get a failure during connection establishment.

D 4.11	83/09/05 15:02:48	eric	377	376	00000/00000/00005
Fix security hole caused by being able to freeze the configuration
without owning the .fc file.

D 4.10	83/09/05 14:33:54	eric	376	375	00000/00000/00005
Cut down the amount of bulk that is sent in SMTP error messages, by
trying to log only real errors in the transcript.  -v mode is unchanged.

D 4.9	83/08/31 17:42:50	eric	375	374	00000/00000/00005
fix problem with timeouts caused by change in EINTR semantics in 4.2bsd;
add a two minute timeout on the greeting message in user smtp to detect
hung connections

D 4.8	83/08/28 15:38:15	eric	374	373	00000/00000/00005
set FIOCLEX on /dev/kmem file when getting load average

D 4.7	83/08/28 14:45:35	eric	373	372	00000/00000/00005
Refuse to talk to yourself (i.e., reject HELO packets with your own name).
Add two thresholds -- option 'x' is the load average at which messages are
queued rather than delivered (default 12); option 'X' is the load average
at which incoming TCP connections are refused (default 25).

D 4.6	83/08/21 15:40:13	eric	372	371	00000/00000/00005
Drop "Sender:" hack, since it doesn't work properly when relaying messages.

D 4.5	83/08/21 15:15:09	eric	371	370	00000/00000/00005
Insert a Sender: line if a From: line is specified and is different than
what we would insert; don't send back a separate error message if we have
diagnosed an error in a RCPT command; fix a *0 problem in some debug code.

D 4.4	83/08/06 10:37:57	eric	370	369	00000/00000/00005
Clear errno before trying connect in an attempt to track down EPERM
problems.

D 4.3	83/07/31 10:46:22	eric	369	368	00000/00000/00005
Add EX_NOPERM to sysexits.h for kre

D 4.2	83/07/27 22:56:44	eric	368	367	00000/00000/00005
Don't uppercase hostname in myhostname so that it can be used as a
UUCP name.

D 4.1	83/07/25 19:46:27	eric	367	366	00000/00000/00005
4.2 release version

D 3.347	83/07/13 10:38:17	eric	366	365	00000/00000/00005
Delete "load limiting" for SMTP connections (a bad ethernet board can
hang up all incoming mail); use sfgets in collect (same reason); check
for I/O error in collect (from Bill Nowicki); switch date format to
RFC822 style.

D 3.346	83/06/14 11:05:18	eric	365	364	00000/00000/00005
log the message-id only if non-null

D 3.345	83/06/11 20:59:30	eric	364	363	00000/00000/00005
%d => %ld in mailq for PDP-11's

D 3.344	83/06/11 19:28:58	eric	363	362	00000/00000/00005
MRs:	045
MRs:	240
don't rearrange input header lines; force Received: lines to be at the
beginning by always adding new header fields at the end of the header.

D 3.343	83/05/21 11:01:51	eric	362	361	00000/00000/00005
Miscellaneous changes for PDP-11's.
Always send to a login name before a full name.

D 3.342	83/05/20 11:50:16	eric	361	360	00000/00000/00005
MRs:	238
MRs:	239
Don't stack processes when VRFY fails.
Give an error message on multiple RCPT commands with a bad address.

D 3.341	83/05/18 11:57:09	eric	360	359	00000/00000/00005
Change WKTIMEFACT to be negative to force failing jobs to the end of the
queue rather than to the beginning, giving better overall performance --
as noted by Jay Lepreau.  Also, clean up the format of the mailq output.

D 3.340	83/05/07 11:28:37	eric	359	358	00000/00000/00005
MRs:	237
Pass the size of the buffer to myhostname.

D 3.339	83/05/04 11:29:53	eric	358	357	00000/00000/00005
MRs:	235
fix botch in 3.338 -- arguments reversed

D 3.338	83/05/04 11:16:29	eric	357	356	00000/00000/00005
MRs:	235
Don't send domain-based UUCP addresses to UGLYUUCP hosts

D 3.337	83/05/01 14:27:55	eric	356	355	00000/00000/00005
MRs:	234
avoid core dumps on messages with very long header fields

D 3.336	83/04/30 15:14:51	eric	355	354	00000/00000/00005
lint

D 3.335	83/04/23 12:54:57	eric	354	353	00000/00000/00005
MRs:	230
Don't put Resent-*: lines in queue file to prevent outputing them
inappropriately in the sent message.

D 3.334	83/04/19 19:20:44	eric	353	352	00000/00000/00005
MRs:	228
Fix bug causing convtime to loop forever on "w" (week) specification.

D 3.333	83/04/17 17:19:04	eric	352	351	00000/00000/00005
MRs:	199
MRs:	218
MRs:	221
MRs:	224
put true current time on SMTP greeting message; drop >From hack (this belongs
in the mailer if needed); fix bogus errno problems (clear errno in many
places); fix *(0) problems

D 3.332	83/03/26 14:26:43	eric	351	350	00000/00000/00005
MRs:	213
MRs:	214
MRs:	215
increase buffer size in smtpmessage to avoid overflows; rewrite again with
ruleset three after appending from domain so that we can add the bracket
punctuation; changes from MRH for USG UNIX 5.0

D 3.331	83/03/19 14:25:33	eric	350	349	00000/00000/00005
MRs:	211
Don't have mail from su'ed people come from "daemon"

D 3.330	83/03/19 13:14:27	eric	349	348	00000/00000/00005
MRs:	147
Fix botch of putting incorrect full names on some addresses (usually
UNIX-to-UNIX Copy)

D 3.329	83/03/12 18:05:51	eric	348	347	00000/00000/00005
MRs:	196
take ECONNRESET (Connection reset by peer) as a temporary error during
connection initiation.

D 3.328	83/03/12 17:41:55	eric	347	346	00000/00000/00005
MRs:	208
Check for I/O error on close of temp file -- i.e., last buffer flush

D 3.327	83/03/12 15:40:05	eric	346	345	00000/00000/00005
MRs:	207
Don't artifically add a "from: $q" when running the queue -- this seems
to cause the from line in the qf file to be ignored.

D 3.326	83/03/10 00:48:02	eric	345	344	00000/00000/00005
MRs:	206
Delete colon on front of aliases

D 3.325	83/03/08 19:22:48	eric	344	343	00000/00000/00005
MRs:	205
Pass the delimiter character to parseaddr so that we can correctly
decide between comma and space

D 3.324	83/03/07 09:55:13	eric	343	342	00000/00000/00005
MRs:	203
Don't pass EX_TEMPFAIL code out (since we have already taken responsibility
for delivering the message anyway).

D 3.323	83/03/07 09:25:01	eric	342	341	00000/00000/00005
MRs:	202
Handle OLDSTYLE addresses properly with the -t flag; notice that tabs also
delimit addresses (as well as spaces)

D 3.322	83/03/06 16:30:29	eric	341	340	00000/00000/00005
MRs:	201
Handle messages with "Resent-xxx:" header lines properly (basically mark
the message as being resent); ignore incoming From: lines if their text
exactly matches our machine-readable address exactly -- this causes the
full name to be added when called from (e.g.) MH.  Both of these mode are
to improve interaction with MH.

D 3.321	83/03/05 17:39:11	eric	340	339	00000/00000/00005
MRs:	200
Ignore delimiters (e.g., commas) inside quotes

D 3.320	83/02/26 15:32:31	eric	339	338	00000/00000/00005
MRs:	193
Don't log message-id mapping and from person in a queue run

D 3.319	83/02/26 15:06:05	eric	338	337	00000/00000/00005
MRs:	162
map host name for $w macro to upper case -- this is more conventional

D 3.318	83/02/26 11:51:43	eric	337	336	00000/00000/00005
MRs:	193
log queueid <=> message-id correspondence

D 3.317	83/02/25 21:20:20	eric	336	335	00000/00000/00005
MRs:	181
More attempts at insuring that everything goes through ruleset 4

D 3.316	83/02/24 20:03:04	eric	335	334	00000/00000/00005
MRs:	181
arrange to cleanup addresses using ruleset 4 before storing in ADDR struct

D 3.315	83/02/24 18:55:32	eric	334	333	00000/00000/00005
MRs:	162
MRs:	192
Fix bug in $w and $=w setup; truncate output to seven bits if the "limits"
flag is set to insure that nothing can be sent that looks like TELNET
protocol

D 3.314	83/02/20 12:29:15	eric	333	332	00000/00000/00005
MRs:	190
Avoid loops in the auto-closedown code

D 3.313	83/02/19 14:42:12	eric	332	331	00000/00000/00005
MRs:	188
Avoid core dumps on very long addresses in headers.

D 3.312	83/02/18 13:11:57	eric	331	330	00000/00000/00005
MRs:	183
Improve host verification -- put something in even if no HELO command given

D 3.311	83/02/18 12:44:13	eric	330	329	00000/00000/00005
MRs:	183
MRs:	185
MRs:	187
Add HELO hostname verification; give an error if a message terminates with
EOF rather than dot in SMTP mode; put in a catch for "Not owner" error on
connect -- see if we can find anything funny.

D 3.310	83/02/13 16:23:03	eric	329	328	00000/00000/00005
MRs:	182
fix clrbitmap to really clear the entire bitmap

D 3.309	83/02/10 10:00:49	eric	328	327	00000/00000/00005
MRs:	176
avoid core dump on return receipt processing; clean up return receipt
message.

D 3.308	83/02/08 10:50:24	eric	327	326	00000/00000/00005
MRs:	167
MRs:	172
MRs:	173
MRs:	174
clean up problems when QUEUE compile flag not defined; clean up hostname
code to run on non-4.1c systems; fix use of -oeq (quiet on errors) flag.

D 3.307	83/02/03 10:46:02	eric	326	325	00001/00001/00004
MRs:	160
Change version stamp to have no date (so that it looks nice as $v/$V)

D 3.306	83/02/03 10:29:15	eric	325	324	00000/00000/00005
MRs:	160
$e macro is now SMTP entry message (so that configuration version can
be included).

D 3.305	83/02/03 07:54:10	eric	324	323	00000/00000/00005
MRs:	168
Make mailer size limits a per-mailer parameter (M field in the mailer
descriptor)

D 3.304	83/02/02 12:53:10	eric	323	322	00000/00000/00005
MRs:	165
implement classes and mailer flags as a bit map; define new class 'w'
as the set of all hosts we are known by

D 3.303	83/02/01 20:47:34	eric	322	321	00000/00000/00005
MRs:	166
don't call printqueue() if queueing is turned off

D 3.302	83/01/18 20:38:09	eric	321	320	00000/00000/00005
pause() after reply error if 18.100 set -- so that Sam can try to track
down the state of the connection in the CMU-CS-A problem.

D 3.301	83/01/17 21:41:42	eric	320	319	00000/00000/00005
MRs:	158
Set the $w macro to the hostname if at all possible to allow generic
configuration tables.

D 3.300	83/01/17 12:31:10	eric	319	318	00000/00000/00005
MRs:	155
fix bug that caused "\r\n" string in TCP mailer definition to turn
into something else -- causing all TCP connections to hang.

D 3.299	83/01/17 09:32:18	eric	318	317	00000/00000/00005
MRs:	154
define a newline in "nullmailer" so that queue files get written with
newlines between the "H" lines.

D 3.298	83/01/16 22:08:47	eric	317	316	00000/00000/00005
put in socket debugging on a debug flag to help Sam find the CMU-CS-A
problem -- this probably won't work with early 4.1c systems.

D 3.297	83/01/16 17:24:29	eric	316	315	00000/00000/00005
MRs:	153
make the maximum number of simultaneous SMTP connections an option

D 3.296	83/01/16 13:14:04	eric	315	314	00000/00000/00005
MRs:	152
Change mailer spec to have labelled fields for future expansion.
**** This delta invalidates previous configuration files ****

D 3.295	83/01/15 17:40:49	eric	314	313	00000/00000/00005
MRs:	149
be sure everything gets appropriately externalized; canonname goes away,
since everything is now done by remotename

D 3.294	83/01/12 08:56:51	eric	313	312	00000/00000/00005
MRs:	144
fix date format to be truly RFC822

D 3.293	83/01/09 15:43:11	eric	312	311	00000/00000/00005
MRs:	139
fix CRLF problem when sending SMTP commands; make newstr() and crlf()
into macros

D 3.292	83/01/08 19:54:37	eric	311	310	00000/00000/00005
MRs:	143
release daemon resources when running the queue

D 3.291	83/01/08 13:49:08	eric	310	309	00000/00000/00005
MRs:	142
be sure to open a new transcript on every queue run; when returning
an SMTP transcript, only send the last half (i.e., that part which
describes the sending half).

D 3.290	83/01/06 18:21:54	eric	309	308	00000/00000/00005
MRs:	141
allow user-setable error codes in "error" mailer

D 3.289	83/01/06 18:09:01	eric	308	307	00000/00000/00005
MRs:	140
fix a botch in argument processing such that it used the last flag as
a recipient if there were no other recipients (e.g., with "-t")

D 3.288	83/01/06 12:05:54	eric	307	306	00000/00000/00005
MRs:	132
fix (one more time!) long line wrapping.....

D 3.287	83/01/06 10:46:17	eric	306	305	00000/00000/00005
MRs:	139
split up FULLSMTP flag -- now have "X" (M_XDOT, use hidden dot algorithm),
"p" (M_FROMPATH, use reverse-path in MAIL FROM:<> command), and "L"
(M_LIMITS, enforce SMTP line limits).  I would like to change the format
of the mail defn one more time to make named fields so that it would
be more extensible.

D 3.286	83/01/05 20:30:10	eric	305	304	00000/00000/00005
MRs:	132
fix folded line output

D 3.285	83/01/05 17:57:11	eric	304	303	00000/00000/00005
MRs:	138
curiouser and curiouser.....  a read error on the reply from a QUIT will
cause another QUIT command, and thus an fclose(NULL).  Some sites seem
to be sending a 421 Shutting down and then closing without waiting for
the QUIT, so this happens....

D 3.284	83/01/05 10:19:04	eric	303	302	00000/00000/00005
MRs:	138
improve error recovery for bizarre SMTP cases; unfortunately CMU has
decided to start working again, so the strangest parts are untested.

D 3.283	83/01/04 19:53:55	eric	302	301	00000/00000/00005
MRs:	137
externalize $g macro using ruleset 4 also

D 3.282	83/01/04 18:52:08	eric	301	300	00000/00000/00005
MRs:	136
Don't try to drop our controlling TTY except when we are starting up a
daemon; this can cause UUCP jobs to hang waiting for carrier on a dialin
line (completely contrary to the intent).  Many thanks to Keith Sklower
for pointing this one out.  However, that damned parameter to disconnect()
has reappeared!

D 3.281	83/01/04 17:45:25	eric	300	299	00000/00000/00005
MRs:	135
Ignore SIGPIPE early (in main rather than in deliver) so that rude hosts
that close the connection early don't cause us to die -- in particular,
CMU-CS-A (nee CMU-10A) was doing this after the DATA command; the problem
went away quite mysteriously, so I have no idea why it happened

D 3.280	83/01/04 13:10:35	eric	299	298	00000/00000/00005
MRs:	129
MRs:	134
take EHOSTDOWN as a temporary failure; change the format of "deferred"
message to be consistent with other messages.

D 3.279	83/01/04 10:58:55	eric	298	297	00000/00000/00005
MRs:	133
fix botch that causes sendmail to core dump when there are large
numbers of requests

D 3.278	83/01/04 10:04:41	eric	297	296	00000/00000/00005
MRs:	132
Detect an internal error that seems to be occuring

D 3.277	83/01/03 18:01:33	eric	296	295	00000/00000/00005
MRs:	129
give more useful error messages (including the errno that caused
a temporary failure); reflect these messages out to mailq

D 3.276	83/01/03 14:02:28	eric	295	294	00000/00000/00005
MRs:	137
have canonname pass the address through ruleset four also

D 3.275	83/01/03 13:44:39	eric	294	293	00000/00000/00005
MRs:	131
take ENETUNREACH (Network unreachable) as a transient error -- this
probably isn't usually right, but it seem safer in the long run....

D 3.274	83/01/03 13:19:37	eric	293	292	00000/00000/00005
MRs:	129
print "request" instead of "requests" if there is only one request

D 3.273	83/01/03 13:03:01	eric	292	291	00000/00000/00005
MRs:	130
fix bug in commaize so that it won't core dump on every message (sigh);
process timeouts like normal errors so that they will get delivered to
the correct address (i.e., the Errors-To: address).

D 3.272	83/01/03 11:28:04	eric	291	290	00000/00000/00005
MRs:	129
fix botch in reading the queue that caused it to throw away the last
entry it read -- this was particulary obvious with only one entry in
the queue.

D 3.271	83/01/02 15:33:36	eric	290	289	00000/00000/00005
MRs:	124
Take errno 65, "Host is unreachable", to be a recoverable error

D 3.270	83/01/02 14:27:32	eric	289	288	00000/00000/00005
MRs:	129
clean up output of mailq mode

D 3.269	83/01/01 21:25:35	eric	288	287	00000/00000/00005
MRs:	123
Put CRLF on all lines in user SMTP.  Pass this info down to
innumerable routines.

D 3.268	83/01/01 18:14:56	eric	287	286	00000/00000/00005
MRs:	122
Use a more resilient algorithm to handle the special case of an SMTP
connection that is going down spontaneously (with a 421 reply code)

D 3.267	83/01/01 16:02:43	eric	286	285	00000/00000/00005
MRs:	128
tag syserr's specially in the log so they can be grep'ed out easily

D 3.266	82/12/30 17:30:57	eric	285	284	00000/00000/00005
MRs:	114
don't bother timing out on *.cf and qf* file reads -- they are
really very vanilla

D 3.265	82/12/30 16:57:13	eric	284	283	00000/00000/00005
MRs:	113
close files 3 through 20 before startup to insure sufficient resources

D 3.264	82/12/29 17:39:27	eric	283	282	00000/00000/00005
MRs:	112
don't htons(sp->s_port) in makeconnection

D 3.263	82/12/24 08:14:55	eric	282	281	00000/00000/00005
MRs:	108
Change parse to parseaddr for BB&N TCP/IP implementation; clean up
comments in daemon.c to simplify the poor alternate protocol type.

D 3.262	82/12/14 19:23:23	eric	281	280	00000/00000/00005
MRs:	106
fix freeze mode to do something more interesting that refreezing (over
and over and over and .......)

D 3.261	82/12/14 16:57:16	eric	280	279	00000/00000/00005
MRs:	105
Add "print mail queue" mode (-bp flag or call as "mailq")

D 3.260	82/12/13 18:25:22	eric	279	278	00000/00000/00005
MRs:	085
The routine "sendto" is now a system call (yeuch!); change our sendto
to "sendtolist"

D 3.259	82/12/13 17:47:27	eric	278	277	00000/00000/00005
MRs:	085
NEW-IPC: convert to 4.1c

D 3.258	82/12/09 19:18:32	eric	277	276	00000/00000/00005
MRs:	036
MRs:	053
MRs:	064
Fix a bug triggered when we add the sender's "@domain" to an address
in the message body that is missing a domain -- we forgot to null
terminate the list.

D 3.257	82/12/09 11:19:09	eric	276	275	00000/00000/00005
MRs:	081
MRs:	095
MRs:	103
log read timeouts as syserrs; be able to match the inverse of a class
using the $~x syntax; don't add a full name during network forward operations

D 3.256	82/12/05 13:46:29	eric	275	274	00000/00000/00005
MRs:	092
Clear the envelope in the child in server SMTP to insure that our oh so
helpful parent doesn't delete our transcript; move the transcript and
temporary file pointers into the envelope; pass the envelope to other
routines in the holy war against global variables; split off envelope
routines from main.c to envelope.c

D 3.255	82/11/28 16:00:50	eric	274	273	00000/00000/00005
implement SMTP auto-shutdown on 421 codes; clean up some error processing
items, particularly in SMTP; don't reinstantiate error message bodies after
queueing; other minor changes.  This is all cleanup from 3.253.

D 3.254	82/11/28 10:22:20	eric	273	272	00000/00000/00005
fix a number of problems left over from yesterday's delta.  The big
triumph is being able to delete the parameter from disconnect().

D 3.253	82/11/28 00:22:21	eric	272	271	00000/00000/00005
Many changes resulting from a complete code readthrough.  Most of these
fix minor bugs or change the internal structure for clarity, etc.  There
should be almost no externally visible changes (other than some cleaner
error message printouts and the like).

D 3.252	82/11/24 18:44:28	eric	271	270	00000/00000/00005
lint it

D 3.251	82/11/24 17:15:30	eric	270	269	00000/00000/00005
MRs:	026
run SMTP jobs in a subprocess so that multiple jobs will work.  This
delta also changes the envelope data structure so that flags are in
a bit map, and adjusts some of the semantics.  The transcript is now
local to an envelope.  A bunch of old code is deleted.  A serious bug
was fixed in the "run in background" code.  Etc., etc.

D 3.250	82/11/21 17:19:05	eric	269	268	00000/00000/00005
MRs:	026
preliminary hacks for multiple SMTP transactions per connection: make
assignment of multiple queue id's more efficient, make prefixes two
characters (e.g., qfAA99999), pass the file name to freeze and thaw,
add a mailer flag saying we are talking to one of our own kind.

D 3.249	82/11/20 12:43:29	eric	268	267	00000/00000/00005
MRs:	083
Accept user@[net.host.logical.imp] syntax

D 3.248	82/11/18 21:33:38	eric	267	266	00000/00000/00005
MRs:	080
Correctly run "sendmail -q" (had a reversed condition on QueueIntvl)

D 3.247	82/11/18 17:54:23	eric	266	265	00000/00000/00005
MRs:	079
Fix bug causing loops in sendto if there are scanner errors in addresses

D 3.246	82/11/18 08:56:27	eric	265	264	00000/00000/00005
MRs:	060
MRs:	073
disconnect input, output, signals, etc. when running in background

D 3.245	82/11/17 09:36:47	eric	264	263	00000/00000/00005
MRs:	060
split operation mode ("-bx" flag) and delivery mode ("d" option)
so that operation mode can apply to SMTP/daemon connections also.

D 3.244	82/11/14 15:34:59	eric	263	262	00000/00000/00005
MRs:	075
Explicitly inherit e_oldstyle from BlankEnvelope to MainEnvelope

D 3.243	82/11/14 15:14:00	eric	262	261	00000/00000/00005
MRs:	074
fix quote processing on program mailers

D 3.242	82/11/14 12:26:17	eric	261	260	00000/00000/00005
MRs:	072
Don't give an error message on unknown options

D 3.241	82/11/13 18:07:59	eric	260	259	00000/00000/00005
MRs:	021
MRs:	066
MRs:	069
MRs:	069
MRs:	070
MRs:	071
Clean up argument structure (-I=>-bi, -Z=>-bz).  Add -bp stub to someday
print queue.  Move compile conf into conf.h (still some stuff in the
Makefile).  Assume -bp if called as "mailq" and -bi if called as
"newaliases".  Drop old directory hack -- send out compat code instead.
Don't rebuild the alias file automatically unless the D option is set.
Clean up compilation flags.

D 3.240	82/11/07 16:14:44	eric	259	258	00000/00000/00005
MRs:	041
make all mailer output go to the transcript if running server smtp; this
has the effect of making some significant changes in the output structure
to handle such things as -em and -as together

D 3.239	82/11/07 15:31:50	eric	258	257	00000/00000/00005
MRs:	063
don't flag errors in setting dangerous options when -C is used

D 3.238	82/11/05 13:12:52	eric	257	256	00000/00000/00005
MRs:	062
arrange to freeze the configuration file for quick startup -- this
technique requires reprocessing the argv so that flag settings don't
get lost, and is really rather of a hack.

D 3.237	82/11/04 15:22:16	eric	256	255	00000/00000/00005
MRs:	019
don't unlink qf & df files on ^C

D 3.236	82/11/04 13:27:28	eric	255	254	00000/00000/00005
MRs:	037
put header conditionals into the .cf file

D 3.235	82/11/03 11:18:06	eric	254	253	00000/00000/00005
MRs:	059
don't strip quotes off of addresses during prescan

D 3.234	82/11/03 10:34:13	eric	253	252	00000/00000/00005
MRs:	058
Put the temporary file mode on the F option

D 3.233	82/11/03 09:49:15	eric	252	251	00000/00000/00005
MRs:	049
Arrange to strip quote bits in message header; move SPACESUB into
conf.c; change SpaceSub to unquoted dot

D 3.232	82/11/03 09:00:33	eric	251	250	00000/00000/00005
MRs:	056
Arrange for queue.c to aapt to systems w/o new directory code

D 3.231	82/10/31 13:47:06	eric	250	249	00000/00000/00005
MRs:	032
MRs:	051
Call ruleset 4 after doing per-mailer translation to convert from
internal to external form.

D 3.230	82/10/28 17:41:15	eric	249	248	00000/00000/00005
MRs:	039
MRs:	027
Put the "@:@" trick on the "a" option.  Also, move TrustedUsers into
sendmail.h (it should have gone here in the first place!)

D 3.229	82/10/27 20:43:35	eric	248	247	00000/00000/00005
MRs:	044
strip out bcc: and resent-bcc: always

D 3.228	82/10/25 08:30:12	eric	247	246	00000/00000/00005
MRs:	040
Don't send closing protocol to SMTP if you haven't opened the
connection; this happens if checkcompat fails on the only recipient.

D 3.227	82/10/22 09:02:44	eric	246	245	00000/00000/00005
MRs:	023
part of a heuristic hill climbing algorithm to minimize the number of
IPC bugs that come crawling over me.

D 3.226	82/10/16 15:24:52	eric	245	244	00000/00000/00005
MRs:	027
Put list of login names able to use -f in the .cf file.

D 3.225	82/10/16 14:43:47	eric	244	243	00000/00000/00005
MRs:	007
Arrange for a wizards password (the W option).  The SMTP KILL command
may only be issued if this option is set.  This delta adds the "WIZ"
command, and changes _KILL => KILL, _DEBUG => DEBUG, _VERBOSE => VERB,
and _SHOWQ => SHOWQ.

D 3.224	82/10/16 13:43:28	eric	243	242	00000/00000/00005
MRs:	024
Fix debug statement in sendall

D 3.223	82/10/16 13:22:22	eric	242	241	00000/00000/00005
MRs:	010
Arrange for MD_FORK to work well with -v so that we can use it as the
default.  This was really a separate bug, but I have just lumped it in
here -- the problem came up with stty tostop.

D 3.222	82/10/13 21:55:07	eric	241	240	00000/00000/00005
MRs:	023
Don't interrupt the process doing accepts (have a separate process
running the queue) to avoid a large pile of 4.1[abc] bugs.  When
something more stable comes out, I will change this.

D 3.221	82/10/13 18:43:28	eric	240	239	00000/00000/00005
MRs:	022
Don't interpret <, >, (, or ) while in QST (quote state) during prescan

D 3.220	82/10/11 09:51:43	eric	239	238	00000/00000/00005
MRs:	016
have auto-queueups print a polite message in the log (rather than an
obnoxious "Temporary Failure").  Adds a new routine "logdelivery".

D 3.219	82/10/09 21:02:27	eric	238	237	00000/00000/00005
MRs:	005
force a queuename in returntosender to insure that everyone has a
queue name.

D 3.218	82/10/09 20:27:42	eric	237	236	00000/00000/00005
MRs:	003
Lock out interrupts during alias rebuild; add an alias "@:@" after
rebuild that we can check for to insure that the alias file is up to
date.  Times out after five minutes (is this reasonable on ucbvax?)
and forces a rebuild.  It shouldn't matter in the long run if two
people rebuild, so I think this is the right thing to do.

D 3.217	82/10/09 19:06:31	eric	236	235	00000/00000/00005
MRs:	002
Handle Reverse-Path in some reasonable way.  You must put the 'X'
flag (M_FULLSMTP) in the local mailer's flags to get this line.

D 3.216	82/10/09 18:11:21	eric	235	234	00000/00000/00005
MRs:	013
print a status indication when a message is autoqueued, either from
a -bq flag or a NoConnect

D 3.215	82/10/09 17:22:35	eric	234	233	00000/00000/00005
MRs:	008
Force -v override of deferred connect

D 3.214	82/10/09 17:10:37	eric	233	232	00000/00000/00005
MRs:	004
Only put the "from" person on the error queue if an error actually
occured -- prevents many "duplicate supressed" messages, etc.

D 3.213	82/10/09 09:05:35	eric	232	231	00000/00000/00005
MRs:	001
implement an experimental forward-path algorithm by stripping off a
leading "@..." component and dropping it on the end.

D 3.212	82/10/07 08:52:47	eric	231	230	00000/00000/00005
try to come up with a workaround on some of the IPC problems relating to
interrupted accepts -- all this can go when everyone at Berkeley runs
unflakey IPC.  Also, never use ruleset three alone -- involves changing
canonname again.  Probably shouldn't do an auto-call of ruleset three now.

D 3.211	82/10/07 01:40:19	eric	230	229	00000/00000/00005
bother.... it takes time to close a socket

D 3.210	82/10/07 00:09:14	eric	229	228	00000/00000/00005
allow passing through multiple rewriting sets in test mode;
<ndir.h> => <dir.h> for maximum portability

D 3.209	82/10/06 11:45:40	eric	228	227	00000/00000/00005
add test mode (MD_TEST, -bt)

D 3.208	82/09/30 22:29:46	eric	227	226	00000/00000/00005
workaround in daemon accept code for 4.1a kernel bug: if an accept
gets an EINTR and then a valid accept before the interrupt returns,
the connection will be lost forever (at least, as I understand it).

D 3.207	82/09/26 17:04:24	eric	226	225	00000/00000/00005
Put more configuration into setoption; merge some of the argv processing
with this; move configuration information out of conf.c into the .cf
file.  Since a lot of stuff will default to zero, a new .cf file is
required.

D 3.206	82/09/26 14:45:52	eric	225	224	00000/00000/00005
completely instantiate queue files for all mail, even if not needed;
fix a bug that would probably come up once every ten years in creating
the queue id.  should merge argv flag processing with option processing.

D 3.205	82/09/24 19:39:25	eric	224	223	00000/00000/00005
change option implementation; define a bunch of interesting options.

D 3.204	82/09/24 09:38:36	eric	223	222	00000/00000/00005
arrange to be able to define options; put precedences in .cf file;
send errors to an Errors-To: field; fix a serious bug causing mail
to not be delivered to anyone if there were any errors.

D 3.203	82/09/22 10:50:47	eric	222	221	00000/00000/00005
don't put commas in non-address fields; have -bq clean up its temp files

D 3.202	82/09/21 10:15:00	eric	221	220	00000/00000/00005
get rid of double error returns; improve verbose mode output to be
standard format (even though verbose mode isn't standard); output
queue files with commas in headers to insure that the oldstyle stuff
doesn't get confused.

D 3.201	82/09/18 20:37:46	eric	220	219	00000/00000/00005
fix botch where it believes that a list is !oldstyle just because
it has an alias expansion in it.

D 3.200	82/09/16 20:25:01	eric	219	218	00000/00000/00005
drop ":...;" stuff -- it screws it up aliases; pass parameters to
subroutines (this also adds $@ and $: features to subr calls); check
overflow of "tobuf" on smtp deliveries, which caused core dumps on
large mailing lists

D 3.199	82/09/12 22:17:30	eric	218	217	00000/00000/00005
accept ": ... ;" syntax for groups if !oldstyle

D 3.198	82/09/12 16:44:03	eric	217	216	00000/00000/00005
change inheritance for macros in envelopes; be able to canonicalize
non-domained names by appending domain from sender; call ruleset 3
explicitly before doing other rulesets; some general cleanup.

D 3.197	82/09/11 17:18:16	eric	216	215	00000/00000/00005
don't assign $s to be the sending host (this mucks up Received:
lines) -- instead just use HELO messages; chdir into queue directory
and make all pathnames relative; be more conservative in creating
queue id's -- in the event you get file table overflows, etc.

D 3.196	82/09/08 23:55:46	eric	215	214	00000/00000/00005
fix stupid bug in wait code to dispose of "Interrupted system call" message

D 3.195	82/09/08 22:13:20	eric	214	213	00000/00000/00005
fix a nasty botch in 3.194 that killed SMTP in daemon mode.   sigh......

D 3.194	82/09/08 21:20:05	eric	213	212	00000/00000/00005
try to avoid "Interrupted system call" on wait in deliver and on
writes; fix returned message to have proper sender; handle extra
mailer output more cleverly.

D 3.193	82/09/06 19:55:17	eric	212	211	00000/00000/00005
more hacking on oldstyle -- always assume sender fields are in new
style so that locally generated fields are edited correctly.

D 3.192	82/09/06 18:47:26	eric	211	210	00000/00000/00005
fix botch in "at"s in headers

D 3.191	82/09/06 18:24:40	eric	210	209	00000/00000/00005
fix botch in computing e_oldstyle

D 3.190	82/09/06 17:58:40	eric	209	208	00000/00000/00005
fix bug in scanner state machine that never let you out of quote state.

D 3.189	82/09/06 17:14:03	eric	208	207	00000/00000/00005
increase log level needed to print "entered" message.

D 3.188	82/09/06 17:00:23	eric	207	206	00000/00000/00005
user SMTP fixes to talk to ISI

D 3.187	82/09/06 16:24:51	eric	206	205	00000/00000/00005
install new state-driven scanner; make everyone use it, thus fixing
problems of quoted commas, etc.

D 3.186	82/09/05 18:08:58	eric	205	204	00000/00000/00005
change $g processing from a macro substitution per mailer to two sets
of rewriting rules per mailer -- one each for sender and recipient
fields; convert to NBS standard on Return-Receipt-To: and Precedence:
fields; clean up From: processing code and generalize it to all
sender fields; tune debugging code; clean up canonname.
**** This delta invalidates previous configuration files ****

D 3.185	82/09/05 11:48:27	eric	204	203	00000/00000/00005
add $* to match zero or more and $> to make a "subroutine" call; stick
in initial hooks for per-mailer rewriting; improve diagnostics in readcf,
including number lines; increase the number of rewriting sets.

D 3.184	82/09/01 10:24:00	eric	203	202	00000/00000/00005
log entering uid & pid; allow CANONUSER ($:) to abort a rewriting set.

D 3.183	82/08/31 17:46:56	eric	202	201	00000/00000/00005
increase MAXMAILERS to 25

D 3.182	82/08/31 10:05:56	eric	201	200	00000/00000/00005
clean up reply code processing some more; in particular, give more
detail in many messages.  process connection failures correctly.

D 3.181	82/08/29 23:31:35	eric	200	199	00000/00000/00005
handle comments and quotes in headers; still doesn't deal with
backslashes however -- prescan should be cleaned up to deal with
this case.  it turns out there are many bugs in prescan; it should
be extensively rewritten.

D 3.180	82/08/29 17:32:49	eric	199	198	00000/00000/00005
fix serious botch in SMTP reply code delta; change a bunch of
"Internal error" codes to a new "Remote protocol error" -- so that
I don't get blamed for things that other systems do.

D 3.179	82/08/29 16:52:31	eric	198	197	00000/00000/00005
do a better job at interpreting SMTP reply codes

D 3.178	82/08/29 15:52:20	eric	197	196	00000/00000/00005
give 554 message on syserr (instead of 451) if errno == 0; as it
was some permanent errors appeared transient

D 3.177	82/08/27 18:01:25	eric	196	195	00000/00000/00005
hack crackaddr to strip blanks off the end of a cracked address; this
happens because "u at h" becomes "$g  " (with two spaces at the end).
this doesn't solve the general case (e.g., "u at h (me)" comes out as
"$g   (me)", with three spaces), but does handle the ugliest case.
besides, the "at" syntax is supposed to go away.

D 3.176	82/08/27 16:01:58	eric	195	194	00000/00000/00005
simplify timeout code; allow multiple simultaneous queue runs so that
large messages don't freeze things up; fix EINTR problem in sfgets;
clean up canonname to be really correct; lots of misc. cleanup

D 3.175	82/08/27 11:11:40	eric	194	193	00000/00000/00005
release e_id after fork in daemon code; ignore events scheduled by
another process

D 3.174	82/08/25 23:18:24	eric	193	192	00000/00000/00005
assign a new queue id to all jobs coming in via TCP

D 3.173	82/08/25 21:22:10	eric	192	191	00000/00000/00005
sigh....  more fun and games in the daemon code to try to guess right.

D 3.172	82/08/25 19:44:33	eric	191	190	00000/00000/00005
recreate a socket after every failed accept()

D 3.171	82/08/25 16:19:23	eric	190	189	00000/00000/00005
apply ruleset 4 to rewrite addresses in the body of the message; fix
a clock.c bug that caused it to lose events; more time cleanup.

D 3.170	82/08/25 11:21:21	eric	189	188	00000/00000/00005
clean up time manipulation to always be current; assign job
id's earlier to make sure there is always one with a message.

D 3.169	82/08/25 10:46:27	eric	188	187	00000/00000/00005
log locked files in queuer; don't mistakenly start up two runqueue's

D 3.168	82/08/24 19:55:39	eric	187	186	00000/00000/00005
put queueup log messages on a higher logging level

D 3.167	82/08/24 19:41:32	eric	186	185	00000/00000/00005
log more info (on log level 11); try to detect wild accept loops;
some minor cleanup and debugging checks; fix NoConnect option to;
only apply to expensive mailers (regardless of sendqueue order)

D 3.166	82/08/24 10:27:46	eric	185	184	00000/00000/00005
clean up event handling so that events that do longjmp's don't turn
off all future events; diagnose reentry of main().

D 3.165	82/08/23 11:59:50	eric	184	183	00000/00000/00005
clean up queueing; log time in queue.
**** This delta invalidates mqueue files ****

D 3.164	82/08/23 09:23:54	eric	183	182	00000/00000/00005
allow continuation lines in .cf file (particularly in headers)

D 3.163	82/08/22 23:07:11	eric	182	181	00000/00000/00005
change $i to $j; $i is now queue id; put "Received:" format in .cf
file; minor cleanup

D 3.162	82/08/22 19:03:19	eric	181	180	00000/00000/00005
assign a unique id to each transaction that can be determined from
the queue file name.

D 3.161	82/08/21 17:54:37	eric	180	179	00000/00000/00005
move <> and forward path processing to .cf file; increase MAXATOMS
since some "comment" information may now be part of the address.
**** this installation requires a new sendmail.cf file ****

D 3.160	82/08/20 20:35:15	eric	179	178	00000/00000/00005
time stamp the SMTP greeting message; increase the buffer size in
syslog to prevent core dumps.

D 3.159	82/08/17 20:45:43	eric	178	177	00000/00000/00005
change Mail-From: to Received: for new SMTP spec (RFC821); handle
folded lines in queue files correctly.

D 3.158	82/08/17 16:19:10	eric	177	176	00000/00000/00005
rework header processing: do special purpose header munging in a
separate routine so that the queue run can do it also; parse From lines
in a fancy way, extracting the address part and turning it into a $g
macro; avoid reading and processing core files that end up in the queue
directory; check the queue directory name for legality; fix the verify
(-bv) option.

D 3.157	82/08/15 17:35:39	eric	176	175	00000/00000/00005
mark From: lines with the H_FROM bit; make VRFY work by adding the
QuickAbort flag; handle headers more cleverly in queue files -- this
seems to work, but I am frankly nervous.  Note: this version
represents a flag day!-- old queues will not process properly.

D 3.156	82/08/15 11:58:25	eric	175	174	00000/00000/00005
output class rather than priority in log; accept zero intervals in
event scheduling; allow continuation lines in queue files (particularly
for headers); don't diagnose inappropriate errors when accepting a
connection; fix bug when reprocessing addresses that put garbage in
the header; some misc. debugging info (-d14 => commaize)

D 3.155	82/08/08 21:15:44	eric	174	173	00000/00000/00005
make "sleep" work correctly even in the face of other events; clean
up the queue processing: child queue runs now go away when done.  more
debugging logging is needed to verify that this works right though.

D 3.154	82/08/08 17:05:19	eric	173	172	00000/00000/00005
move remotename() from deliver.c to parse.c; change default log level
to 9; put error versus success delivery on different log levels;
diagnose overlength lines in headers; more general event mechanism;
initial implementation of canonname; don't diagnose link errors in
queue, since another daemon could have grabbed it legitimately;
fix a problem in VRFY on bad addresses

D 3.153	82/08/08 01:00:25	eric	172	171	00000/00000/00005
change debug level to a debug vector; add levels on logging (and the
-L flag); change logging to be by message-id; elevate message-id;
some lint-type cleanup

D 3.152	82/08/07 11:13:15	eric	171	170	00000/00000/00005
Fix address rewriting so that multiple spaces and tabs work properly

D 3.151	82/07/31 16:57:49	eric	170	169	00000/00000/00005
don't give error on ETIMEDOUT on accept call; print errno properly
in syserr()

D 3.150	82/07/31 12:56:56	eric	169	168	00000/00000/00005
By default, just queue up the mail for most mailers and deliver
from the queue.  This avoids the wild process problem in netnews
(or so we hope).

D 3.149	82/07/27 23:09:04	eric	168	167	00000/00000/00005
clean up semantics of daemon mode to facilitate use of other IPC;
move some code to main.c because it is not part of creating a
connection; if you can't create a socket assume there is another
sendmail running and exit; improve the verbose information in an
SMTP connection to make it more obvious which messages went which
way; don't attempt delivery if you get an error reading the queue file.

D 3.148	82/07/25 13:11:20	eric	167	166	00000/00000/00005
clean up error handling

D 3.147	82/07/22 01:23:15	eric	166	165	00000/00000/00005
new version of syslog that uses 4.2 IPC

D 3.146	82/07/20 19:39:15	eric	165	164	00000/00000/00005
be smarter about when to rerun the queue when you are in repeated
queue mode.

D 3.145	82/07/14 11:19:27	eric	164	163	00000/00000/00005
disconnect sendmail from the controlling tty in daemon mode.

D 3.144	82/07/14 11:00:33	eric	163	162	00000/00000/00005
arrange for MotherPid to be correct in daemon mode

D 3.143	82/07/14 10:46:00	eric	162	161	00000/00000/00005
fork automatically in daemon mode (but only if no debugging)

D 3.142	82/07/14 09:25:11	eric	161	160	00000/00000/00005
don't accidently unlink a null pointer in finis(); change the way
it decides whether to remove the temp file (you don't want to if you
are queueing it up); fix a bug in Apparently-To: that caused it to
output both pre- and post-expanded names (only output pre-expanded)

D 3.141	82/07/05 20:56:24	eric	160	159	00000/00000/00005
flush fatal error flag before returning error messages to avoid error
message loop; improve debugging: flush transcript before doing things
that might take a long time, add some info to debug messages; extend
configuration on timeouts so that text of error message is in conf.c

D 3.140	82/07/05 18:49:49	eric	159	158	00000/00000/00005
add "Apparently-To:" field if no recipients are in the header.

D 3.139	82/07/05 13:21:21	eric	158	157	00000/00000/00005
count Mail-From: lines to get a hop count, giving error as
appropriate after collection; don't attempt delivery of message if
errors occur before or during collection; fix a bug in -as mode
(non-daemon SMTP).

D 3.138	82/07/05 12:37:23	eric	157	156	00000/00000/00005
check for errors before opening a daemon connection

D 3.137	82/07/05 12:22:02	eric	156	155	00000/00000/00005
split off clock stuff from util.c so that vacation will compile

D 3.136	82/07/05 12:02:53	eric	155	154	00000/00000/00005
put timeouts on net reads

D 3.135	82/07/02 20:52:46	eric	154	153	00000/00000/00005
arrange to not lose temporary queue control files (tf files)
if you interrupt a queue run process.

D 3.134	82/07/02 10:00:16	eric	153	152	00000/00000/00005
be paranoid about resetting the "FatalErrors" flag -- always reset
after forking for daemon or queue mode.  This will prevent false error
return delivery.

D 3.133	82/07/02 09:43:39	eric	152	151	00000/00000/00005
take special care in outputing error messages to the transcript;
a little bit of paranoia never hurt a mail system.

D 3.132	82/06/30 22:39:22	eric	151	150	00000/00000/00005
open a separate transcript in the child of a daemon.

D 3.131	82/06/26 14:53:40	eric	150	149	00000/00000/00005
clean up error messages on SMTP temporary failures resulting from
failure to connect properly.

D 3.130	82/06/26 13:53:35	eric	149	148	00000/00000/00005
more debug information; fix dependencies in makefile

D 3.129	82/06/26 13:10:41	eric	148	147	00000/00000/00005
lint

D 3.128	82/06/26 12:33:39	eric	147	146	00000/00000/00005
take environment variable "NAME" to determine what your full name
is; this is overridden by -F.

D 3.127	82/06/26 11:56:51	eric	146	145	00000/00000/00005
add _kill command to SMTP.  this is probably dangerous in the
outside world.

D 3.126	82/06/25 19:38:24	eric	145	144	00000/00000/00005
add debugging information to server smtp code: _debug to set
Debug, _verbose to set verbose, and _showq to show the send queue
(already existant, just a name change)

D 3.125	82/06/23 12:12:19	eric	144	143	00000/00000/00005
check prescan return values in remotename

D 3.124	82/06/19 21:14:18	eric	143	142	00000/00000/00005
remember to put a newline at the end of the UGLYUUCP line

D 3.123	82/06/19 21:09:10	eric	142	141	00000/00000/00005
get rid of comment lines when storing rewriting rules

D 3.122	82/06/19 20:47:23	eric	141	140	00000/00000/00005
improve SMTP error reporting

D 3.121	82/06/18 11:58:31	eric	140	139	00000/00000/00005
believe host name on SMTP "HELO" line

D 3.120	82/06/17 10:44:38	eric	139	138	00000/00000/00005
fix botch in -Q flag (sets AliasFile rather than QueueDir)

D 3.119	82/06/16 14:51:16	eric	138	137	00000/00000/00005
allow connection refused as a temporary error; make sure there is
always someone to return the mail to on error

D 3.118	82/06/16 14:29:20	eric	137	136	00000/00000/00005
fix botch in outputing "recipient" type lines (e.g., To:)

D 3.117	82/06/07 23:53:26	eric	136	135	00000/00000/00005
allow multiple connections; call putline to output FULL_SMTP
lines (to limit line lengths, etc.); involves adding an asm.sed script
to the makefile

D 3.116	82/06/07 07:54:55	eric	135	134	00000/00000/00005
make transcripts verbose always; misc. message cleanup, etc.

D 3.115	82/06/07 07:06:13	eric	134	133	00000/00000/00005
strip out xlate stuff -- this belongs at a different level

D 3.114	82/06/06 23:13:08	eric	133	132	00000/00000/00005
avoid loops by not sending to owner-owner-* -- just send to
owner-owner instead

D 3.113	82/06/06 23:05:17	eric	132	131	00000/00000/00005
implement alias owner feature.  this actually works for any user.
basically, if the alias owner-xxx exists, errors sending to xxx will be
sent to that alias rather than to the sender.

D 3.112	82/05/31 19:03:47	eric	131	130	00000/00000/00005
make temp files the correct modes in all cases

D 3.111	82/05/31 18:49:50	eric	130	129	00000/00000/00005
pass lint.  notice that definitions in llib-lc have changed for
alarm() and sleep() calls {arg was unsigned, is now int}.

D 3.110	82/05/31 17:10:51	eric	129	128	00000/00000/00005
eliminate -V, -D, -p flags in favor of a single flag "-bx"
(be in mode x).

D 3.109	82/05/31 15:35:57	eric	128	127	00000/00000/00005
don't output SMTP/FTP error codes in transcript file

D 3.108	82/05/31 15:32:18	eric	127	126	00000/00000/00005
finish implementing envelopes.  it's not completely clear to me that
this is really the way to go, but it seems clearly better than what
i had before.  this delta includes many other minor changes, so it
should probably not be blithely removed.

D 3.107	82/05/30 10:25:35	eric	126	125	00000/00000/00005
add M_FULLSMTP (``X'' flag in .cf file) for eventual implementation
of full SMTP.  This version must support such garbage as line limits,
address length limits, return-path, etc.

D 3.106	82/05/29 20:00:30	eric	125	124	00000/00000/00005
allow the user with name "daemon" to send mail as anyone s/he wants.

D 3.105	82/05/22 02:05:48	eric	124	123	00000/00000/00005
add "junk mail" -- error responses are never returned.

D 3.104	82/05/22 01:38:07	eric	123	122	00000/00000/00005
add "envelopes" to contain the basic information needed as control
info for each message.  currently there is only one envelope -- this
being the obvious stupid conversion.  later there will be separate
envelopes for error messages, return receipts, etc.

D 3.103	82/05/20 17:46:07	eric	122	121	00000/00000/00005
add dfopen as a "determined fopen" -- it retries if it gets recoverable
errors.  we use it for returning mail (to dead.letter) and creating the
temp file.  the whole idea is to avoid dropping things on the floor on
heavily loaded systems.  this is untested, since it seems impossible to
fill up the inode or file tables on this VAX (but it works if the open
succeeds).

D 3.102	82/05/15 12:29:36	eric	121	120	00000/00000/00005
add the 'R' flag to mailers, saying to rewrite the recipient addresses
to be relative to the recipient.  This makes reply code easy, but
confuses user mail programs that are expecting to have to rewrite
recipient addresses.  In general, the receiving host must be "smart"
for this to work.

D 3.101	82/05/06 20:21:14	eric	120	119	00000/00000/00005
allow socket number on [IPC] connections -- eventually this
could be used to handle other low-level protocols.

D 3.100	82/03/27 20:15:12	eric	119	118	00000/00000/00005
delete neat remotename feature, because it doesn't work right in
UUCPland -- and breaks stupid hosts.  'Twill be fixed, I promise.  Time
must be spent figuring out how to define the civilized versus the
uncivilized world.

D 3.99	82/03/27 19:57:44	eric	118	117	00000/00000/00005
compensate for bug in getlogin: can return the empty string ("")
for certain error conditions rather than the NULL pointer.

D 3.98	82/03/22 22:37:54	eric	117	116	00000/00000/00005
continue the impossible task of tracking Bill Joy

D 3.97	82/03/22 22:10:44	eric	116	115	00000/00000/00005
more cleanup for new 4.2 system configuration and some more fixes
for vanilla V7

D 3.96	82/03/20 18:13:01	eric	115	114	00000/00000/00005
take % as an acceptable name terminator in GECOS field
("in-care-of") -- for TEF%UCSFCGL

D 3.95	82/03/20 16:12:27	eric	114	113	00000/00000/00005
16 bit changes -- should have no effect on VAX binaries to
speak of.

D 3.94	82/03/06 16:11:49	eric	113	112	00000/00000/00005
get queue scanning working correctly in conjunction with daemon mode

D 3.93	82/03/06 15:35:49	eric	112	111	00000/00000/00005
have daemon mode assume SMTP mode

D 3.92	82/03/06 15:08:24	eric	111	110	00000/00000/00005
give correct error message in SMTP if some of the addresses are not ok

D 3.91	82/03/06 14:52:22	eric	110	109	00000/00000/00005
arrange to be able to accept a connection from any host.

D 3.90	82/03/06 14:15:55	eric	109	108	00000/00000/00005
collapse special character processing into macro processing for
simplicity of code.

D 3.89	82/03/06 12:09:18	eric	108	107	00000/00000/00005
clean up error handling in IPC case; fix a minor bug in headers in
queueing code; make SMTP mail multi user per connect.

D 3.88	82/03/05 10:45:48	eric	107	106	00000/00000/00005
remove silly $U dependency in UGLYUUCP code

D 3.87	82/03/05 10:13:06	eric	106	105	00000/00000/00005
include direct connect on outgoing mail if the pathname is "[IPC]" --
this gives minimal number of processes for ethernet mail.

D 3.86	82/02/27 12:29:31	eric	105	104	00000/00000/00005
more work on after $g translate rewriting

D 3.85	82/02/27 11:37:42	eric	104	103	00000/00000/00005
implement "return receipt requested".

D 3.84	82/02/27 09:51:36	eric	103	102	00000/00000/00005
improve rewriting of "after $g translate" to correspond to the
real world.....

D 3.83	82/02/26 21:56:10	eric	102	101	00000/00000/00005
implement daemon mode

D 3.82	82/02/26 19:02:33	eric	101	100	00000/00000/00005
default to OldStyle headers -- this turns out to be needed so that it
will work right when running as a server.

D 3.81	82/02/22 19:59:16	eric	100	99	00000/00000/00005
some hacks to make the ethernet community happier -- .cf changes only

D 3.80	82/02/22 19:32:12	eric	99	98	00000/00000/00005
be much more clever about splitting up addresses when doing header
rewriting.  become NewStyle automatically based on heuristics; this
makes some other addresses work, although there are conceivably
sites that this could break.  Perhaps we should default to OldStyle?
I don't think this would break anything.

D 3.79	82/02/20 16:56:02	eric	98	97	00000/00000/00005
output names in a nice comma-separated fashion in messages -- this
opens up other possibilities

D 3.78	82/02/20 12:59:46	eric	97	96	00000/00000/00005
expand macros in rewriting rules early to allow multi-word macros to
be processed correctly.

D 3.77	82/02/20 12:12:09	eric	96	95	00000/00000/00005
add the -c flag, to cause sendmail to just queue messages that are for
mailers that are expensive; a later instantiation can come around and
send them in a batch.  Also, pass macro definitions through the
queueing code so that macros can be expanded later rather than sooner;
this is important for destination dependent macros such as $g.

D 3.76	82/02/04 20:31:21	eric	95	94	00000/00000/00005
add host aliasing; add -p flag.  this version doesn't yet know about
replacing the text of the host alias into the message however.  syntax
is grotty: "/hostmatch:%s@newhost" or whatever.

D 3.75	82/01/23 14:21:06	eric	94	93	00000/00000/00005
add M_UGLYUUCP flag; only catch names with leading slash as filenames;
let the user redefine their full name; don't pass -r or -f to uux.

D 3.74	82/01/10 21:57:33	eric	93	92	00000/00000/00005
cleanup from cbosgd (Mark Horton) testing; some internals, mostly
configuration.  This tries to make the configuration file (cf.m4) be
much more general, but I fear it is doomed to failure -- it may be
better to just tell people to roll their own.

D 3.73	82/01/05 09:59:19	eric	92	91	00000/00000/00005
fix botch in UGLYUUCP code -- gave "remote from <dest>" instead of
"remote from <source>"

D 3.72	82/01/01 18:39:43	eric	91	90	00000/00000/00005
know about all known Berknet host names so that we can handle the "."
notation happily in the full name representation.

D 3.71	82/01/01 18:27:15	eric	90	89	00000/00000/00005
send to dead.letter using sendto/recipient/deliver mechanism rather
than mailfile; this fixes a bug with the "from" name and seems like
a better abstraction.

D 3.70	81/12/06 12:39:10	eric	89	88	00002/00000/00003
cleanup so it will go through lint without any fancy grep -v's;
change the way SCCS Id's are handled.

D 3.69	81/12/05 14:14:06	eric	88	87	00000/00000/00003
insert SMTP "Mail-From:" line.

D 3.68	81/12/05 11:53:41	eric	87	86	00000/00000/00003
put the SMTP and queueing code on compilation flags so that sendmail
will fit on non-ID PDP-11's (ugh); put the ugly UUCP hack on a
compilation flag also to emphasize that it sucks eggs; makefile
cleanup.

D 3.67	81/11/27 21:37:05	eric	86	85	00000/00000/00003
this is a stupid hack to put "remote from <host>" lines on the From
lines going to UUCP mail.  someday i hope to rip out this stupidity.
it is triggered by any mailer named "uucp" -- it ought to be a special
flag, but i refuse to legitimize antique hacky mistakes.

D 3.66	81/11/22 19:17:45	eric	85	84	00000/00000/00003
Edit queue control files when running queue (assuming there are still
recipients left).  Also, modify the MsgPriority to be the absolute
number (not the Priority: value); this allows us to include aging into
the priority algorithm.

D 3.65	81/11/21 18:42:47	eric	84	83	00000/00000/00003
change the send queue to be only one queue instead of one per mailer.
this is slightly inefficient but simpler.  also, pass this queue
around so we can have multiple send queues.  this makes VRFY work.

D 3.64	81/11/21 16:38:29	eric	83	82	00000/00000/00003
convert to SMTP draft 3 -- finishing touches.  Punt on the VRFY/
EXPN commands for now; they aren't required anyhow.  Move the fullname
into the address structure so it can be inherited.

D 3.63	81/11/11 20:24:07	eric	82	81	00000/00000/00003
integrate user SMTP into sendmail itself.  If there is no $u arg
in the mailer argument list, SMTP is run.  This can be used directly
over the Ethernet or to any clever mailer.  We still need to edit
spooled control files to remove recipients that have been successfully
sent during queue processing.

D 3.62	81/11/08 13:00:21	eric	81	80	00000/00000/00003
know about SMTP over TCP.  The current SMTP user is not clever
enough to deal with multiple users at one host.  To fix this we will
have to speak some smart protocol between sendmail and the mailer --
maybe SMTP??

D 3.61	81/11/07 15:41:39	eric	80	79	00000/00000/00003
clean up to compile & work on ARPAVAX; move stat file to
/usr/lib/sendmail.st; fix bug in syserr with error codes

D 3.60	81/10/31 22:12:47	eric	79	78	00000/00000/00003
drop old NCP stuff for ARPANET handling; fix some bugs in error
messages with multiple recipients in SMTP; clean up error handling

D 3.59	81/10/27 12:24:51	eric	78	77	00000/00000/00003
experimental version combining queueing with daemon operation.
I'm sure this doesn't work -- if only because the wait()s are funny.

D 3.58	81/10/27 10:50:52	eric	77	76	00000/00000/00003
More queueing cleanup: implement timeouts (still one-stage),
properly implement priorities (didn't work before), and miscellaneous
cleanup.

D 3.57	81/10/26 14:22:34	eric	76	75	00000/00000/00003
Install new experimental queueing facility -- one stage timeout,
etc.  This version is still quite incomplete.  It needs to reorder
the queue after some interval, do two-stage timeout, take option
info from the queue file instead of the command line, read the
sender's .mailcf file, etc.  Some of this is useful for SMTP also.

D 3.56	81/10/23 19:38:09	eric	75	74	00000/00000/00003
Eliminate magic MN_LOCAL and MN_PROG; change q_mailer item in ADDRESS
to be pointer to mailer rather than index.

D 3.55	81/10/22 10:25:29	eric	74	73	00000/00000/00003
move stats file to /usr/lib; put location of sendmail.hf in conf.c

D 3.54	81/10/22 09:43:48	eric	73	72	00000/00000/00003
take fullname from /etc/passwd if Smtp mode and sender name
is indeed local.  Also accepts names more often; this is probably
a disaster for sender verification.

D 3.53	81/10/22 09:14:30	eric	72	71	00000/00000/00003
implement HELP and MRSQ -- MRSQ is a partial implementation
of old MTP -- in particular, To: fields in MAIL commands are not yet
implemented.  The "message" routine now takes first args of the form
"999-" to specify continuation.

D 3.52	81/10/20 11:36:24	eric	71	70	00000/00000/00003
clean up the SMTP stuff some more

D 3.51	81/10/19 22:27:17	eric	70	69	00000/00000/00003
implement SMTP mode -- doesn't support source routing or the
HELP command, and doesn't give the correct code on VRFY or forwarding.
Maybe someday....

D 3.50	81/10/17 16:58:04	eric	69	68	00000/00000/00003
initial prep to put in Daemon mode

D 3.49	81/10/12 10:04:19	eric	68	67	00000/00000/00003
throw in some "errno = 0;"'s to make syserr's more accurate;
take any -f flag if debug mode and uid==euid.

D 3.48	81/10/08 22:55:37	eric	67	66	00000/00000/00003
ignore interrupts and hangups while calling mailer.

D 3.47	81/10/08 09:13:30	eric	66	65	00000/00000/00003
fix botch in backup code during rewriting

D 3.46	81/10/06 19:12:57	eric	65	64	00000/00000/00003
change rewriting rules to use $N on RHS to match LHS, and include
$=X (class match) in the matching; this will allow us to match the
proposed "user.host@domain" syntax as well as the old syntax, by
putting the known domains into a class to disambiguate.

D 3.45	81/10/02 11:05:21	eric	64	63	00000/00000/00003
arrange to give the correct "from" person on error messages.

D 3.44	81/10/02 10:07:38	eric	63	62	00000/00000/00003
use ruleset 2 to rewrite names after the $g translate (to fix some
forwarding problems; do a read check in putmessage (just in case
the temp file disappeared); some general cleanup.

D 3.43	81/09/30 10:00:20	eric	62	61	00000/00000/00003
fix the <> syntax, convert "at" to "@" in from addresses also,
plus some minorness to get the multi-machine case going nicely.

D 3.42	81/09/29 18:22:25	eric	61	60	00000/00000/00003
change processing of From: person; basically, this mod deletes the
Original-From: line if redundant with the generated From: line, uses
the Original-From: line for the From: line if possible, and a host of
other such trivialities.

D 3.41	81/09/29 14:59:00	eric	60	59	00000/00000/00003
fix hash function computation for symbol table on 16-bit machines

D 3.40	81/09/28 19:17:29	eric	59	58	00000/00000/00003
enable aliases to self-reference themselves correctly.

D 3.39	81/09/24 10:30:44	eric	58	56	00000/00000/00003
changes to work under a V6 system -- mostly compilation flags

D 3.38.1.1	81/09/23 18:23:19	eric	57	56	00000/00000/00003
break some configuration into conf.h -- simplifies makefile changes
on different machines.  But there are still conditional libraries, so
this may not be a good idea.....

D 3.38	81/09/23 09:52:23	eric	56	55	00000/00000/00003
fix bug in file modes when mailing to files that didn't exist before;
add an 'F' line to the .cf file that will read class entries from
another file, given a scanf string to do the parsing.

D 3.37	81/09/22 13:24:25	eric	55	54	00000/00000/00003
fix problem with using macros in rules; change configuration to send
different flags when destined for arpanet vs. berknet hosts, etc.

D 3.36	81/09/22 11:33:29	eric	54	53	00000/00000/00003
finally make suppression of sender in mailing lists work -- this version
also works correctly on simple aliases.

D 3.35	81/09/21 18:49:53	eric	53	52	00000/00000/00003
fix suppression of from address in mailing lists.

D 3.34	81/09/20 10:46:32	eric	52	51	00000/00000/00003
don't let a mailer ever execute as root (that should solve the
security problems!) -- essentially just map root into someone else.
Adds yet another configuration variable (should this be in the .cf file?

D 3.33	81/09/16 20:04:26	eric	51	50	00000/00000/00003
take underscore as well as space to separate parts of a person's
full name.  Dot notation should be made to work someday also.

D 3.32	81/09/16 17:16:19	eric	50	49	00000/00000/00003
have .forward ownership stick harder than :include: ownership:
this prevents cretins from using writable root files for nastiness.
all this is pointless if /usr/lib/aliases is writable though....

D 3.31	81/09/16 16:39:44	eric	49	48	00000/00000/00003
be REALLY clever and inherit uid/gid from owner of :include:
files also...   solves problem of alias to non-secure file

D 3.30	81/09/16 16:24:19	eric	48	47	00000/00000/00003
last delta was too paranoid -- this increases the number of
cases that work (but is still safe -- I hope)

D 3.29	81/09/16 16:08:08	eric	47	46	00000/00000/00003
run as the user forwarding mail after a .forward operation;
fix a bug in .forwarding that caused recursive expansion;
restrict mail to programs, files, and with :include: for security
reasons.

D 3.28	81/09/14 12:41:28	eric	46	45	00000/00000/00003
check MAXATOM overflow; increase MAXATOM value

D 3.27	81/09/14 12:19:23	eric	45	44	00000/00000/00003
fix but in mailers that only take one address at a time

D 3.26	81/09/12 17:34:05	eric	44	43	00000/00000/00003
match on full name for local users

D 3.25	81/09/12 15:48:57	eric	43	42	00000/00000/00003
change From: processing to reduce the number of Original-From:
lines; change allocation of global variables.

D 3.24	81/09/07 14:12:48	eric	42	41	00000/00000/00003
fix up umask; allow setuid on files to take recipient owner;
factor some configuration info into conf.c

D 3.23	81/09/07 12:33:38	eric	41	40	00000/00000/00003
add auto-rebuild on alias database; fix some aliasing bugs;
add statistics to rebuild; slight code restructuring; change
version handling one last (?) time

D 3.22	81/09/07 10:23:04	eric	40	39	00000/00000/00003
add NOTUNIX compile flag to turn off UNIX "From " line processing

D 3.21	81/09/06 19:50:23	eric	39	38	00000/00000/00003
cleanup, commenting, linting, etc.

D 3.20	81/09/06 14:23:20	eric	38	37	00000/00000/00003
improve "version:" entry in makefile; fix -em (mail back errors) option

D 3.19	81/09/06 10:29:49	eric	37	36	00000/00000/00003
fix $u argument processing bug (happened in uucp); .cf file cleanup

D 3.18	81/08/31 21:22:39	eric	36	35	00000/00000/00003
collect mail statistics; minor configuration changes

D 3.17	81/08/31 12:11:57	eric	35	34	00000/00000/00003
allow "error" as a net name to print error messages

D 3.16	81/08/29 19:16:24	eric	34	33	00000/00000/00003
drop unnecessary H_FORCE bits in conf.c; delete H_DELETE
(since H_ACHECK can be used with a zero mask field)

D 3.15	81/08/27 11:42:58	eric	33	32	00000/00000/00003
on -t, use argument list as a supress list

D 3.14	81/08/25 16:06:37	eric	32	31	00000/00000/00003
change handling of <LWSP> characters; remove special
"at" processing (put it in .cf file)

D 3.13	81/08/24 14:05:39	eric	31	30	00000/00000/00003
Add "-t" option to read To:, Cc:, and Bcc: lines to get recipients

D 3.12	81/08/23 12:08:53	eric	30	29	00000/00000/00003
plug assorted security holes

D 3.11	81/08/22 17:52:06	eric	29	28	00000/00000/00003
Arrange to pull full name out of From line if found; includes
Original-From: hacking (oh so ugly).  There's got to be a better way
to do this...

D 3.10	81/08/22 14:45:27	eric	28	27	00000/00000/00003
fix UNIX From line parsing problem; add $b macro to get
correct dates; fix mail-to-file problem; define basic macros
before cracking arguments so that -D is more useful

D 3.9	81/08/21 18:51:33	eric	27	26	00000/00000/00003
return transcript even if no message was collected

D 3.8	81/08/21 18:23:53	eric	26	25	00000/00000/00003
drop M_FINAL, add $y=ttyname, rename some constants

D 3.7	81/08/20 15:19:50	eric	25	24	00000/00000/00003
internal cleanup & minor improvements

D 3.6	81/08/18 11:40:37	eric	24	23	00000/00000/00003
allow :include: specs

D 3.5	81/08/17 11:02:12	eric	23	22	00000/00000/00003
implement hashing in symbol table and add more headers, from
BB&N Report No. ICST/CBOS - 80/2, "Specification of a Draft Message
Format Standard (Draft Report)", prepared for NBS.

D 3.4	81/08/09 19:08:25	eric	22	21	00000/00000/00003
put mailer definitions into configuration file

D 3.3	81/03/28 11:52:37	eric	21	20	00003/00001/00000
change format of Version to be suitable for internal use

D 3.2	81/03/20 09:45:43	eric	20	19	00001/00001/00000
change name (again); from postbox to sendmail

D 3.1	81/03/07 14:27:18	eric	19	18	00001/00001/00000
----- delivermail ==> postbox -----

D 2.8	81/02/28 11:54:11	eric	18	17	00000/00000/00001
install VAX mpx file logging

D 2.7	81/02/05 08:01:25	eric	17	16	00000/00000/00001
always issued a delivermail error message regardless of M_QUIET

D 2.6	81/01/10 14:28:41	eric	16	15	00000/00000/00001
include ArpaLocal; flush output on errors; allow
"user" "at" "host" as separate parameters; etc.

D 2.5	81/01/08 23:57:31	eric	15	14	00000/00000/00001
fixed botch in at => @ translation

D 2.4	81/01/08 19:37:22	eric	14	13	00000/00000/00001
fixed several bugs in parser; translate <lwsp> into quoted .

D 2.3	80/12/06 17:33:32	eric	13	12	00000/00000/00001
fix ^D botch in maketemp; allow "eric:eric,i.eric"

D 2.2	80/11/20 19:57:57	eric	12	11	00000/00000/00001
make dates be taken as date sent rather than date delivered

D 2.1	80/11/05 11:00:06	eric	11	10	00000/00000/00001
release 2

D 1.10	80/10/28 23:52:38	eric	10	9	00000/00000/00001
fixed ANOTHER bug in aliasing (this is getting dull....)

D 1.9	80/10/27 19:28:30	eric	9	8	00000/00000/00001
fix alias bug; count message sizes; map stderr->stdout; misc.

D 1.8	80/10/21 12:58:43	eric	8	7	00000/00000/00001
install dbm stuff + fix quoting bugs

D 1.7	80/10/18 16:48:51	eric	7	6	00000/00000/00001
cleanup for dbm stuff: Error => Errors; move local host
detection into parse; misc cleanup

D 1.6	80/10/15 10:24:24	eric	6	5	00000/00000/00001
deal with disk overflows

D 1.5	80/10/11 20:10:44	eric	5	4	00000/00000/00001
fixed problem with sizeof MsgId == 0

D 1.4	80/10/11 18:55:35	eric	4	3	00000/00000/00001
cleanup to simplify distribution

D 1.3	80/10/11 13:49:55	eric	3	2	00000/00000/00001
accept multi-line fields in headers

D 1.2	80/10/11 13:37:53	eric	2	1	00000/00000/00001
test

D 1.1	80/10/11 13:34:43	eric	1	0	00001/00000/00000

code versions:

alias.o:
	alias.c	5.16 (Berkeley) 7/14/88 (without DBM)
arpadate.o:
	arpadate.c	5.9 (Berkeley) 6/30/88
clock.o:
	clock.c	5.6 (Berkeley) 6/30/88
collect.o:
	collect.c	5.4 (Berkeley) 6/30/88
conf.o:
	conf.c	5.17 (Berkeley) 6/30/88
convtime.o:
	convtime.c	5.3 (Berkeley) 6/30/88
daemon.o:
	daemon.c	5.27 (Berkeley) 9/20/88 (with daemon mode)
deliver.o:
	deliver.c	5.23 (Berkeley) 9/20/88
domain.o:
	domain.c	5.16 (Berkeley) 9/20/88 (with name server)
envelope.o:
	envelope.c	5.15 (Berkeley) 6/30/88
err.o:
	err.c	5.9 (Berkeley) 6/30/88
headers.o:
	headers.c	5.10 (Berkeley) 6/30/88
macro.o:
	macro.c	5.5 (Berkeley) 6/30/88
main.o:
	 Copyright (c) 1988 Regents of the University of California.
	main.c	5.21 (Berkeley) 9/20/88
	sendmail.h	5.13		9/20/88
parseaddr.o:
	parseaddr.c	5.9 (Berkeley) 6/30/88
queue.o:
	queue.c	5.24 (Berkeley) 6/30/88 (with queueing)
readcf.o:
	readcf.c	5.14 (Berkeley) 9/20/88
recipient.o:
	recipient.c	5.12 (Berkeley) 6/30/88
savemail.o:
	savemail.c	5.9 (Berkeley) 6/30/88
srvrsmtp.o:
	srvrsmtp.c	5.22 (Berkeley) 6/30/88 (with SMTP)
stab.o:
	stab.c	5.5 (Berkeley) 6/30/88
stats.o:
	stats.c	5.10 (Berkeley) 6/30/88
sysexits.o:
	sysexits.c	5.5 (Berkeley) 6/30/88
trace.o:
	trace.c	5.5 (Berkeley) 6/30/88
usersmtp.o:
	usersmtp.c	5.10 (Berkeley) 6/30/88 (with SMTP)
util.o:
	util.c	5.9 (Berkeley) 12/17/86

# endif COMMENT
