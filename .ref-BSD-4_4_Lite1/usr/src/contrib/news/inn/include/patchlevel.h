/* $Revision: 1.14 $
**
**  $Log: patchlevel.h,v $
**  Revision 1.14  1993/03/18  21:04:15  rsalz
**  patch04:  Add NEWSUMASK (default 2) and have appropriate programs set it.
**  patch04:  Add comment about NEWSLIB to config.dist <chongo@ncd.com>
**  patch04:  Add YACC config variable.
**  patch04:  Alias FNDELAY to O_NDELAY for systems without it <urlichs@smurf.sub.org>
**  patch04:  Use $(SHELL) not sh in Makefiles <osm@msen.com>
**  patch04:  Use "echo ...|su" not "su -c ..." in rc.news and BUILD.
**  patch04:  Document setsockopt/svr4 problems.
**  patch04:  overview.fmt.5 referenced makeoverview <henrich@crh.cl.msu.edu>.
**  patch04:  Various typos in documentation <ry66@rz;uni-karlsruhe.de>,
**  patch04:  <robert@steffi.demon.co.uk>, <wdh@grouper.mkt.csd.harris.com>.
**  patch04:  Add sample newsfeeds entry to overchan.8
**  patch04:  More text for innwatch.ctl, ``make update'', news overview <rsalz>.
**  patch04:  inews can spool, so you need rnews; update inews.1
**  patch04:  Add rnews explanations to Install.ms.1
**  patch04:  Inews should not spool or email if -D given.
**  patch04:  rnews had bad fopen call <mam@mamunx.garmhausen.de>
**  patch04:  rnews could not connect to remote server.
**  patch04:  rnews leaked memory in ReadRemainder.
**  patch04:  decode unpacked end wrong.
**  patch04:  decode used bad pointer comparisons.
**  patch04:  #if was backwards in syslog/syslogd.c <peter@dialix.oz.au>.
**  patch04:  Remove leading space in filenames in syslog.conf
**  patch04:  Add -O flag to expireover; fix sorting bug <rob@violet.berkeley.edu>.
**  patch04:  *** RUN "expireover -a -s" SOON **
**  patch04:  Malloc overrun in expireover.c article.c <rob@agate.berkeley.edu>
**  patch04:  Have overchan create needed dirs if overview dir != spool dir.
**  patch04:  Add overchan .o dependencies to backends/Makefile.
**  patch04:  avoid unneeded unlink/group in expireover <tale@uunet.uu.net>
**  patch04:  Add -g to expire <tal@warren.mentorg.com>
**  patch04:  faster raceless expireover/overchan locking <rob@agate.berkeley.edu>
**  patch04:  use caseEQ not EQ in CMDmode in nnrpd <urlichs@smurf.sub.org>
**  patch04:  Two wrong CloseOnExec calls in article.c <stevo@elroy.Jpl.Nasa.Gov>
**  patch04:  Have nnrpd's HISgetent return (char *) not (STRING).
**  patch04:  Remove STATIC from nnrpd's CMD_unimp function definition; change
**  patch04:  it to recognize slave command.
**  patch04:  Add "date" command (from nntpv2 draft) to nnrpd.
**  patch04:  Add -u flag and statistics to nntpget.
**  patch04:  Add -p flag to filechan, buffchan.
**  patch04:  buffchan shouldn't open dropped sites.
**  patch04:  Add -A flag to innxmit.
**  patch04:  newsrequeue re-used variable; coredumped if not logfile mode.
**  patch04:  expireover and fastrm need <ctype.h>.
**  patch04:  Move functions in fastrm so STATIC declaration is okay.
**  patch04:  BUILD and makehistory no longer assume history is in NEWSLIB.
**  patch04:  Spelled Jon's name wrong in dbz.pch <davison@borland.com>
**  patch04:  Use memset not bzero in local FD_ZERO macro.
**  patch04:  getlist parsed positional arguments wrong <chongo@ncd.com>
**  patch04:  getlist did not send a QUIT to the server.
**  patch04:  Reverse order of elements in include/uio.h <aej@wpi.WPI.EDU>
**  patch04:  Have GetFQDN try to force NIS/YP to use DNS <wietse@wzv.win.tue.nl>
**  patch04:  Fix date parser when hour is 12.
**  patch04:  Typo in header in send-ihave <mit@huie.hokudai.ac.jp>
**  patch04:  Had senduucp.log in samples/scanlogs <earle@isolar.Tujunga.CA.US>
**  patch04:  Have innwatch not complain to console if innd dies <mcooper@usc.edu>
**  patch04:  Add logwatch into innwatch <Christophe.Wolfhugel@grasp1.univ-lyon1.fr>
**  patch04:  rmgroup, newgroup, checkgroup are better about updating newsgroups
**  patch04:  <cs@germany.eu.net>
**  patch04:  Spurious erroneous mail line in rmgroup.
**  patch04:  checkgroups mail message is now more clear.
**  patch04:  Convert remaining scripts in samples to use innshellvars
**  patch04:  Fix dataloss and fd leak in SITEflush <cjj@sun.com>
**  patch04:  Don't use strlen on mmap'd active file <bobs@monty.rand.org>
**  patch04:  Used ModeReason not RejectReason in CCmode, CCreject<dem@meaddata.com>
**  patch04:  Used wrong argv[] in CCreject <dem@meaddata.com>
**  patch04:  Don't free NULL pointer in innd/rc.c <peter@dialix.oz.au>.
**  patch04:  Set all WIP's properly to NULL <dem@meaddata.com>
**  patch04:  SITEparsefile didn't free old ME entry <dem@meaddata.com>
**  patch04:  innd had typo in NICE_KIDS #if test <enger@seka.reston.ans.net>
**  patch04:  Add (void) to setsid call in innd <stevo@elroy.Jpl.Nasa.Gov>
**  patch04:  SITEwantsgroup didn't check ME patterns
**  patch04:  <watanabe@argon.material.tohoku.ac.jp>
**  patch04:  Don't crash if spooling fails <alden@zaphod.mps.ohio-state.edu>
**  patch04:  Don't reuse socket in innd/cc.c <peter@dialix.oz.au>
**  patch04:  Protect possible NULL return from RChostname (!?).
**  patch04:  New NNTP connections to innd must clear any old WIP.
**  patch04:  Add "p" item to newsfeeds "A" flag; document slave use.
**  patch04:  rmgroups propagate like newgroups.
**  patch04:  ctlinnd 'addhist' must open history if server not running.
**  patch04:  innd clobbered memory on some reloads.
**  patch04:  Copy SetDescriptorLimit into inndstart.
**  patch04:  Make LIST case-insensitive in innd.
**  patch04:  Add hosts.nntp.nolimit
**  patch04:  Check PID file before innd starts up.
**
**  Revision 1.13  1993/01/29  16:51:16  rsalz
**  patch03:  Don't use absolute paths for true false in BUILD and send-nntp
**  patch03:  Add G flag to newsfeeds
**  patch03:  Add XPATH to innd and nnrpd; have nnrpd use it to ask boss.
**  patch03:  Used free'd memory in lib/dfdist.c <rob@agate.berkeley.edu>
**  patch03:  Move all shell config stuff to innshellvars.
**  patch03:  Reject article if all groups are 'x', even if DO_WANT_TRASH
**  patch03:  <jmalcolm@sura.net>
**  patch03:  Allow cross-posting to groups that don't exist if one does exist.
**  patch03:  Add group permissions to hosts.nntp
**  patch03:  Spool to C News-style togo files on the fly <chris@wugate.wustl.edu>
**  patch03:  Sun needs <stdarg.h> before <syslog.h> <wilson@cygnus.com>
**  patch03:  Don't isntall nntpsend.ctl when doing "make most" <jgm+@CMU.EDU>
**  patch03:  Add -gd to uux calls in sendbathc, send-uucp
**  patch03:  Do clearerr after error writing log file <jepeway@cs.utk.edu>.
**  patch03:  nntpget -o would core dump <matthias@smurf.sub.org>
**  patch03:  Add MIME support to nnrpd and innxmit
**  patch03:  <Christophe.Wolfhugel@hsc-sec.fr>
**  patch03:  Remove nn support.
**  patch03:  Have nntpget send "mode reader" <peter@dialix.oz.au>
**  patch03:  Add SCO style to putdoc <peter@dialix.oz.au>
**  patch03:  Add binary search to nnrpd newnews. <rob@violet.berkeley.edu>
**  patch03:  Add RNEWSLOCALCONNECT config param.
**  patch03:  Add -S flag to rnews.
**  patch03:  Remove empty batchfiles <syd@dsinc.dsi.com>
**  patch03:  Replace NNRPD_DBZINCORE with NNRPD_DBZINCORE_DELAY;
**  patch03:  <rob@violet.berkeley.edu>.
**  patch03:  innd closes connections after 'n' bad commands in a row.
**  patch03:  Add WO and ability to write overview data.
**  patch03:  Add expireover, overchan, nnrpd use of overview
**  patch03:  <rob@violet.berkely.edu> <rsalz>
**  patch03:  Fix SITE.FNLnames memory leak <craig@ee.lbl.gov>
**  patch03:  Document Sequent maxfd limitation.
**  patch03:  Fix quoting error in control/default <tale@uunet.uu.net>
**  patch03:  catch AIX SIGDANGER in innd <Christophe.Wolfhugel@grasp.insa-lyon.fr>
**  patch03:  All mod addresse are wildcard matches <craig@ee.lbl.gov>
**  patch03:  Remove duplicate action in clobber <kieber@is2151.inf.tu-dresden.de>
**  patch03:  Add HISincore <taka@fxis.fujixerox.co.jp>
**  patch03:  Add site.cmd to sendbatch <sob@tmc.edu>
**  patch03:  Missing +1 in nnrpd NEWNEWS initial alloc <cjj@sun.com>.
**  patch03:  Have nnrpd not put site in Path if it's making one
**  patch03:  <dem@meaddata.com>
**  patch03:  shrinkfile EOF was wrong; removed -n flag; fix nntpsend call
**  patch03:  <chongo@pyramid.com>
**  patch03:  Fix exit status in getlist.
**  patch03:  Use Maxlength not %.50s in more places in innd
**  patch03:  Add and document expirerm
**  patch03:  Add "Tl" feeds to innd.
**  patch03:  Tighten window on "server died in ctlinnd".
**  patch03:  Replicated slaves wrote truncated Xref headers.
**  patch03:  Fix OS/x varargs and porting nits <chongo@pyramid.com>
**  patch03:  Only put changed files in site in source control
**  patch03:  <chongo@pyramid.com>
**  patch03:  Make grephistory more robust on lookup failures
**  patch03:  <taka@fxis.fujixerox.co.jp>
**  patch03:  Blank symlink buffer in makehistory for A/UX <urlichs@smurf.sub.org>
**  patch03:  Fix so innd -u stays unbuffered after "ctlinnd flushlogs"
**  patch03:  Close a cople of memory leaks in innd <leres@ee.lbl.gov>
**  patch03:  Always set exit status var in batcher <cs@germany.eu.net>
**  patch03:  Fix no-Unix-domain sockets better in rnews.
**  patch03:  Add -S flag to rnews.
**  patch03:  Have rnews rename bad batches when unspooling.
**  patch03:  Missing subst line for AWK in sendbatch <dem@meaddata.com>
**  patch03:  Fix nntpget -o flag <Pekka.Nikander@ajk.tele.fi>
**  patch03:  Add "make sedtest" to config/Makefile and update Install.
**  patch03:  fix backup restore targets in config/Makefile
**  patch03:  Don't log post stats in nnrpd if no posting <rob@agate.berkely.edu>
**  patch03:  Say "localhost" connected not "local" <rob@agate.berkeley.edu>
**  patch03:  Document ACT_STYLE_MMAP better.
**  patch03:  Declare mmap() in include/clibrary.h <leres@ee.lbl.gov>
**  patch03:  Add #if'd #define so only one mmap() call in icd.c
**  patch03:  Don't have innd make spool directories for alias newgroups.
**  patch03:  Use full path to innconfval in makegorup <phil@forum.swarthmore.edu>
**  patch03:  makegroup parsed `date` wrong for Europen DST output
**  patch03:  <wib@jupiter.informatik.uni-kiel.dbp.de>
**  patch03:  Have newgroup not put blank lines in newsgroups file.
**  patch03:  Expire opens "-z" file in append mode.
**  patch03:  Remove extra DTS correction in LOCALtoGMT in nnrpd/misc.c
**  patch03:  Make active file format errors more verbose in expire.
**  patch03:  Add IFTRUE macro to {lib,config}/Makefile and use it.
**  patch03:  Add eval wrapper in if in iftrue.
**  patch03:  Add drop command to buffchan, have innd forward the command
**  patch03:  if a site feeding into an exploder channel is dropped <kre>.
**  patch03:  Add readmap command to buffchan.
**  patch03:  Perl is in /usr/bin/perl for scanspool.
**  patch03:  lib/writev.c has two Revision lines <kre@munnari.oz.au>
**  patch03:  innd would mark wrong master site; using same name 3+ times
**  patch03:  could result in articles going nowhere. <kre@munnari.oz.au>
**  patch03:  Argument gluing in ctlinnd was wrong for send, reload, etc.
**  patch03:  and save malloc of one byte <kre@munnari.oz.au>
**  patch03:  buffchan gave away perm to open its own files <kre@munnari.oz.au>
**  patch03:  Remove relaynews.
**  patch03:  Various manpage fixes <kieber@sax.sax.de>
**  patch03:  Typos in doc/inwatch.ctl.5 <aej@wpi.WPI.EDU> <chongo@pyramid.com>
**  patch03:  Typos in Install.ms.2 and news-recovery.8; wrong name for log
**  patch03:  file in newslog.5 <wdh@grouper.mkt.csd.harris.com>
**  patch03:  Mention "rnews -U" in install.ms.2 <dem@meaddata.com>
**  patch03:  Tweak NAME section of some manpages.
**  patch03:  Note UUNET address in Install.ms
**  patch03:  Portability nit in UX macro in Install.ms.1 <eggert@twinsun.com>
**  patch03:  Better wording for doifarg in control.ctl.5.
**  patch03:  Say distribs aren't patterns in doc/newsfeeds.5
**  patch03:  Add BSDI and Solaris2.X sections to Install.ms.1
**  patch03:  Document importance of _PATH_LOCKS.
**  patch03:  Add more documentation on syslog variants.
**  patch03:  More documentation DBZ and patching it.
**  patch03:  Document need to review some scripts.
**  patch03:  Move part of Install.ms.1 to Install.ms.2
**  patch03:  Document xfopena work-around.
**  patch03:  Document WANT_JUNK and its effects.
**  patch03:  Don't make OLD dir in scanlogs.
**  patch03:  Tell people not to use "/foo,!bar" in newsfeeds.5
**  patch03:  Make the nnrpd THREAD_NAME_FLAT code really flatten the name of
**  patch03:  the thread file <usbill@ustores.ustores.missouri.edu>
**  patch03:  Missing close paren on fseek call in feedone
**  patch03:  <mike@pyrdc.va.pyramid.com>
**  patch03:  Have expire *not* rewrite "arrival time" of expired articles
**  patch03:  to be current time.
**  patch03:  Clean up nntpsend and send-nntp.
**  patch03:  It's R not h for W flag; fix samples/newsfeeds and inncheck.
**  patch03:  Have innlog.awk ignore replicated ihave messages.
**  patch03:  remove samples/innreport from config file list.
**  patch03:  Write to $HOME/dead.article if inews can't spool.
**  patch03:  Add -R flag to inews.
**  patch03:  Document more timezone limitations in parsedate.3
**  patch03:  Have convdate exit with number of parse failures.
**  patch03:  FDCOUNT_ULIMIT code in getdtab.c was uncompilable
**  patch03:  <wdh@grouper.mkt.csd.harris.com>
**  patch03:  FDCOUNT_CONSTANT code in getdtab.c was uncompilable
**  patch03:  Use FDCOUNT_STYLE SYSCONF for HP-UX.
**
**  Revision 1.12  1992/09/14  19:21:15  rsalz
**  patch02:  Fix writev setup error in nntpget <stephan@math.uni-muenster.de>
**  patch02:  Point to install.ms in newsfeeds.5
**  patch02:  Have nnrpd post work if DONT_HAVE_UNIX_DOMAIN.
**  patch02:  Don't have nnrpd die if distrib.pats isn't installed.
**  patch02:  Fix innd so cancelling cross-posted articles doesn't only
**  patch02:  remove the first one.
**  patch02:  Have inncheck warn about newsfeed lines that end with whitespace
**  patch02:  Have nnrpd NEWNEWS command ignore expired articles.
**  patch02:  Make send-nntp log file be in standard place; clean up all
**  patch02:  sample files with respect to subst params.
**  patch02:  Add "-s" to "du" command in sendbatch <bgg@pta.pyramid.com.au>
**  patch02:  Add -v flag to print stats to stdout in batcher and innxmit.
**  patch02:  Accept "(EDT)" in date parser; validate numeric timezones.
**  patch02:  nnrpd date conversion was off by a month <rynes@ins.cwru.edu>
**  patch02:  Fix indenting; document exit status in inews.1
**  patch02:  Add _PATH_SED config param and use it in various sample scripts
**  patch02:  <rainer@flyer.uni-duisburg.de>
**  patch02:  Use PROGNAME in all samples scripts.
**  patch02:  Fix typo's in nes-recovery.8 that made config skip _PATH_SPOOL
**  patch02:  lines <aej@wpi.wpi.edu>
**  patch02:  Remove duplicate DOC= line in MakeInews.
**  patch02:  Make getdtab required; change getdtablesize calls to getfdcount.
**  patch02:  Add FDCOUNT_STYLE config param.  Document in Install.ms.1
**  patch02:  Allow "-" to mean read file from stdin in expire.
**  patch02:  Fix expire to have history database depend on history file, not
**  patch02:  just be in "." <tim@deakin.OZ.AU>
**  patch02:  Fix fencepost error in setting ARTpathme in ARTsetup;
**  patch02:  Don't coredump with grephistory -s when article expired.
**  patch02:  <taka@fxis.fujixerox.co.jp>
**  patch02:  innd wrote default expires of Now for all articles! **SORRY**
**  patch02:  <taka@fxis.fujixerox.co.jp>
**  patch02:  Don't ReopenLog if innd is in Debug mode.
**  patch02:  Using LOCK_LOCKF needs read/write access; fix innxmit to
**  patch02:  open the batchfile with those permissions <neal@ctd.comsat.com>
**  patch02:  nonblock.c needs sys/ioctl.h not ioctl.h if NBIO_IOCTL
**  patch02:  <bin@primate.wisc.edu>.
**  patch02:  Make cvtbatch use only first word in file; warn about bad -w flags.
**  patch02:  Forgot to write doc/cvtbatch.8
**  patch02:  Use nntplink -i stdin, not nntpchan, in newsfeeds.5 examples.
**  patch02:  Install.ms.[12] changes:
**  patch02:    add more on nntp newsreaders; add more "lint noise" caveats;
**  patch02:    warn about HP-UX df; document Sun's Bug# for "too many open
**  patch02:    files"; add notes about sVr4 LIBS variances; note Dell has
**  patch02:    broken Unix-domain sockets for Dell sVr4
**  patch02:  Update IRIX notes in Install.ms.1 <jwpope@uswest.com>
**  patch02:  Change "someone else sending this to us?" timeouts in nc.c
**  patch02:  Remove extra break; get a const char call right in nc.c
**  patch02:  <leres@ee.lbl.gov>
**  patch02:  Address2Name isn't always needed; re-order #if in nnrpd.c
**  patch02:  <leres@ee.lbl.gov>.
**  patch02:  Fix typo in innlog.awk <kre@munnari.oz.au>
**  patch02:  Use " not ' in su to start nnmaster in rc.news <jonl@hal.com>
**  patch02:  Add /full/path keyword to news.daily <jonl@hal.com>.
**  patch02:  Fix UUXFLAGS, and -U flag to sendbatch <jonl@hal.com>
**  patch02:  Remove hpux __hpux and -DSDD and always use -DHPUX for HP
**  patch02:  ports <ken@sdd.hp.com>.
**  patch02:  Fix Cookie calculation in batcher
**  patch02:  <Christophe.Wolfhugel@grasp.insa-lyon.fr>
**  patch02:  Use _PATH_RNEWS not "rnews" in news.daily
**  patch02:  <jgreely@cis.ohio-state.edu>
**  patch02:  Inews says "not posted" if exiting non-zero.
**  patch02:  Add arrival time to ADDHIST message.  Fix ctlinnd,makehistory -u
**  patch02:  to use it; add field to ARTDATA so HISwrite() doesn't assume
**  patch02:  articles arrived at Now.time <urlichs@smurf.sub.org>
**
**  Revision 1.11  1992/09/01  15:38:41  rsalz
**  patch01:  Avoid "eval ... 2>&1" in news.daily; some shells report an
**  patch01:  "io error" <brister@pa.dec.com>
**  patch01:  Fix some (fs)?printf format/argument errors.
**  patch01:  Declare ICCreserve, RCismaster, GetLoadAverage; INN has no
**  patch01:  implicit function declarations anywhere.
**  patch01:  New innwatch and new config parameters, _PATH_WATCHPID
**  patch01:  _PATH_INNWSTATUS <kre@munnari.oz.au>
**  patch01:  Have doc/Makefile install newslog.8 and distrib.pats.5
**  patch01:  <barrett@ee.und.ac.za> and some others <rsalz>.
**  patch01:  Clarify newsfeeds.5 <barrett@ee.und.ac.za>, <rsalz>.
**  patch01:  Install.ms changes:  typo fixes <bin@primate.wisc.edu>;
**  patch01:    explain DIR_STYLE; say that gcc doesn't have SystemV dbz
**  patch01:    problem; document "stdin" entry in nnrp.access; add
**  patch01:    note about Sun's unbundled compiler.
**  patch01:  Mention MERGE_TO_GROUPS in active.5
**  patch01:  Fix kill(1) to kill(2) in comment in config.dist
**  patch01:  <Christophe.Wolfhugel@univ-lyon1.fr>.
**  patch01:  Fix "inwatch" typo in config.dist <urlichs@smurf.sub.org>
**  patch01:  Make subst.c report errors; remove spurious subst line in
**  patch01:  innd.8, fix sendbatch subst line <urlichs@smurf.sub.org>
**  patch01:  Change default to not do setrlimit(NOFILE) in config.dist.
**  patch01:  Fix backup and restore targets in config/Makefile to use tar.
**  patch01:  Fix news.daily to pass -dxxx to expire, not -Dxxx, if the
**  patch01:  "expdir" keyword was given <brendan@cygnus.com>.
**  patch01:  Don't pass null to sprintf if expire not given -r flag.
**  patch01:  nnrpd said "bad command" not "syntax error" if given wrong
**  patch01:  number of arguments <schmitz@scd.hp.com>.
**  patch01:  Fix == to = so "article <no-such-art>" doesn't make nnrpd
**  patch01:  crash <clarsen@ux6.lbl.gov>.
**  patch01:  Fix VAR_STYLE NONE in nnrpd (don't declare Reply in nnrpd.h).
**  patch01:  Wrap all "#include <sys/un.h>" inside "#if
**  patch01:  defined(DO_HAVE_UNIX_DOMAIN)" <bof@midget.saar.de>
**  patch01:  Had wrong struct iov element in writev.c <bof@midget.saar.de>
**  patch01:  Use chmod, not fchmod in buffchan.c <bof@midget.saar.de>
**  patch01:  Use fwrite() != i, not fwrite() < 0 in shrinkfile.
**  patch01:  Have innd syslog the Message-ID that was cancelled, not the
**  patch01:  the article doing the cancel <barrett@ee.und.ac.za>.
**  patch01:  The "wants to cancel" message was backwards
**  patch01:  <space@ncc1701.stgt.sub.org>
**  patch01:  Fix largewrite in chan.c and some places where %m should
**  patch01:  or should not be used in syslog calls <kre@munnari.oz.au>
**  patch01:  Use caseEQ in ListHas; host names in Path are not case-
**  patch01:  sensitive <jonl@hal.com>
**  patch01:  Ignore bad Expires headers in innd (sigh).
**  patch01:  Remove magic numbers from MaxLength in innd (use sizeof) and
**  patch01:  Make MaxLength truncate at 80, not 50.
**  patch01:  Don't let a Tm site point to a site that is a Tm site.
**  patch01:  Add Join() to innd and use it so article-reject messages
**  patch01:  don't span multiple lines.
**  patch01:  Remove extra comment in art.c
**  patch01:  Put filename in "cant dbminit" error message in innd/his.c
**  patch01:  Have ICDreadactive check result of mmap call.
**  patch01:  Or in MAP_FILE into mmap call if it is #define'd (used by BSDI).
**  patch01:  Have ICDreadactive pass end of file back to caller.
**  patch01:  Remove extra spaces between fields in logfile; make
**  patch01:  newsrequeue -l work with DO_NNTPLINK_LOG and without trailing
**  patch01:  spaces <barrett@ee.und.ac.za>
**  patch01:  Make the "set" lines in parsecontrol more robust; control
**  patch01:  messages without args (e.g., sendsys) were getting dropped.
**  patch01:  Fix Newsgroups spelling in samples/ihave; inews and
**  patch01:  nnrpd rejected ihave and sendme <taka@fxis.fujixerox.co.jp>
**  patch01:  Missing curly brace in scanlogs <barrett@ee.und.ac.za>
**  patch01:  Missing && in nntpsend (-d was broke) <katz@rpal.rockwell.com>
**  patch01:  Add more rnews and nntplink and mthreads (sigh) messages to
**  patch01:  innlog.awk <barrett@ee.und.ac.za>.
**  patch01:  Have c7unbatch pass flags to rnews <L.McLoughlin@doc.ic.ac.uk>
**
**  Revision 1.10  1992/08/19  16:36:04  rsalz
**  Official release.
**
*/


/*
**  If you make local modifications to INN, change this line as needed:
*/
#define LOCAL_STRING	""


/*
**  Try to avoid changing these.
*/
#define RELEASE		"1"
#define PATCHLEVEL	"4"
#define DATE		"20-Mar-93"


/*
**  See lib/innvers.c, but the version string should look like this:
**	INN ${RELEASE}.${PATCHLEVEL} ${DATE} (${LOCAL_STRING})
**  for example:
**	INN 1.0 10-Feb-92 (FOO.COM)
*/
