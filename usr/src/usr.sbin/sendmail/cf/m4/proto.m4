divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)

VERSIONID(`@(#)proto.m4	8.67 (Berkeley) %G%')

MAILER(local)dnl

# level 6 config file format
V6/Berkeley
divert(-1)

# pick our default mailers
ifdef(`confSMTP_MAILER',, `define(`confSMTP_MAILER', `smtp')')
ifdef(`confLOCAL_MAILER',, `define(`confLOCAL_MAILER', `local')')
ifdef(`confRELAY_MAILER',,
	`define(`confRELAY_MAILER',
		`ifdef(`_MAILER_smtp_', `relay',
			`ifdef(`_MAILER_uucp', `suucp', `unknown')')')')
define(`_SMTP_', `confSMTP_MAILER')dnl		for readability only
define(`_LOCAL_', `confLOCAL_MAILER')dnl	for readability only
define(`_RELAY_', `confRELAY_MAILER')dnl	for readability only

# back compatibility with old config files
ifdef(`confDEF_GROUP_ID',
	`errprint(`*** confDEF_GROUP_ID is obsolete.')
	 errprint(`    Use confDEF_USER_ID with a colon in the value instead.')')
ifdef(`confREAD_TIMEOUT',
	`errprint(`*** confREAD_TIMEOUT is obsolete.')
	 errprint(`    Use individual confTO_<timeout> parameters instead.')')
ifdef(`confMESSAGE_TIMEOUT',
	`define(`_ARG_', index(confMESSAGE_TIMEOUT, /))
	 ifelse(_ARG_, -1,
		`define(`confTO_QUEUERETURN', confMESSAGE_TIMEOUT)',
		`define(`confTO_QUEUERETURN',
			substr(confMESSAGE_TIMEOUT, 0, _ARG_))
		 define(`confTO_QUEUEWARN',
			substr(confMESSAGE_TIMEOUT, eval(_ARG_+1)))')')
ifdef(`confMIN_FREE_BLOCKS', `ifelse(index(confMIN_FREE_BLOCKS, /), -1,,
	`errprint(`*** compound confMIN_FREE_BLOCKS is obsolete.')
	 errprint(`    Use confMAX_MESSAGE_SIZE for the second part of the value.')')')

# clean option definitions below....
define(`_OPTION', `ifdef(`$2', `O $1=$2', `#O $1`'ifelse($3, `',, `=$3')')')dnl

divert(0)dnl

##################
#   local info   #
##################

Cwlocalhost
ifdef(`USE_CW_FILE',
`# file containing names of hosts for which we receive email
Fw`'confCW_FILE',
	`dnl')

# my official domain name
# ... define this only if sendmail cannot automatically determine your domain
ifdef(`confDOMAIN_NAME', `Dj`'confDOMAIN_NAME', `#Dj$w.Foo.COM')

ifdef(`_NULL_CLIENT_ONLY_', `divert(-1)')dnl

CP.

ifdef(`UUCP_RELAY',
`# UUCP relay host
DY`'UUCP_RELAY
CPUUCP

')dnl
ifdef(`BITNET_RELAY',
`#  BITNET relay host
DB`'BITNET_RELAY
CPBITNET

')dnl
ifdef(`FAX_RELAY',
`# FAX relay host
DF`'FAX_RELAY
CPFAX

')dnl
# "Smart" relay host (may be null)
DS`'ifdef(`SMART_HOST', SMART_HOST)

ifdef(`LUSER_RELAY',
`# place to which unknown users should be forwarded
Kuser user -m -a<>
DL`'LUSER_RELAY
', `dnl')

# operators that cannot be in local usernames (i.e., network indicators)
CO @ % ifdef(`_NO_UUCP_', `', `!')

# a class with just dot (for identifying canonical names)
C..

ifdef(`MAILER_TABLE',
`# Mailer table (overriding domains)
Kmailertable MAILER_TABLE

')dnl
ifdef(`DOMAIN_TABLE',
`# Domain table (adding domains)
Kdomaintable DOMAIN_TABLE

')dnl
# who I send unqualified names to (null means deliver locally)
DR`'ifdef(`LOCAL_RELAY', LOCAL_RELAY)

# who gets all local email traffic ($R has precedence for unqualified names)
DH`'ifdef(`MAIL_HUB', MAIL_HUB)

# class L: names that should be delivered locally, even if we have a relay
# class E: names that should be exposed as from this host, even if we masquerade
# class D: dotted names, e.g., root.machinename
#CL root
CE root
undivert(5)dnl
ifdef(`__DOTTED_USER_LIST__',
	`__DOTTED_USER_LIST__',
	`#CD postmaster')

# dequoting map
Kdequote dequote

divert(0)dnl
# who I masquerade as (null for no masquerading)
DM`'ifdef(`MASQUERADE_NAME', MASQUERADE_NAME)

undivert(6)dnl

######################
#   Special macros   #
######################

# SMTP initial login message
De`'confSMTP_LOGIN_MSG

# UNIX initial From header format
Dl`'confFROM_LINE

# my name for error messages
Dn`'confMAILER_NAME

# delimiter (operator) characters
Do`'confOPERATORS

# format of a total name
Dq`'ifdef(`confFROM_HEADER', confFROM_HEADER, `$?x$x <$g>$|$g$.')
include(`../m4/version.m4')

###############
#   Options   #
###############

# strip message body to 7 bits on input?
_OPTION(SevenBitInput, `confSEVEN_BIT_INPUT')

# 8-bit data handling
_OPTION(EightBitMode, `confEIGHT_BIT_HANDLING', adaptive)

# wait for alias file rebuild (default units: minutes)
_OPTION(AliasWait, `confALIAS_WAIT', 5m)

# location of alias file
O AliasFile=ifdef(`ALIAS_FILE', `ALIAS_FILE', /etc/aliases)

# minimum number of free blocks on filesystem
_OPTION(MinFreeBlocks, `confMIN_FREE_BLOCKS', 100)

# maximum message size
_OPTION(MaxMessageSize, `confMAX_MESSAGE_SIZE', 1000000)

# substitution for space (blank) characters
_OPTION(BlankSub, `confBLANK_SUB', _)

# avoid connecting to "expensive" mailers on initial submission?
_OPTION(HoldExpensive, `confCON_EXPENSIVE')

# checkpoint queue runs after every N successful deliveries
_OPTION(CheckpointInterval, `confCHECKPOINT_INTERVAL', 10)

# default delivery mode
_OPTION(DeliveryMode, `confDELIVERY_MODE', background)

# automatically rebuild the alias database?
_OPTION(AutoRebuildAliases, `confAUTO_REBUILD')

# error message header/file
_OPTION(ErrorHeader, `confERROR_MESSAGE', /etc/sendmail.oE)

# error mode
_OPTION(ErrorMode, `confERROR_MODE', print)

# save Unix-style "From_" lines at top of header?
_OPTION(SaveFromLine, `confSAVE_FROM_LINES')

# temporary file mode
_OPTION(TempFileMode, `confTEMP_FILE_MODE', 0600)

# match recipients against GECOS field?
_OPTION(MatchGECOS, `confMATCH_GECOS')

# maximum hop count
_OPTION(MaxHopCount, `confMAX_HOP', 17)

# location of help file
O HelpFile=ifdef(`HELP_FILE', HELP_FILE, /usr/lib/sendmail.hf)

# ignore dots as terminators in incoming messages?
_OPTION(IgnoreDots, `confIGNORE_DOTS')

# name resolver options
_OPTION(ResolverOptions, `confBIND_OPTS', +AAONLY)

# deliver MIME-encapsulated error messages?
_OPTION(SendMimeErrors, `confMIME_FORMAT_ERRORS')

# Forward file search path
_OPTION(ForwardPath, `confFORWARD_PATH', /var/forward/$u:$z/.forward.$w:$z/.forward)

# open connection cache size
_OPTION(ConnectionCacheSize, `confMCI_CACHE_SIZE', 2)

# open connection cache timeout
_OPTION(ConnectionCacheTimeout, `confMCI_CACHE_TIMEOUT', 5m)

# use Errors-To: header?
_OPTION(UseErrorsTo, `confUSE_ERRORS_TO')

# log level
_OPTION(LogLevel, `confLOG_LEVEL', 10)

# send to me too, even in an alias expansion?
_OPTION(MeToo, `confME_TOO')

# verify RHS in newaliases?
_OPTION(CheckAliases, `confCHECK_ALIASES')

# default messages to old style headers if no special punctuation?
_OPTION(OldStyleHeaders, `confOLD_STYLE_HEADERS')

# SMTP daemon options
_OPTION(DaemonPortOptions, `confDAEMON_OPTIONS', Port=esmtp)

# privacy flags
_OPTION(PrivacyOptions, `confPRIVACY_FLAGS', authwarnings)

# who (if anyone) should get extra copies of error messages
_OPTION(PostMasterCopy, `confCOPY_ERRORS_TO', Postmaster)

# slope of queue-only function
_OPTION(QueueFactor, `confQUEUE_FACTOR', 600000)

# queue directory
O QueueDirectory=ifdef(`QUEUE_DIR', QUEUE_DIR, /var/spool/mqueue)

# timeouts (many of these)
_OPTION(Timeout.initial, `confTO_INITIAL', 5m)
_OPTION(Timeout.helo, `confTO_HELO', 5m)
_OPTION(Timeout.mail, `confTO_MAIL', 10m)
_OPTION(Timeout.rcpt, `confTO_RCPT', 1h)
_OPTION(Timeout.datainit, `confTO_DATAINIT', 5m)
_OPTION(Timeout.datablock, `confTO_DATABLOCK', 1h)
_OPTION(Timeout.datafinal, `confTO_DATAFINAL', 1h)
_OPTION(Timeout.rset, `confTO_RSET', 5m)
_OPTION(Timeout.quit, `confTO_QUIT', 2m)
_OPTION(Timeout.misc, `confTO_MISC', 2m)
_OPTION(Timeout.command, `confTO_COMMAND', 1h)
_OPTION(Timeout.ident, `confTO_IDENT', 30s)
_OPTION(Timeout.fileopen, `confTO_FILEOPEN', 60s)
_OPTION(Timeout.queuereturn, `confTO_QUEUERETURN', 5d)
_OPTION(Timeout.queuereturn.normal, `confTO_QUEUERETURN_NORMAL', 5d)
_OPTION(Timeout.queuereturn.urgent, `confTO_QUEUERETURN_URGENT', 2d)
_OPTION(Timeout.queuereturn.non-urgent, `confTO_QUEUERETURN_NONURGENT', 7d)
_OPTION(Timeout.queuewarn, `confTO_QUEUEWARN', 4h)
_OPTION(Timeout.queuewarn.normal, `confTO_QUEUEWARN_NORMAL', 4h)
_OPTION(Timeout.queuewarn.urgent, `confTO_QUEUEWARN_URGENT', 1h)
_OPTION(Timeout.queuewarn.non-urgent, `confTO_QUEUEWARN_NONURGENT', 12h)

# should we not prune routes in route-addr syntax addresses?
_OPTION(DontPruneRoutes, `confDONT_PRUNE_ROUTES')

# queue up everything before forking?
_OPTION(SuperSafe, `confSAFE_QUEUE')

# status file
_OPTION(StatusFile, `STATUS_FILE', /etc/sendmail.st)

# time zone handling:
#  if undefined, use system default
#  if defined but null, use TZ envariable passed in
#  if defined and non-null, use that info
ifelse(confTIME_ZONE, `USE_SYSTEM', `#O TimeZoneSpec=',
	confTIME_ZONE, `USE_TZ', `O TimeZoneSpec=',
	`O TimeZoneSpec=confTIME_ZONE')

# default UID (can be username or userid:groupid)
_OPTION(DefaultUser, `confDEF_USER_ID', nobody)

# list of locations of user database file (null means no lookup)
_OPTION(UserDatabaseSpec, `confUSERDB_SPEC', /etc/userdb)

# fallback MX host
_OPTION(FallbackMXhost, `confFALLBACK_MX', fall.back.host.net)

# if we are the best MX host for a site, try it directly instead of config err
_OPTION(TryNullMXList, `confTRY_NULL_MX_LIST')

# load average at which we just queue messages
_OPTION(QueueLA, `confQUEUE_LA', 8)

# load average at which we refuse connections
_OPTION(RefuseLA, `confREFUSE_LA', 12)

# work recipient factor
_OPTION(RecipientFactor, `confWORK_RECIPIENT_FACTOR', 30000)

# deliver each queued job in a separate process?
_OPTION(ForkEachJob, `confSEPARATE_PROC')

# work class factor
_OPTION(ClassFactor, `confWORK_CLASS_FACTOR', 1800)

# work time factor
_OPTION(RetryFactor, `confWORK_TIME_FACTOR', 90000)

# shall we sort the queue by hostname first?
_OPTION(QueueSortOrder, `confQUEUE_SORT_ORDER', priority)

# minimum time in queue before retry
_OPTION(MinQueueAge, `confMIN_QUEUE_AGE', 30m)

# default character set
_OPTION(DefaultCharSet, `confDEF_CHAR_SET', iso-8859-1)

# service switch file (ignored on Solaris, Ultrix, OSF/1, others)
_OPTION(ServiceSwitchFile, `confSERVICE_SWITCH_FILE', /etc/service.switch)

# dialup line delay on connection failure
_OPTION(DialDelay, `confDIAL_DELAY', 10s)

# action to take if there are no recipients in the message
_OPTION(NoRecipientAction, `confNO_RCPT_ACTION', add-to-undisclosed)

# chrooted environment for writing to files
_OPTION(SafeFileEnvironment, `confSAFE_FILE_ENV', /arch)

# are colons OK in addresses?
_OPTION(ColonOkInAddr, `confCOLON_OK_IN_ADDR')

###########################
#   Message precedences   #
###########################

Pfirst-class=0
Pspecial-delivery=100
Plist=-30
Pbulk=-60
Pjunk=-100

#####################
#   Trusted users   #
#####################

# this is equivalent to setting class "t"
#Ft/etc/sendmail.trusted
Troot
Tdaemon
Tuucp

#########################
#   Format of headers   #
#########################

H?P?Return-Path: $g
HReceived: confRECEIVED_HEADER
H?D?Resent-Date: $a
H?D?Date: $a
H?F?Resent-From: $q
H?F?From: $q
H?x?Full-Name: $x
HSubject:
# HPosted-Date: $a
# H?l?Received-Date: $b
H?M?Resent-Message-Id: <$t.$i@$j>
H?M?Message-Id: <$t.$i@$j>
ifdef(`_NULL_CLIENT_ONLY_',
	`include(../m4/nullrelay.m4)m4exit',
	`dnl')
#
######################################################################
######################################################################
#####
#####			REWRITING RULES
#####
######################################################################
######################################################################

undivert(9)dnl

###########################################
###  Rulset 3 -- Name Canonicalization  ###
###########################################
S3

# handle null input (translate to <@> special case)
R$@			$@ <@>

# strip group: syntax (not inside angle brackets!) and trailing semicolon
R$*			$: $1 <@>			mark addresses
R$* < $* > $* <@>	$: $1 < $2 > $3			unmark <addr>
R$* :: $* <@>		$: $1 :: $2			unmark host::addr
R$* : $* <@>		$: $2				strip colon if marked
R$* <@>			$: $1				unmark
R$* ;			$: $1				strip trailing semi

# null input now results from list:; syntax
R$@			$@ :; <@>

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*>$*<$*>$*		$2$3<$4>$5			strip multiple <> <>
R$*<$*<$+>$*>$*		<$3>$5				2-level <> nesting
R$*<>$*			$@ <@>				MAIL FROM:<> case
R$*<$+>$*		$2				basic RFC821/822 parsing

# make sure <@a,@b,@c:user@d> syntax is easy to parse -- undone later
R@ $+ , $+		@ $1 : $2			change all "," to ":"

# localize and dispose of route-based addresses
R@ $+ : $+		$@ $>96 < @$1 > : $2		handle <route-addr>

# find focus for list syntax
R $+ : $* ; @ $+	$@ $>96 $1 : $2 ; < @ $3 >	list syntax
R $+ : $* ;		$@ $1 : $2;			list syntax

# find focus for @ syntax addresses
R$+ @ $+		$: $1 < @ $2 >			focus on domain
R$+ < $+ @ $+ >		$1 $2 < @ $3 >			move gaze right
R$+ < @ $+ >		$@ $>96 $1 < @ $2 >		already canonical

# do some sanity checking
R$* < @ $* : $* > $*	$1 < @ $2 $3 > $4		nix colons in addrs

ifdef(`_NO_UUCP_', `dnl',
`# convert old-style addresses to a domain-based address
R$- ! $+		$@ $>96 $2 < @ $1 .UUCP >	resolve uucp names
R$+ . $- ! $+		$@ $>96 $3 < @ $1 . $2 >		domain uucps
R$+ ! $+		$@ $>96 $2 < @ $1 .UUCP >	uucp subdomains')

# if we have % signs, take the rightmost one
R$* % $*		$1 @ $2				First make them all @s.
R$* @ $* @ $*		$1 % $2 @ $3			Undo all but the last.
R$* @ $*		$@ $>96 $1 < @ $2 >		Insert < > and finish

# else we must be a local name


################################################
###  Ruleset 96 -- bottom half of ruleset 3  ###
################################################

#  At this point, everything should be in a "local_part<@domain>extra" format.
S96

# handle special cases for local names
R$* < @ localhost > $*		$: $1 < @ $j . > $2		no domain at all
R$* < @ localhost . $m > $*	$: $1 < @ $j . > $2		local domain
ifdef(`_NO_UUCP_', `dnl',
`R$* < @ localhost . UUCP > $*	$: $1 < @ $j . > $2		.UUCP domain')
R$* < @ [ $+ ] > $*		$: $1 < @@ [ $2 ] > $3		mark [a.b.c.d]
R$* < @@ $=w > $*		$: $1 < @ $j . > $3		self-literal
R$* < @@ $+ > $*		$@ $1 < @ $2 > $3		canon IP addr
ifdef(`DOMAIN_TABLE', `
# look up domains in the domain table
R$* < @ $+ > $*			$: $1 < @ $(domaintable $2 $) > $3',
`dnl')
undivert(2)dnl

ifdef(`_NO_UUCP_', `dnl',
`ifdef(`UUCP_RELAY',
`# pass UUCP addresses straight through
R$* < @ $+ . UUCP > $*		$@ $1 < @ $2 . UUCP . > $3',
`# if really UUCP, handle it immediately
ifdef(`_CLASS_U_',
`R$* < @ $=U . UUCP > $*	$@ $1 < @ $2 . UUCP . > $3', `dnl')
ifdef(`_CLASS_V_',
`R$* < @ $=V . UUCP > $*	$@ $1 < @ $2 . UUCP . > $3', `dnl')
ifdef(`_CLASS_W_',
`R$* < @ $=W . UUCP > $*	$@ $1 < @ $2 . UUCP . > $3', `dnl')
ifdef(`_CLASS_X_',
`R$* < @ $=X . UUCP > $*	$@ $1 < @ $2 . UUCP . > $3', `dnl')
ifdef(`_CLASS_Y_',
`R$* < @ $=Y . UUCP > $*	$@ $1 < @ $2 . UUCP . > $3', `dnl')

# try UUCP traffic as a local address
R$* < @ $+ . UUCP > $*		$: $1 < @ $[ $2 $] . UUCP . > $3
R$* < @ $+ . . UUCP . > $*		$@ $1 < @ $2 . > $3')
')
ifdef(`_NO_CANONIFY_', `dnl',
`# pass to name server to make hostname canonical
R$* < @ $* $~P > $*		$: $1 < @ $[ $2 $3 $] > $4')

# local host aliases and pseudo-domains are always canonical
R$* < @ $=w > $*		$: $1 < @ $2 . > $3
R$* < @ $* $=P > $*		$: $1 < @ $2 $3 . > $4
R$* < @ $* . . > $*		$1 < @ $2 . > $3

# if this is the local hostname, make sure we treat is as canonical
R$* < @ $j > $*			$: $1 < @ $j . > $2


##################################################
###  Ruleset 4 -- Final Output Post-rewriting  ###
##################################################
S4

R$*<@>			$@ $1				handle <> and list:;

# strip trailing dot off possibly canonical name
R$* < @ $+ . > $*	$1 < @ $2 > $3

# externalize local domain info
R$* < $+ > $*		$1 $2 $3			defocus
R@ $+ : @ $+ : $+	@ $1 , @ $2 : $3		<route-addr> canonical
R@ $*			$@ @ $1				... and exit

ifdef(`_NO_UUCP_', `dnl',
`# UUCP must always be presented in old form
R$+ @ $- . UUCP		$2!$1				u@h.UUCP => h!u')

# delete duplicate local names
R$+ % $=w @ $=w		$1 @ $j				u%host@host => u@host



##############################################################
###   Ruleset 97 -- recanonicalize and call ruleset zero   ###
###		   (used for recursive calls)		   ###
##############################################################

S`'97
R$*			$: $>3 $1
R$*			$@ $>0 $1


######################################
###   Ruleset 0 -- Parse Address   ###
######################################

S0

R<@>			$#_LOCAL_ $: <@>		special case error msgs
R$* : $* ;		$#error $@ USAGE $: "list:; syntax illegal for recipient addresses"
R<@ $+>			$#error $@ USAGE $: "user address required"
R$* <$* : $* > $*	$#error $@ USAGE $: "colon illegal in host name part"
R$* < @ . > $*		$#error $@ USAGE $: "invalid host name"

ifdef(`_MAILER_smtp_',
`# handle numeric address spec
R$* < @ [ $+ ] > $*	$: $>98 $1 < @ [ $2 ] > $3	numeric internet spec
R$* < @ [ $+ ] > $*	$#_SMTP_ $@ [$2] $: $1 < @ [$2] > $3	still numeric: send',
	`dnl')

# now delete the local info -- note $=O to find characters that cause forwarding
R$* < @ > $*		$@ $>97 $1		user@ => user
R< @ $=w . > : $*	$@ $>97 $2		@here:... -> ...
R$* $=O $* < @ $=w . >	$@ $>97 $1 $2 $3		...@here -> ...

# handle local hacks
R$*			$: $>98 $1

# short circuit local delivery so forwarded email works
R$* < @ $=w . >		$: < $R @ $H > $1 < @ $2 . >	if both relay & hub ...
R<$+ @ $+ > $* < $+ >	$: $>_SET_95_ < $H > $3 < $4 >	... send direct to hub
R<$* @ $* > $* < $+ >	$: $3 < $4 >
ifdef(`_STICKY_LOCAL_DOMAIN_',
`R$+ < @ $=w . >		$: < $H > $1 < @ $2 . >		first try hub
R< $+ > $+ < $+ >	$>95 < $1 > $2 < $3 >		yep ....
R< > $=D . $+ < $+ >	$#_LOCAL_ $: $1 . $2		dotted name?
R< > $+ + $* < $+ >	$#_LOCAL_ $: $1 + $2		plussed name?
R< > $+ < $+ >		$#_LOCAL_ $: @ $1			nope, local address',
`R$+ < @ $=w . >		$#_LOCAL_ $: $1			dispose directly',
`R$+ < @ $=w . >		$: $>_SET_95_ < $H > $1 < @ $2 . >	sticky local names
R$+ < @ $=w . >		$#_LOCAL_ $: @ $1		sticky local names')
undivert(4)dnl

ifdef(`_NO_UUCP_', `dnl',
`# resolve remotely connected UUCP links (if any)
ifdef(`_CLASS_V_',
`R$* < @ $=V . UUCP > $*		$#smtp $@ $V $: @ $V : $1 @ $2.UUCP $3',
	`dnl')
ifdef(`_CLASS_W_',
`R$* < @ $=W . UUCP > $*		$#smtp $@ $W $: @ $W : $1 @ $2.UUCP $3',
	`dnl')
ifdef(`_CLASS_X_',
`R$* < @ $=X . UUCP > $*		$#smtp $@ $X $: @ $X : $1 @ $2.UUCP $3',
	`dnl')')

# resolve fake top level domains by forwarding to other hosts
ifdef(`BITNET_RELAY',
`R$*<@$+.BITNET>$*	$#smtp $@ $B $: $1 @ $2 . BITNET $3	user@host.BITNET',
`R$*<@$+.CSNET>$*	$#smtp $@ $C $: $1 @ $2 . CSNET $3	user@host.CSNET',
	`dnl')
ifdef(`_MAILER_pop_',
`R$+ < @ POP. >		$#pop $: $1			user@POP',
	`dnl')
ifdef(`_MAILER_fax_',
`R$+ < @ $+ .FAX. >	$#fax $@ $2 $: $1		user@host.FAX',
`ifdef(`FAX_RELAY',
`R$*<@$+.FAX>$*		$#smtp $@ $F $: $1 @ $2 . FAX $3		user@host.FAX',
	`dnl')')

ifdef(`UUCP_RELAY',
`# forward non-local UUCP traffic to our UUCP relay
R$*<@$*.UUCP>$*		$#smtp $@ $Y $: @ $Y : $1 @ $2.UUCP $3	uucp mail',
`ifdef(`_MAILER_uucp_',
`# forward other UUCP traffic straight to UUCP
R$* < @ $+ .UUCP. > $*		$#uucp $@ $2 $: $1 < @ $2 .UUCP. > $3	user@host.UUCP',
	`dnl')')
ifdef(`_MAILER_usenet_', `
# addresses sent to net.group.USENET will get forwarded to a newsgroup
R$+ . USENET		$#usenet $: $1',
	`dnl')

ifdef(`_LOCAL_RULES_',
`# figure out what should stay in our local mail system
R$* < @ $* > $*		$#smtp $@ $2 $: $1 @ $2 $3		user@host.domain')')
# pass names that still have a host to a smarthost (if defined)
R$* < @ $* > $*		$: $>95 < $S > $1 < @ $2 > $3	glue on smarthost name

# deal with other remote names
ifdef(`_MAILER_smtp_',
`R$* < @$* > $*		$#_SMTP_ $@ $2 $: $1 < @ $2 > $3		user@host.domain',
`R$* < @$* > $*		$#error $@NOHOST $: Unrecognized host name $2')

# if this is quoted, strip the quotes and try again
R$+			$: $(dequote $1 $)		strip quotes
R$+ $=O $+		$@ $>97 $1 $2 $3			try again

# handle locally delivered names
R$=L			$: $>_SET_95_ < $H > $1		special local names
R$=L			$#_LOCAL_ $: @ $1			special local names
R$+			$#_LOCAL_ $: $1			regular local names

###########################################################################
###   Ruleset 5 -- special rewriting after aliases have been expanded   ###
###########################################################################

S5

# if we have a "special dotted user", convert it back to the base name
R$=D . *		$#_LOCAL_ $: $1
R$=D . $+		$#_LOCAL_ $: $1 . *

# prepend an empty "forward host" on the front
R$+			$: <> $1

ifdef(`LUSER_RELAY',
`# send unrecognized local users to a relay host
R< > $+ + $*		$: < $L . > $( user $1 $) + $2
R< > $+			$: < $L . > $( user $1 $)	look up user
R< $* > $+ <> $*	$: < > $2 $3			found; strip $L
R< $* . > $+		$: < $1 > $2			strip extra dot')

# handle plussed local names
R< > $+ + $*		$#_LOCAL_ $@ $2 $: $1

# see if we have a relay or a hub
R$+			$: $>_SET_95_ < $R > $1		try relay
R$+			$: $>_SET_95_ < $H > $1		try hub')
ifdef(`MAILER_TABLE',
`

###################################################################
###  Ruleset 90 -- try domain part of mailertable entry 	###
###################################################################

S90
R<$*> <$- $+ > $*	$: < $1 . $2 > < $3 > $4
R<. $+ > $+		$: < $1 > $2
R< $+ > < > $+		$: < $1 > < . > $2
R<$*> < $+ > $*		$: <$1> < $(mailertable $2 $@ $1 $) > $3	lookup
R<$+> <$- : $+ > $*	$# $2 $@ $3 $: $4		check -- resolved?
R$*			$: $>87 $1
R<$+> < . $+ > $*<$*>	$@ $>90 <$1> <$2> $3<$4>	no -- strip & try again
R$*			$: $>88 $1
R<$+> <$*> $*		$@ $3				no match',
`dnl')

###################################################################
###  Ruleset 95 -- canonify mailer:host syntax to triple	###
###################################################################

S95
R< > $*			$@ $1				strip off null relay
R< $- : $+ > $*		$# $1 $@ $2 $: $3		try qualified mailer
R< $=w > $*		$@ $2				delete local host
R< $+ > $*		$#_RELAY_ $@ $1 $: $2		use unqualified mailer

###################################################################
###  Ruleset 98 -- local part of ruleset zero (can be null)	###
###################################################################

S98
undivert(3)dnl
#
######################################################################
######################################################################
#####
`#####			MAILER DEFINITIONS'
#####
######################################################################
######################################################################
undivert(7)dnl
