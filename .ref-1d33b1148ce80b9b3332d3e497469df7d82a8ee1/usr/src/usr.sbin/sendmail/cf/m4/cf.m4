divert(0)dnl
#
# Copyright (c) 1983, 1995 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#


######################################################################
######################################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
define(`TEMPFILE', maketemp(/tmp/cfXXXXXX))dnl
syscmd(sh ../sh/makeinfo.sh > TEMPFILE)dnl
include(TEMPFILE)dnl
syscmd(rm -f TEMPFILE)dnl
#####
######################################################################
######################################################################

divert(-1)

changecom()
undefine(`format')
undefine(`hpux')
ifdef(`pushdef', `',
	`errprint(`You need a newer version of M4, at least as new as
System V or GNU')
	include(NoSuchFile)')
define(`PUSHDIVERT', `pushdef(`__D__', divnum)divert($1)')
define(`POPDIVERT', `divert(__D__)popdef(`__D__')')
define(`OSTYPE',
	`PUSHDIVERT(-1)
	ifdef(`__OSTYPE__', `errprint(`duplicate OSTYPE')')
	define(`__OSTYPE__', $1)
	define(`_ARG_', $2)
	include(../ostype/$1.m4)POPDIVERT`'')
define(`MAILER',
`ifdef(`_MAILER_$1_', `dnl`'',
`define(`_MAILER_$1_', `')PUSHDIVERT(7)include(../mailer/$1.m4)POPDIVERT`'')')
define(`DOMAIN', `PUSHDIVERT(-1)define(`_ARG_', $2)include(../domain/$1.m4)POPDIVERT`'')
define(`FEATURE', `PUSHDIVERT(-1)define(`_ARG_', $2)include(../feature/$1.m4)POPDIVERT`'')
define(`HACK', `PUSHDIVERT(-1)define(`_ARG_', $2)include(../hack/$1.m4)POPDIVERT`'')
define(`VERSIONID', ``#####  $1  #####'')
define(`LOCAL_RULE_0', `divert(3)')
define(`LOCAL_RULE_1',
`divert(9)dnl
#######################################
###  Ruleset 1 -- Sender Rewriting  ###
#######################################

S1
')
define(`LOCAL_RULE_2',
`divert(9)dnl
##########################################
###  Ruleset 2 -- Recipient Rewriting  ###
##########################################

S2
')
define(`LOCAL_RULE_3', `divert(2)')
define(`LOCAL_CONFIG', `divert(6)')
define(`LOCAL_NET_CONFIG', `define(`_LOCAL_RULES_', 1)divert(1)')
define(`UUCPSMTP', `R DOL(*) < @ $1 .UUCP > DOL(*)	DOL(1) < @ $2 > DOL(2)')
define(`CONCAT', `$1$2$3$4$5$6$7')
define(`DOL', ``$'$1')
define(`SITECONFIG',
`CONCAT(D, $3, $2)
define(`_CLASS_$3_', `')dnl
ifelse($3, U, Cw$2 $2.UUCP, `dnl')
define(`SITE', `ifelse(CONCAT($'2`, $3), SU,
		CONCAT(CY, $'1`),
		CONCAT(C, $3, $'1`))')
sinclude(../siteconfig/$1.m4)')
define(`EXPOSED_USER', `PUSHDIVERT(5)CE$1
POPDIVERT`'dnl')
define(`LOCAL_USER', `PUSHDIVERT(5)CL$1
POPDIVERT`'dnl')
define(`MASQUERADE_AS', `define(`MASQUERADE_NAME', $1)')
define(`_OPTINS', `ifdef(`$1', `$2$1$3')')

m4wrap(`include(`../m4/proto.m4')')

# set up default values for options
define(`confMAILER_NAME', ``MAILER-DAEMON'')
define(`confFROM_LINE', `From $g  $d')
define(`confOPERATORS', `.:%@!^/[]+')
define(`confSMTP_LOGIN_MSG', `$j Sendmail $v/$Z; $b')
define(`confRECEIVED_HEADER', `$?sfrom $s $.$?_($?s$|from $.$_) $.by $j ($v/$Z)$?r with $r$. id $i$?u for $u$.; $b')
define(`confSEVEN_BIT_INPUT', `False')
define(`confEIGHT_BIT_HANDLING', `pass8')
define(`confALIAS_WAIT', `10')
define(`confMIN_FREE_BLOCKS', `100')
define(`confBLANK_SUB', `.')
define(`confCON_EXPENSIVE', `False')
define(`confDELIVERY_MODE', `background')
define(`confTEMP_FILE_MODE', `0600')
define(`confMCI_CACHE_SIZE', `2')
define(`confMCI_CACHE_TIMEOUT', `5m')
define(`confUSE_ERRORS_TO', `False')
define(`confLOG_LEVEL', `9')
define(`confCHECK_ALIASES', `False')
define(`confOLD_STYLE_HEADERS', `True')
define(`confPRIVACY_FLAGS', `authwarnings')
define(`confSAFE_QUEUE', `True')
define(`confMESSAGE_TIMEOUT', `5d/4h')
define(`confTIME_ZONE', `USE_SYSTEM')
define(`confDEF_USER_ID', `1:1')
define(`confCW_FILE', `/etc/sendmail.cw')
define(`confMIME_FORMAT_ERRORS', `True')
define(`confFORWARD_PATH', `$z/.forward.$w:$z/.forward')

divert(0)dnl
VERSIONID(`@(#)cf.m4	8.20 (Berkeley) %G%')
