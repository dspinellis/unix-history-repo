divert(10)dnl
#
# cad UUCP/TCP configuration file
#
# @(#)cad.mc	1.8 (Berkeley) 3/31/88
#
define(EXTERNAL_VERSION, ``#'	`@(#)cad.mc	1.8' (Berkeley) `3/31/88'')
define(INTERNET_ALIASES, Cwcad ucbcad)
define(UUCP_NAME, DUucbcad)
define(UUCP_ALIASES, CUucbcad)
define(UUCP_HOSTS_FILE, ../machdep/uucp.cad.m4)
define(SMTPUUCP, ../machdep/smtpuucp.cad.m4)
divert(0)dnl
include(ucbproto.mc)dnl
