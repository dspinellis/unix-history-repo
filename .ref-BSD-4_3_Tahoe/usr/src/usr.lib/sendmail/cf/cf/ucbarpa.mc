divert(10)dnl
#
# ucbarpa UUCP/TCP configuration file
#
# @(#)ucbarpa.mc	1.2 (Berkeley) 3/31/88
#
define(EXTERNAL_VERSION, ``#'	`@(#)ucbarpa.mc	1.2' (Berkeley) `3/31/88'')
define(INTERNET_ALIASES, `Cwucbarpa arpa arpavax ucb-arpa kim ucbkim')
define(UUCP_NAME, DUucbarpa)
define(UUCP_ALIASES, CUucbarpa)
define(UUCP_HOSTS_FILE, ../machdep/uucp.ucbarpa.m4)
divert(0)dnl
include(ucbproto.mc)dnl
