divert(10)dnl
#
# cogsci UUCP/TCP configuration file
#
# @(#)cogsci.mc	1.5 (Berkeley) 3/31/88
#
define(INTERNET_ALIASES, Cwcogsci ucbcogsci)
define(UUCP_NAME, DUcogsci)
define(UUCP_ALIASES, CUcogsci)
define(UUCP_HOSTS_FILE, ../machdep/uucp.cogsci.m4)
define(EXTERNAL_VERSION, ``#'	`@(#)cogsci.mc	1.5' (Berkeley) `3/31/88'')
divert(0)dnl
include(ucbproto.mc)
