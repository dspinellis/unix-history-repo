divert(10)dnl
#
# okeeffe UUCP/TCP configuration file
#
# @(#)okeeffe.mc	1.2 (Berkeley) 3/31/88
#
define(INTERNET_ALIASES, Cwokeeffe ucbokeeffe)
define(UUCP_NAME, DUokeeffe)
define(UUCP_ALIASES, CUokeeffe)
define(UUCP_HOSTS_FILE, ../machdep/uucp.okeeffe.m4)
define(EXTERNAL_VERSION, ``#'	`@(#)okeeffe.mc	1.2' (Berkeley) `3/31/88'')
divert(0)dnl
include(ucbproto.mc)
