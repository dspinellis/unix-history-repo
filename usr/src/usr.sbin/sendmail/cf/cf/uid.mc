divert(10)dnl
#
# Prototype configuration file for systems only on UUCP networks
#
# @(#)uucpproto.mc	1.2 (Berkeley) 1/24/89
#
define(DOMAIN, `DDUUCP')
define(UUCP_NAME, DUid)
define(UUCP_ALIASES, CUid uid)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.proto.m4)
define(UUCP_ONLY, 1)
define(EXTERNAL_VERSION, ``#'	`@(#)uucpproto.mc	1.2' (Berkeley) `1/24/89'')
divert(0)dnl
include(proto.mc)dnl
