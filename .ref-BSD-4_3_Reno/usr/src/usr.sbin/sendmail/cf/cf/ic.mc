divert(10)dnl
#
# ic configuration file
#
# @(#)ic.mc	1.2 (Berkeley) 1/3/89
#
define(DOMAIN, `DDBerkeley.EDU')
define(UUCP_NAME, DUic)
define(UUCP_ALIASES, CUic ucbic)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.ic.m4)
define(INTERNET_RELAY, `DAcad.Berkeley.EDU')
define(UUCP_RELAY, `DRcad.Berkeley.EDU')
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`@(#)ic.mc	1.2' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)dnl
