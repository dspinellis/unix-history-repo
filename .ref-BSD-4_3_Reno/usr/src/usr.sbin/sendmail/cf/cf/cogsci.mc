divert(10)dnl
#
# cogsci configuration file
#
# @(#)cogsci.mc	1.6 (Berkeley) 1/3/89
#
define(DOMAIN, `DDBerkeley.EDU')
define(UUCP_NAME, DUcogsci)
define(UUCP_ALIASES, CUcogsci)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.cogsci.m4)
define(INTERNET_RELAY, `DAucbvax.Berkeley.EDU')
define(UUCP_RELAY, DRucbvax.Berkeley.EDU)
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`@(#)cogsci.mc	1.6' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)
