divert(10)dnl
#
# okeeffe configuration file
#
# @(#)okeeffe.mc	1.3 (Berkeley) 1/3/89
#
define(DOMAIN, `DDBerkeley.EDU')
define(UUCP_NAME, DUokeeffe)
define(UUCP_ALIASES, CUokeeffe)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.okeeffe.m4)
define(INTERNET_RELAY, `DAucbvax.Berkeley.EDU')
define(UUCP_RELAY, DRucbvax.Berkeley.EDU)
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(EXTERNAL_VERSION, ``#'	`@(#)okeeffe.mc	1.3' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)
