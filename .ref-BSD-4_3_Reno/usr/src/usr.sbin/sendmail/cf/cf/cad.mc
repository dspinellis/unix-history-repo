divert(10)dnl
#
# cad configuration file
#
# @(#)cad.mc	1.11 (Berkeley) 1/24/89
#
define(DOMAIN, `DDBerkeley.EDU')
define(UUCP_NAME, DUucbcad)
define(UUCP_ALIASES, CUucbcad)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.cad.m4)
define(UUCP_RELAY, DRucbvax.Berkeley.EDU)
define(SMTPUUCP, ../sitedep/smtpuucp.cad.m4)
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1');
define(EXTERNAL_VERSION, ``#'	`@(#)cad.mc	1.11' (Berkeley) `1/24/89'')
divert(0)dnl
include(proto.mc)dnl
