divert(10)dnl
#
# Generic configuration file for Berkeley machines.
#
# @(#)ucbtcp.mc	1.3 (Berkeley) 1/3/89
#
define(DOMAIN, `DDBerkeley.EDU')
define(INTERNET_RELAY, `DAucbvax.Berkeley.EDU')
define(UUCP_RELAY, DRucbvax.Berkeley.EDU)
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`@(#)ucbtcp.mc	1.3' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)dnl
