divert(10)dnl
#
# Computer Center configuration file
#
# @(#)cc.mc	1.2 (Berkeley) 1/3/89
#
define(INTERNET_RELAY, `DAviolet.Berkeley.EDU')
define(DOMAIN, `DDBerkeley.EDU')
define(UUCP_RELAY, `DRucbvax.Berkeley.EDU')
define(BITNET_RELAY, `DBjade.Berkeley.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`@(#)cc.mc	1.2' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)dnl
