divert(10)dnl
#
# Prototype configuration file for systems on TCP/IP (SMTP) based networks
#
# @(#)tcpproto.mc	1.2 (Berkeley) 1/24/89
#
define(DOMAIN, `DDYOUR_DOMAIN_GOES_HERE')
define(UUCP_RELAY, DRYOUR_UUCP_RELAY_GOES_HERE)
define(BITNET_RELAY, `DBYOUR_BITNET_RELAY_GOES_HERE')
define(CSNET_RELAY, `DCYOUR_CSNET_RELAY_GOES_HERE')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`@(#)tcpproto.mc	1.2' (Berkeley) `1/24/89'')
divert(0)dnl
include(proto.mc)dnl
