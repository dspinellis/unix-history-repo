divert(10)dnl
#
# Prototype configuration file for a UUCP hub on a TCP/IP (SMTP) based network
#
# @(#)tcpuucpproto.mc	1.2 (Berkeley) 1/24/89
#
define(DOMAIN, `DDYOUR_DOMAIN_GOES_HERE')
define(BITNET_RELAY, `DBYOUR_BITNET_RELAY_GOES_HERE')
define(CSNET_RELAY, `DCYOUR_CSNET_RELAY_GOES_HERE')
define(UUCP_NAME, `DUYOUR_UUCP_NAME_GOES_HERE')
define(UUCP_ALIASES, CUYOUR_UUCP_ALIASES_GO_HERE)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.proto.m4)
define(EXTERNAL_VERSION, ``#'	`@(#)tcpuucpproto.mc	1.2' (Berkeley) `1/24/89'')
divert(0)dnl
include(proto.mc)dnl
