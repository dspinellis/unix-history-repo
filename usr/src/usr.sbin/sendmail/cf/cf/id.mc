divert(10)dnl
#
# ic configuration file
#
# From: @(#)ic.mc	1.2 (Berkeley) 1/3/89
#
define(UUCP_NAME, DUid)
define(UUCP_ALIASES, CUid uid)
define(UUCP_HOSTS_FILE, ../sitedep/uucp.id.m4)
define(INTERNET_RELAY, `DAjpunix')
define(UUCP_RELAY, `DRjpunix')
define(BITNET_RELAY, `DBricevm1.Rice.EDU')
define(CSNET_RELAY, `DCrelay.cs.net')
define(ARPAKLUDGE, `1')
define(EXTERNAL_VERSION, ``#'	`based on: @(#)ic.mc	1.2' (Berkeley) `1/3/89'')
divert(0)dnl
include(proto.mc)dnl
