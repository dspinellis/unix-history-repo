/* include.h */

/* EGP User Process, ISI 23-Jun-84 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>			/* init.c, egp2.c, rt_egp.c */

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>

#include <stdio.h>
#include <netdb.h>
#include <errno.h>

#include <net/if.h>			/* init.c */
#include <net/route.h>

/* definitions from routed */

#include "trace_egp.h"
#include "if.h"
#include "rt_table.h"


/* new definitions */

#include "defs.h"
#include "egp.h"
#include "egp_param.h"		/* used by egp.c, init.c */
