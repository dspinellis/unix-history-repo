/* tailor.c - ISODE tailoring */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/tailor.c,v 7.9 91/02/22 09:16:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/tailor.c,v 7.9 91/02/22 09:16:09 mrose Interim $
 *
 *
 * $Log:	tailor.c,v $
 * Revision 7.9  91/02/22  09:16:09  mrose
 * Interim 6.8
 * 
 * Revision 7.8  90/11/21  11:35:35  mrose
 * update
 * 
 * Revision 7.7  90/10/15  18:19:40  mrose
 * sync
 * 
 * Revision 7.6  90/07/09  14:32:25  mrose
 * sync
 * 
 * Revision 7.5  90/03/08  08:01:12  mrose
 * ending
 * 
 * Revision 7.4  90/02/19  13:07:20  mrose
 * update
 * 
 * Revision 7.3  90/01/11  18:35:16  mrose
 * real-sync
 * 
 * Revision 7.2  89/12/12  16:14:57  mrose
 * localHost
 * 
 * Revision 7.1  89/11/30  23:53:32  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:23:45  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "isoaddrs.h"
#include "tailor.h"


#define	SUBNET_MAX	(6 + 10)  /* maximum number of subnet entries */

/*    DATA */

struct pair {
    char   *p_name;
    int	    p_value;
};


static char   *isotailor = "isotailor";


char   *isodename = "";

char   *isodebinpath = BINPATH;
char   *isodesbinpath = SBINPATH;
char   *isodetcpath = ETCPATH;
char   *isodelogpath = LOGPATH;


static struct pair ll_pairs[] = {
    "none", LLOG_NONE,
    "fatal", LLOG_FATAL,
    "exceptions", LLOG_EXCEPTIONS,
    "notice", LLOG_NOTICE,
    "pdus", LLOG_PDUS,
    "trace", LLOG_TRACE,
    "debug", LLOG_DEBUG,
    "all", LLOG_ALL,

    NULL
};

static char *compatdebug = "none";
LLog _compat_log = {
    "%d.log", NULLCP, "compat", LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *compat_log = &_compat_log;

static char *addrdebug = "none";
LLog _addr_log = {
    "%d.log", "addr", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *addr_log = &_addr_log;

static char *tsapdebug = "none";
LLog _tsap_log = {
    "%d.tpkt", "tsap", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *tsap_log = &_tsap_log;

static char *ssapdebug = "none";
LLog _ssap_log = {
    "%d.spkt", "ssap", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *ssap_log = &_ssap_log;

static char *psapdebug = "none";
LLog _psap_log = {
    "%d.pe", NULLCP, "psap", LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *psap_log = &_psap_log;

static char *psap2debug = "none";
LLog _psap2_log = {
    "%d.ppkt", "psap2", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *psap2_log = &_psap2_log;

static char *acsapdebug = "none";
LLog _acsap_log = {
    "%d.acpkt", "acsap", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *acsap_log = &_acsap_log;

static char *rtsapdebug = "none";
LLog _rtsap_log = {
    "%d.rtpkt", "rtsap", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *rtsap_log = &_rtsap_log;

static char *rosapdebug = "none";
LLog _rosap_log = {
    "%d.ropkt", "rosap", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *rosap_log = &_rosap_log;


#define	TS_MASK	"\020\01TCP\02X25\03BRG\04TP4"

static struct pair ts_pairs[] = {
    "tcp", TS_TCP,
    "x25", TS_X25,
    "bridge", TS_BRG,
    "tp4", TS_TP4,
    "all", TS_ALL,

    NULL
};

static char *_ts_stacks = "all";
int	ts_stacks = TS_ALL;

static char *_ts_interim = "";
struct ts_interim ts_interim[SUBNET_MAX + 1] = {
    "realNS",	NULL,	SUBNET_REALNS,		NA_NSAP,	"",	0,
    "Int-X25",	NULL,	SUBNET_INT_X25,		NA_X25,		"",	0,
    "Internet",	NULL,	SUBNET_INTERNET,	NA_TCP,		"",	0,
    "Janet",	NULL,	SUBNET_DYNAMIC - 1,	NA_X25,		"",	0,
    "localTCP", NULL,   SUBNET_DYNAMIC - 2,	NA_TCP,		"",	0,
    "IXI",	NULL,	SUBNET_DYNAMIC - 3,	NA_X25,		"",	0,

    NULL
};

static char *_ts_communities = "all";
int    ts_communities[SUBNET_MAX + 1];

static char *_ts_comm_nsap_default = "realNS";
int	ts_comm_nsap_default = SUBNET_REALNS;

static char *_ts_comm_x25_default = "Int-X25";
int	ts_comm_x25_default = SUBNET_INT_X25;

static char *_ts_comm_tcp_default = "Internet";
int	ts_comm_tcp_default = SUBNET_INTERNET;

int	tsb_communities[SUBNET_MAX + 1];
char	*tsb_addresses[SUBNET_MAX + 1];
static char	*_tsb_config = "";
char	*tsb_default_address = "undefined";


#ifdef  X25
char    *x25_local_dte = "";
char    *x25_local_pid = "";

static char    *x25_intl_zero_str = "off";
char     x25_intl_zero = 0;

static char     *x25_strip_dnic_str = "off";
char    x25_strip_dnic = 0;

char    *x25_dnic_prefix = "";

		/* 0 = don't request/allow reverse charging
		 * 1 = reverse charge request/allowed
		 */
static char *reverse_charge_default = "0";
u_char reverse_charge = 0;

		/* 0= default recv packet size.
		 * 16, 32, 64, 128,
		 * 256, 512, 1024
		 * ( octets in decimal )
		 */
static char *recvpktsize_default = "0";
u_short recvpktsize = 0;

		/* same as above, but for send packet size */
static char *sendpktsize_default = "0";
u_short sendpktsize = 0;

		/* 0= default recv window size.
		 * 7, 127 ( in decimal )
		 */
static char *recvwndsize_default = "0";
u_char recvwndsize = 0;

		/* same as above, but for send window size */
static char *sendwndsize_default = "0";
u_char sendwndsize = 0;

		/* 0= default recv throughtput.
		 * 3= 75    8= 2400
		 * 4= 150   9= 4800
		 * 5= 300  10= 9600
		 * 6= 600  11= 19200
		 * 7= 1200 12= 48000
		 * ( bps in decimal )
		 */
static char *recvthruput_default = "0";
u_char recvthruput = 0;

		/* same as above, but for send throughput */
		/* 1 = use closed user group in cug_index */
static char *sendthruput_default = "0";
u_char sendthruput = 0;

		/* 0 = no closed user group. */
		/* 1 = use closed user group in cug_index */
static char *cug_req_default = "0";
u_char cug_req = 0;

		/* valid when cug_req= 1.
		 * 0x00 ~ 0x99 ( closed user group in BCD)
		 */
static char *cug_index_default = "0";
u_char     cug_index = 0;

		/* 0= don't use fast select
		 * 1= clear is fast select response
		 * 2= clear or call accepted
		 *    is fast select response
		 */
static char *fast_select_type_default = "0";
u_char fast_select_type = 0;

		/* 0= no RPOA transit request
		 * 1= use RPOA transit request in rpoa
		 */
static char *rpoa_req_default = "0";
u_char rpoa_req = 0;

		/* valid when rpoa_req= 1 */
		/* 0x0000 ~ 0x9999 (RPOA transit group in BCD) */
static char *rpoa_default = "0";
u_short rpoa = 0;

static char *x25debug = "none";
static LLog _x25_log = {
    "x25log", "x25", NULLCP, LLOG_NONE, LLOG_NONE, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *x25_log = &_x25_log;

#ifdef CAMTEC_CCL
static char *x25_outgoing_port_str = "A";
char    x25_outgoing_port = 'A';
#endif
#endif


#ifdef	BRIDGE_X25
char    *x25_bridge_host = "x25bridge";
char    *x25_bridge_addr = "000021000018";
char	*x25_bridge_listen = "";
char	*x25_bridge_pid = "";
char	*x25_bridge_discrim = "0000";
#endif


#if	defined (BRIDGE_X25) || defined (X25)
static char *x25_bridge_port_default = "146";
u_short x25_bridge_port = 0;
#endif


static char *_ses_ab_timer = "30";    /* drain for 30 seconds on ABORTs */
int	ses_ab_timer = 30;

static char *_ses_dn_timer = "30";    /* drain for 30 seconds on DISCONNECTs */
int	ses_dn_timer = 30;

static char *_ses_rf_timer = "30";    /* drain for 30 seconds on REFUSEs */
int	ses_rf_timer = 30;


char	ns_enabled = 0;
static char *usens = "off";

char   *ns_address = "undefined";

/*  */

static struct bind {
    char   *b_key;

    char  **b_value;
    int	    b_dynamic;
}	binds[] = {
    "localname",	&isodename,			0,
    "binpath",		&isodebinpath,			0,
    "sbinpath",		&isodesbinpath,			0,
    "etcpath",		&isodetcpath,			0,
    "logpath",		&isodelogpath,			0,

    "compatlevel",	&compatdebug,			0,
    "compatfile",	&_compat_log.ll_file,		0,

    "addrlevel",	&addrdebug,			0,
    "addrfile",		&_addr_log.ll_file,		0,

    "tsaplevel",	&tsapdebug,			0,
    "tsapfile",		&_tsap_log.ll_file,		0,

    "ssaplevel",	&ssapdebug,			0,
    "ssapfile",		&_ssap_log.ll_file,		0,

    "psaplevel",	&psapdebug,			0,
    "psapfile",		&_psap_log.ll_file,		0,

    "psap2level",	&psap2debug,			0,
    "psap2file",	&_psap2_log.ll_file,		0,

    "acsaplevel",	&acsapdebug,			0,
    "acsapfile",	&_acsap_log.ll_file,		0,

    "rtsaplevel",	&rtsapdebug,			0,
    "rtsapfile",	&_rtsap_log.ll_file,		0,

    "rosaplevel",	&rosapdebug,			0,
    "rosapfile",	&_rosap_log.ll_file,		0,

    "ts_stacks",	&_ts_stacks,			0,
    "ts_interim",	&_ts_interim,			0,
    "ts_communities",	&_ts_communities,		0,

    "default_nsap_community", &_ts_comm_nsap_default,	0,
    "default_x25_community", &_ts_comm_x25_default,	0,
    "default_tcp_community", &_ts_comm_tcp_default,	0,

    "tsb_communities",	&_tsb_config,			0,
    "tsb_default_address", &tsb_default_address,	0,

#ifdef  X25
    "x25_local_dte",	&x25_local_dte,			0,
    "x25_local_pid",	&x25_local_pid,			0,
    "x25_dnic_prefix",	&x25_dnic_prefix,		0,
    "x25_intl_zero",	&x25_intl_zero_str,		0,
    "x25_strip_dnic",	&x25_strip_dnic_str,		0,

    "reverse_charge",   &reverse_charge_default,	0,
    "recvpktsize",      &recvpktsize_default,		0,
    "sendpktsize",      &sendpktsize_default,		0,
    "recvwndsize",      &recvwndsize_default,		0,
    "sendwndsize",      &sendwndsize_default,		0,
    "recvthruput",      &recvthruput_default,		0,
    "sendthruput",      &sendthruput_default,		0,
    "cug_req",          &cug_req_default,		0,
    "cug_index",        &cug_index_default,		0,
    "fast_select_type", &fast_select_type_default,	0,
    "rpoa_req",         &rpoa_req_default,		0,
    "rpoa",             &rpoa_default,			0,

    "x25level",		&x25debug,			0,
    "x25file",		&_x25_log.ll_file,		0,

#ifdef CAMTEC_CCL
    "x25_outgoing_port",        &x25_outgoing_port_str,	0,
#endif
#endif

#ifdef	BRIDGE_X25
    "x25_bridge_host",		&x25_bridge_host,	0,
    "x25_bridge_addr",		&x25_bridge_addr,	0,
    "x25_bridge_listen",	&x25_bridge_listen,	0,
    "x25_bridge_pid",		&x25_bridge_pid,	0,
    "x25_bridge_discrim",	&x25_bridge_discrim,	0,
#endif

#if	defined (BRIDGE_X25) || defined (X25)
    "x25_bridge_port",		&x25_bridge_port_default,0,
#endif


    "ses_abort_timer",		&_ses_ab_timer,		0,
    "ses_disconnect_timer", 	&_ses_dn_timer,		0,
    "ses_refuse_timer",		&_ses_rf_timer,		0,

    "ns_enable",		&usens,			0,
    "ns_address",		&ns_address,		0,

    NULL
};


int	tailor_read ();
char   *tailor_value ();

/*  */

char  *isodesetailor (file)
char  *file;
{
    char   *ofile = isotailor;

    if ((isotailor = file) == NULLCP)
	isotailor = "isotailor";

    return ofile;
}

/*  */

void	isodetailor (myname, wantuser)
char   *myname;
int	wantuser;
{
    register char *hp,
		  *mp;
    char   buffer[BUFSIZ];
    static int  inited = 0;

    if (inited)
	return;
    inited = 1;

    tsb_addresses[0] = NULL;

    tailor_read (isodefile (isotailor, 0));

    if (wantuser) {
	if ((hp = getenv ("HOME")) == NULL)
	    hp = ".";
	if (myname) {
	    if (mp = rindex (myname, '/'))
		mp++;
	    if (mp == NULL || *mp == NULL)
		mp = myname;
	}
	else
	    mp = "isode";
	(void) sprintf (buffer, "%s/.%s_tailor", hp, mp);
	tailor_read (buffer);
    }

    isodexport (myname);
}

/*  */

static int  tailor_read (file)
char   *file;
{
    register char  *bp,
                   *cp;
    char    buffer[BUFSIZ];
    register FILE *fp;

    if (fp = fopen (file, "r")) {
	while (fgets (buffer, sizeof buffer, fp)) {
	    if ((cp = index (buffer, '\n')) == NULL) {
		fprintf (stderr, "%s: line too long\n", file);
		break;
	    }
	    *cp = NULL;
	    if (*buffer == '#' || *buffer == NULL)
		continue;
	    if ((bp = index (buffer, ':')) == NULL) {
		fprintf (stderr, "%s: invalid syntax in \"%s\"\n",
			 file, buffer);
		break;
	    }
	    for (cp = bp - 1; cp >= buffer; cp--)
		if (isspace ((u_char) *cp))
		    *cp = NULL;
		else
		    break;

	    *bp++ = NULL;
	    while (isspace ((u_char) *bp))
		*bp++ = NULL;

	    for (cp = bp + strlen (bp) - 1; cp >= bp; cp--)
		if (isspace ((u_char) *cp))
		    *cp = NULL;
	        else
		    break;
			
	    if ((cp = tailor_value (bp))
		     && isodesetvar (buffer, cp, 1) == NOTOK)
		free (cp);
	}

	(void) fclose (fp);
    }
}

/*  */

int	isodesetvar (name, value, dynamic)
char   *name,
       *value;
int	dynamic;
{
    register struct bind   *b;

    for (b = binds; b -> b_key; b++)
	if (strcmp (b -> b_key, name) == 0) {
	    if (b -> b_dynamic && *b -> b_value)
		free (*b -> b_value);
	    *b -> b_value = value, b -> b_dynamic = dynamic;
	    return OK;
	}

    return NOTOK;
}

/*  */

void	isodexport (myname)
char   *myname;
{
    compat_log -> ll_events = events_value (ll_pairs, compatdebug, "compatlevel");
    addr_log -> ll_events = events_value (ll_pairs, addrdebug, "addrlevel");
    tsap_log -> ll_events = events_value (ll_pairs, tsapdebug, "tsaplevel");
    ssap_log -> ll_events = events_value (ll_pairs, ssapdebug, "ssaplevel");
    psap_log -> ll_events = events_value (ll_pairs, psapdebug, "psaplevel");
    psap2_log -> ll_events = events_value (ll_pairs, psap2debug, "psap2level");
    acsap_log -> ll_events = events_value (ll_pairs, acsapdebug, "acsaplevel");
    rtsap_log -> ll_events = events_value (ll_pairs, rtsapdebug, "rtsaplevel");
    rosap_log -> ll_events = events_value (ll_pairs, rosapdebug, "rosaplevel");
#ifdef	X25
    x25_log -> ll_events = events_value (ll_pairs, x25debug, "x25level");
#endif

    ll_hdinit (compat_log, myname);
    ll_hdinit (addr_log, myname);
    ll_hdinit (tsap_log, myname);
    ll_hdinit (ssap_log, myname);
    ll_hdinit (psap_log, myname);
    ll_hdinit (psap2_log, myname);
    ll_hdinit (acsap_log, myname);
    ll_hdinit (rtsap_log, myname);
    ll_hdinit (rosap_log, myname);
#ifdef X25
    ll_hdinit (x25_log, myname);
#endif

    ts_stacks = events_value (ts_pairs, _ts_stacks, "ts_stacks");
#ifndef	TCP
    ts_stacks &= ~TS_TCP;
#endif
#ifndef	X25
    ts_stacks &= ~TS_X25;
#endif
#ifndef	BRIDGE_X25
    ts_stacks &= ~TS_BRG;
#endif
#ifndef	TP4
    ts_stacks &= ~TS_TP4;
#endif

    {
	register int  *ip,
		      *jp,
		      *kp;
	int	j;
	register char *cp,
		      *adrp,
		     **ap,
		     **cpp;
	register struct ts_interim *ts;
	static struct ts_interim *te = NULL;
	char    buffer[BUFSIZ],
	       *vec[NVEC + NSLACK + 1];

	if (te)
	    te -> ts_name = NULL;
	for (ts = ts_interim; ts -> ts_name; ts++)
	    if (macro2comm (ts -> ts_name, ts) == NOTOK)
		fprintf (stderr, "internal error for community \"%s\"\n",
			 ts -> ts_name);
	if (te == NULL)
	    te = ts;

	(void) strcpy (buffer, _ts_interim);
	(void) str2vec (buffer, ap = vec);
	j = SUBNET_DYNAMIC;
	while (cp = *ap++) {
	    register struct ts_interim *tp;

	    ts -> ts_subnet = 0;
	    if (macro2comm (cp, ts) == NOTOK) {
		fprintf (stderr, "invalid community name \"%s\"\n", cp);
		break;
	    }
	    for (tp = ts_interim; tp < ts; tp++)
		if (tp -> ts_length == ts -> ts_length
		        && bcmp (tp -> ts_prefix, ts -> ts_prefix,
				 tp -> ts_length) == 0) {
		    fprintf (stderr,
			     "duplicate prefixes for communities \"%s\" and \"%s\"\n",
			     tp -> ts_name, cp);
		    break;
		}
	    if (tp < ts)
		continue;

	    ts -> ts_name = strdup (cp);
	    if (ts -> ts_subnet == 0)
		ts -> ts_subnet = j++;
	    ts++;
	}
	ts -> ts_name = NULL;

	(void) strcpy (buffer, _ts_communities);
	(void) str2vec (buffer, ap = vec);
	ip = ts_communities;
	while (cp = *ap++) {
	    if (strcmp (cp, "all") == 0) {
		for (ts = ts_interim; ts -> ts_name; ts++) {
		    for (jp = ts_communities; jp < ip; jp++)
			if (*jp == ts -> ts_subnet)
			    break;
		    if (jp >= ip)
			*ip++ = ts -> ts_subnet;
		}
		break;
	    }

	    for (ts = ts_interim; ts -> ts_name; ts++)
		if (lexequ (ts -> ts_name, cp) == 0)
		    break;
	    if (!ts -> ts_name) {
		fprintf (stderr,
			 "unknown community name \"%s\" for variable ts_communities\n",
			 cp);
		continue;
	    }

	    for (jp = ts_communities; jp < ip; jp++)
		if (*jp == ts -> ts_subnet)
		    break;
	    if (jp >= ip)
		*ip++ = ts -> ts_subnet;
	}
	*ip = NULL;

	for (ap = tsb_addresses; *ap; ap++)
	    free (*ap);
	(void) strcpy (buffer, _tsb_config);
	(void) str2vec (buffer, ap = vec);
	ip = tsb_communities, cpp = tsb_addresses;
	while (cp = *ap++) {
	    if ((adrp = *ap++) == NULLCP) {
		fprintf (stderr,
			 "missing address for tsb_community \"%s\"\n", cp);
		break;
	    }

	    for (ts = ts_interim; ts -> ts_name; ts++)
		if (lexequ (ts -> ts_name, cp) == 0)
		    break;
	    if (!ts -> ts_name) {
		fprintf (stderr,
			 "unknown community name \"%s\" for variable tsb_communities\n",
			 cp);
		continue;
	    }

	    for (jp = tsb_communities; jp < ip; jp++)
		if (*jp == ts -> ts_subnet)
		    break;
	    if (jp >= ip) {
		*ip++ = ts -> ts_subnet, *cpp++ = strdup (adrp);

		for (kp = ts_communities; *kp; kp++)
		    if (*kp == ts -> ts_subnet)
			break;
		if (!*kp)
		    *kp++ = ts -> ts_subnet, *kp = NULL;
	    }
	}
	*ip = NULL, *cpp = NULLCP;

	cp = _ts_comm_nsap_default;
	for (ts = ts_interim; ts -> ts_name; ts++)
	    if (lexequ (ts -> ts_name, cp) == 0)
		break;
	if (ts -> ts_name && ts -> ts_syntax == NA_NSAP)
	    ts_comm_nsap_default = ts -> ts_subnet;
	else
	    fprintf (stderr,
		     "bad community name \"%s\" for variable default_nsap_community\n",
		     cp);

	cp = _ts_comm_x25_default;
	for (ts = ts_interim; ts -> ts_name; ts++)
	    if (lexequ (ts -> ts_name, cp) == 0)
		break;
	if (ts -> ts_name && ts -> ts_syntax == NA_X25)
	    ts_comm_x25_default = ts -> ts_subnet;
	else
	    fprintf (stderr,
		     "bad community name \"%s\" for variable default_x25_community\n",
		     cp);

	cp = _ts_comm_tcp_default;
	for (ts = ts_interim; ts -> ts_name; ts++)
	    if (lexequ (ts -> ts_name, cp) == 0)
		break;
	if (ts -> ts_name && ts -> ts_syntax == NA_TCP)
	    ts_comm_tcp_default = ts -> ts_subnet;
	else
	    fprintf (stderr,
		     "bad community name \"%s\" for variable default_tcp_community\n",
		     cp);

	if (addr_log -> ll_events & LLOG_DEBUG) {
	    LLOG (addr_log, LLOG_DEBUG,
		  ("ts_stacks: %s", sprintb (ts_stacks, TS_MASK)));

	    for (ts = ts_interim; ts -> ts_name; ts++) {
		LLOG (addr_log, LLOG_DEBUG,
		      ("community %s value \"%s\" subnet %d syntax %d",
		       ts -> ts_name, ts -> ts_value ? ts -> ts_value : "",
		       ts -> ts_subnet, ts -> ts_syntax));
		buffer[explode(buffer, (u_char *) ts -> ts_prefix,
			       ts -> ts_length)] = NULL;
		LLOG (addr_log, LLOG_DEBUG,
		      ("    prefix \"%s\" (%d octets)", buffer,
		       ts -> ts_length));
	    }

	    for (ip = ts_communities; *ip; ip++)
		LLOG (addr_log, LLOG_DEBUG, ("community %d enabled", *ip));

	    LLOG (addr_log, LLOG_DEBUG,
		  ("default communities: nsap=%d x25=%d tcp=%d",
		   ts_comm_nsap_default, ts_comm_x25_default,
		   ts_comm_tcp_default));

	    for (ip = tsb_communities, cpp = tsb_addresses; *ip; ip++, cpp++) {
		LLOG (addr_log, LLOG_DEBUG,
		      ("TSB for community %d residing at %s", *ip, *cpp));
		if (str2taddr (*cpp) == NULLTA)
		    LLOG (addr_log, LLOG_EXCEPTIONS,
			  ("invalid address for TSB to community %d: %s",
			   *ip, *cpp));
	    }
	}
    }

#ifdef	X25
    reverse_charge = (u_char) atoi (reverse_charge_default);
    recvpktsize = (u_short) atoi (recvpktsize_default);
    sendpktsize = (u_short) atoi (sendpktsize_default);
    recvwndsize = (u_char) atoi (recvwndsize_default);
    sendwndsize = (u_char) atoi (sendwndsize_default);
    recvthruput = (u_char) atoi (recvthruput_default);
    sendthruput = (u_char) atoi (sendthruput_default);
    cug_req = (u_char) atoi (cug_req_default);
    cug_index = (u_char) atoi (cug_index_default);
    fast_select_type = (u_char) atoi (fast_select_type_default);
    rpoa_req = atoi (rpoa_req_default);
    rpoa = (u_short) atoi (rpoa_default);

#ifdef	CAMTEC_CCL
    x25_outgoing_port = *x25_outgoing_port_str;
#endif

    x25_intl_zero = !strcmp (x25_intl_zero_str, "on");
    x25_strip_dnic =  !strcmp (x25_strip_dnic_str, "on");
#endif

#if	defined (BRIDGE_X25) || defined (X25)
    x25_bridge_port = htons ((u_short) atoi (x25_bridge_port_default));
#endif

    ns_enabled = !strcmp (usens, "on");
}

/*  */

#define	QUOTE	'\\'


static char *tailor_value (s)
register char   *s;
{
    register int    i,
                    r;
    register char  *bp;
    char    buffer[BUFSIZ];

    for (bp = buffer; *s; bp++, s++)
	if (*s != QUOTE)
	    *bp = *s;
	else
	    switch (*++s) {
		case '0':
		    *bp = '\0';
		    break;
		case 'b':
		    *bp = '\b';
		    break;
		case 'f':
		    *bp = '\f';
		    break;
		case 'n':
		    *bp = '\n';
		    break;
		case 'r':
		    *bp = '\r';
		    break;
		case 't':
		    *bp = '\t';
		    break;

		case NULL: s--;
		case QUOTE: 
		    *bp = QUOTE;
		    break;

		default: 
		    if (!isdigit ((u_char) *s)) {
			*bp++ = QUOTE;
			*bp = *s;
			break;
		    }
		    r = *s != '0' ? 10 : 8;
		    for (i = 0; isdigit ((u_char) *s); s++)
			i = i * r + *s - '0';
		    s--;
		    *bp = toascii (i);
		    break;
	    }
    *bp = NULL;

    if ((bp = malloc ((unsigned) (strlen (buffer) + 1))) != NULL)
	(void) strcpy (bp, buffer);

    return bp;
}

/*  */

static int  events_value (pairs, s, var)
struct pair *pairs;
char   *s,
       *var;
{
    int     value;
    register char  *cp,
                  **ap;
    register struct pair   *pp;
    char    buffer[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];

    value = 0;
    (void) strcpy (buffer, s);
    (void) str2vec (buffer, ap = vec);
    while (cp = *ap++) {
	for (pp = pairs; pp -> p_name; pp++) 
	    if (strcmp (pp -> p_name, cp) == 0) {
		value |= pp -> p_value;
		break;
	    }
	if (!pp -> p_name)
	    fprintf (stderr, "unknown value \"%s\" for variable %s\n",
		     cp, var);
    }

    return value;
}

/*  */

#ifdef PEP_TEST
tailorfree () {
    register struct bind   *b;

    for (b = binds; b -> b_key; b++)
	if (b -> b_dynamic && *b -> b_value) {
	    free (*b -> b_value);
	    *b -> b_value = NULL;
	}

    ll_hdfree (compat_log);
    ll_hdfree (addr_log);
    ll_hdfree (tsap_log);
    ll_hdfree (ssap_log);
    ll_hdfree (psap_log);
    ll_hdfree (psap2_log);
    ll_hdfree (acsap_log);
    ll_hdfree (rtsap_log);
    ll_hdfree (rosap_log);
#ifdef X25
    ll_hdfree (x25_log);
#endif

    free_macros ();
    free_bp ();
    free_oid ();
}


ll_hdfree (lp)
register LLog *lp;
{
    if (lp -> ll_stat & LLOGHDR)
        free (lp -> ll_hdr);
    lp -> ll_stat &= ~LLOGHDR;
}
#endif
