/* conf.c - DSAP Configuration */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/conf.c,v 7.3 91/02/22 09:18:50 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/conf.c,v 7.3 91/02/22 09:18:50 mrose Interim $
 *
 *
 * $Log:	conf.c,v $
 * Revision 7.3  91/02/22  09:18:50  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:41:31  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:34:12  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:47:42  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "tailor.h"

int     sizelimit = 20;
int	timelimit = 30;
	
char    *oidtable =  "oidtable";
char    *tailfile =  "dsaptailor";
char    *isotailor = NULLCP;

char    *dsa_address = NULLCP;
char    *myname = NULLCP;

char	*local_dit= NULLCP;	/* the part of the tree local to the users */
char 	dishinit = FALSE;

int     dsap_id,        /* global last id sent */
	dsap_ad;        /* global association descriptor */

time_t  cache_timeout = 21600;	/* Keep cache entries for 6 hours */

static  LLog    _ldsap_log =
{
	"dsap.log",
	NULLCP,
	NULLCP,
	LLOG_FATAL | LLOG_EXCEPTIONS,
	LLOG_NONE,
	-1,
	LLOGZER | LLOGCRT | LLOGCLS,
	NOTOK
};

LLog    *log_dsap = &_ldsap_log;

#ifndef NO_STATS

static  LLog    lstat_log =
{
	"quipu.log",
	NULLCP,
	NULLCP,
	LLOG_ALL,
	LLOG_NONE,
	-1,
	LLOGCRT | LLOGZER,
	NOTOK
};

LLog    *log_stat = &lstat_log;

#endif

int     oidformat = 1;      /* oid format, 1=part, 2=full, 3=numeric */

/* a quick def incase quipu/malloc.c is not compiled in !!! */
unsigned mem_heap = 0;

char    *dsaoidtable =  "oidtable";
char    *dsatailfile =  "quiputailor";
char    *treedir  =  "quipu-db";
int	no_dsp_chain = FALSE;
int	no_last_mod = FALSE;
char	startup_update = FALSE;
int	search_level = 2;	/* do NOT allow country level searching */
int	read_only = FALSE;	/* Prevent DIT modification */

				/* admin limits */
int	admin_size = 100;	/* 100 entries */
time_t	admin_time = 120;	/* don't spend more than 2 minutes on a task */
time_t	conn_timeout = 300;	/* don't hold an unused connection open for more than 5 minutes */
time_t	nsap_timeout = 45;	/* after 45 seconds assume nsap has failed */

time_t  slave_timeout = 0;	/* Update slaves every 'cache_time' seconds */
time_t  retry_timeout = 0;	/* Test DSA for reliability after 'cache_time' seconds */

unsigned watchdog_time  = 5 * 60;	/* allow lower layers 5 minutes per *async* operation */
unsigned watchdog_delta = 5;	/* allow 5 second for timeout to propagate */

char dsa_mode = FALSE;
