/* sys_init.c - System tailoring initialisation */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/sys_init.c,v 7.2 91/02/22 09:39:52 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/sys_init.c,v 7.2 91/02/22 09:39:52 mrose Interim $
 *
 *
 * $Log:	sys_init.c,v $
 * Revision 7.2  91/02/22  09:39:52  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:45  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:07  mrose
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


#include <stdio.h>
#include "quipu/oid.h"
#include "tailor.h"
#include "logger.h"

extern char *dsaoidtable;
extern LLog * log_dsap;
extern time_t cache_timeout;
extern time_t retry_timeout;
extern time_t slave_timeout;

dsa_sys_init(acptr,avptr)
int *acptr;
char *** avptr;
{
char *name;
char **ptr;
int cnt;
extern int parse_line;
extern char dsa_mode;

	parse_line = 0;		/* stop 'line 1:' being printed in tailor file errors */
	dsa_mode = 1;

	name = **avptr;

	DLOG (log_dsap,LLOG_TRACE,("Initialisation"));

	cnt = *acptr;
	ptr = *avptr;
	dsa_tai_args (acptr,avptr);

	if (dsa_tai_init(name) != OK)
		fatal (-43,"Tailoring failed");

	dsa_tai_args (&cnt,&ptr);    /* second call IS needed !!! */

	DLOG (log_dsap,LLOG_TRACE,("Loading oid table (%s)",dsaoidtable));

	load_oid_table (dsaoidtable);

	if (retry_timeout == (time_t)0)
		retry_timeout = cache_timeout;

	if (slave_timeout == (time_t)0)
		slave_timeout = cache_timeout;

	DLOG (log_dsap,LLOG_TRACE,("*** Starting ***"));

}
