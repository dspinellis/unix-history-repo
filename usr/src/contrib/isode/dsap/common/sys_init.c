/* sys_init.c - System tailoring initialisation */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/sys_init.c,v 7.1 91/02/22 09:20:24 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/sys_init.c,v 7.1 91/02/22 09:20:24 mrose Interim $
 *
 *
 * $Log:	sys_init.c,v $
 * Revision 7.1  91/02/22  09:20:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:47:47  mrose
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

extern char *oidtable;
extern char *dsa_address;
extern char *myname;
extern LLog * log_dsap;

dsap_init(acptr,avptr)
int *acptr;
char *** avptr;
{
char * name;
char ** ptr;
int cnt;
extern int parse_line;

	parse_line = 0;		/* stop 'line 1:' being printed in tailor file errors */

	if (acptr == (int *) NULL)
		name= "unknown";
	else
		name = **avptr;

 	isodetailor (name, 1);		/* must be called before any isodefile() */
 	log_dsap -> ll_file = strdup ("./dsap.log");
 	ll_hdinit (log_dsap, name);

	DLOG (log_dsap,LLOG_TRACE,("Initialisation"));

	if (acptr != (int *) NULL) {
		cnt = *acptr;
		ptr = *avptr;
	}
	(void) tai_args (acptr,avptr);

	if (dsap_tai_init() != OK)
		fatal (-1,"Tailor failure");

	if (dsa_address == NULLCP)
		if (myname != NULLCP)
			dsa_address = myname;
		else
			fatal (-1, "dsa_address not set");

	if (acptr != (int *)NULL) 
		(void) tai_args (&cnt,&ptr);  /* second call IS needed */
	else
		(void) tai_args (acptr,avptr);  

	LLOG (log_dsap,LLOG_NOTICE,("Loading oid table (%s)",oidtable));
	load_oid_table (oidtable);

	DLOG (log_dsap,LLOG_TRACE ,("*** Starting ***"));
}
