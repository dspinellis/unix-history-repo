/* control.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/control.c,v 7.3 91/02/22 09:38:36 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/control.c,v 7.3 91/02/22 09:38:36 mrose Interim $
 *
 *
 * $Log:	control.c,v $
 * Revision 7.3  91/02/22  09:38:36  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/07/09  14:45:31  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:18:45  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:16:58  mrose
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


/* the routine dsa_control is called when the modifyentry operation
   is performed, with and 'add attribute' request and the attribute type
   is 'control'.  The value decides what to control
   This is strictly non standard, but gives the dua control of the dsa.
*/

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"
#include "tailor.h"

extern LLog * log_dsap;
#ifndef NO_STATS
extern LLog * log_stat;
#endif

dsa_control (as,error,dn)
Attr_Sequence as;
struct DSError *error;
DN dn;
{
char * str;
DN dn2;
Entry theentry;
extern Entry database_root;
SFD attempt_restart();

	if ( ! manager(dn) ) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_ERROR_REMOTE);
	}

	str = (char *) as->attr_value->avseq_av.av_struct;

#ifndef NO_STATS
	LLOG (log_stat,LLOG_NOTICE,("DSA control: %s",str));
#endif

	switch (*str) {
	case 'd':	/* -dump <directory> */
		str = SkipSpace (++str);
/* 
		directory_dump (str, database_root);
*/
		return (DS_OK);
	case 't':	/* -tailor <string> */
		str = SkipSpace (++str);
		if (dsa_tai_string (str) == OK) {
			isodexport (NULLCP);
			return (DS_OK);
		}
		break;
	case 'a':	/* -abort */
		LLOG (log_dsap,LLOG_FATAL,("*** abort signal ***"));
		stop_listeners();
		exit(0);
	case 'b':	/* -restart */
		LLOG (log_dsap,LLOG_FATAL,("*** restart signal ***"));
		attempt_restart (NOTOK);
		exit(0);		/* should not be reached */
	case 'r':	/* -refresh <entry> */
		str = SkipSpace (++str);
		if (lexequ (str,"root") == 0)
		    dn2= NULLDN;
		else
		    if ((dn2 = str2dn (str)) == NULLDN)
			break;

		if (refresh_from_disk (dn2) == OK)
			return (DS_OK);
		break;
	case 'f':	/* -resync <entry> */
		str = SkipSpace (++str);
		if (lexequ (str,"root") == 0)
		    dn2= NULLDN;
		else
		    if ((dn2 = str2dn (str)) == NULLDN)
			break;

		if ((theentry = local_find_entry (dn2,FALSE)) != NULLENTRY) 
#ifdef TURBO_DISK
#ifdef TURBO_AVL
	{
	Entry akid = (Entry) avl_getone(theentry->e_children);
			if (turbo_writeall (akid) == OK)
				return (DS_OK);
	}
#else
			if ( turbo_writeall(theentry->e_child) == OK )
				return (DS_OK);
#endif
#else
#ifdef TURBO_AVL
	{
	Entry akid = (Entry) avl_getone(theentry->e_children);
			if (journal (akid) == OK)
				return (DS_OK);
	}
#else
			if (journal (theentry->e_child) == OK)
				return (DS_OK);
#endif
#endif
		break;
	case 'l':	/* -lock <entry> */
		str = SkipSpace (++str);
		if (lexequ (str,"root") == 0)
			dn2 = NULLDN;
		else if ((dn2 = str2dn (str)) == NULLDN)
			break;

		if ((theentry = local_find_entry (dn2,FALSE)) != NULLENTRY) {
			theentry->e_lock = TRUE;
			return (DS_OK);
		}
		break;
	case 'u':	/* -unlock <entry> */
		str = SkipSpace (++str);
		if (lexequ (str,"root") == 0)
			dn2 = NULLDN;
		else if ((dn2 = str2dn (str)) == NULLDN)
			break;

		if ((theentry = local_find_entry (dn2,FALSE)) != NULLENTRY) {
			theentry->e_lock = FALSE;
			return (DS_OK);
		}
		break;
	case 's':	/* -slave */
		/*
		 * When we go async return of OK will mean that a getedb
		 * operation has been scheduled, NOT that it has succeeded.
		 */
		str = SkipSpace (++str);
		if (*str == NULL) {
		    slave_update();
		    return DS_OK;
		}

		if (lexequ (str, "shadow") == 0) {
			shadow_update();	
			return DS_OK;
		}

		if (lexequ (str, "root") == 0)
			dn2 = NULLDN;
		else if ((dn2 = str2dn (str)) == NULLDN)
			break;

		if (update_aux (dn2, dn2 == NULLDN) == OK)
			return DS_OK;
		break;
	default:
		break;
	}

	error->dse_type = DSE_SERVICEERROR;
	error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
	return (DS_ERROR_REMOTE);
}
