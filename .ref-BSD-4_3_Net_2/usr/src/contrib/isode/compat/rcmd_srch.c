/* rcmd_srch.c: search a lookup table: return string value */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/rcmd_srch.c,v 7.1 91/02/22 09:15:42 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/rcmd_srch.c,v 7.1 91/02/22 09:15:42 mrose Interim $
 *
 *
 * $Log:	rcmd_srch.c,v $
 * Revision 7.1  91/02/22  09:15:42  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:22  mrose
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

#include "manifest.h"
#include "cmd_srch.h"

/*  */

char   *rcmd_srch(val, cmd)
register int   val;
register CMD_TABLE *cmd;
{
	for(;cmd->cmd_key != NULLCP; cmd++)
		if(val == cmd->cmd_value)
			return(cmd->cmd_key);
	return(NULLCP);
}
