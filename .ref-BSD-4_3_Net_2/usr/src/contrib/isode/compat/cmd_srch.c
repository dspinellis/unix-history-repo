/* cmd_srch.c - search a lookup table: return numeric value */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/cmd_srch.c,v 7.1 91/02/22 09:15:06 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/cmd_srch.c,v 7.1 91/02/22 09:15:06 mrose Interim $
 *
 *
 * $Log:	cmd_srch.c,v $
 * Revision 7.1  91/02/22  09:15:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:22:58  mrose
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

/* map a string onto a value */

cmd_srch(str, cmd)
register char   *str;
register CMD_TABLE *cmd;
{
	for(;cmd->cmd_key != NULLCP; cmd++)
		if(lexequ(str, cmd->cmd_key) == 0)
			return(cmd->cmd_value);
	return(cmd->cmd_value);
}
