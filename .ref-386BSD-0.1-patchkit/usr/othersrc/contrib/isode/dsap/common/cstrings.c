/* cstrings.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/cstrings.c,v 7.2 91/02/22 09:18:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/cstrings.c,v 7.2 91/02/22 09:18:54 mrose Interim $
 *
 *
 * $Log:	cstrings.c,v $
 * Revision 7.2  91/02/22  09:18:54  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:45:33  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:59  mrose
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


#include "psap.h"

static char arg_error [1024];
static char arg_flag [100];
int chase_flag = 2;
extern char * result_sequence;

reset_arg ()
{	
	arg_error [0] = 0;
	arg_flag [0] = 0;
	chase_flag = 2;
	if (result_sequence)
		free (result_sequence);
	result_sequence = NULLCP;
}

print_arg_error (opt)
PS opt;
{
	if (arg_error [0] != 0) {
		ps_printf (opt,"'%s' ambiguous, specify\n%s",arg_flag,arg_error);
		return (OK);
	} else
		return (NOTOK);
}


int test_arg (x, y, c)		
char           *x;		
char           *y;
int   		c;
{
int count = 0;
char * top, *topx;

	top = y;
	topx = x;

	if (*y == '-' )
		count--;

	for (; (*y != 0) || (*x != 0); y++) {	
		if (*x == 0) 
			if (count >= c)
				return (1);
			else {
				(void) strcat (arg_error, top);
				(void) strcat (arg_error, "\n");
				(void) strcpy (arg_flag,topx);
				return (0);
			}
		if (chrcnv[*x] != chrcnv[*y]) 
			return (0);
			
		count++;	
		x++;
	}

	if (count >= c)
		return (1);	
	else {
		(void) strcat (arg_error, top);
		(void) strcat (arg_error, "\n");
		(void) strcpy (arg_flag, topx);
		return (0);
	}
}

