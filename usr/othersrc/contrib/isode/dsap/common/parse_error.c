/* parse_error.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/parse_error.c,v 7.2 91/02/22 09:19:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/parse_error.c,v 7.2 91/02/22 09:19:53 mrose Interim $
 *
 *
 * $Log:	parse_error.c,v $
 * Revision 7.2  91/02/22  09:19:53  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:34:56  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:37  mrose
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

#include "quipu/util.h"
#include "psap.h"

int print_parse_errors = TRUE;
int parse_status = 0;
#ifdef TURBO_DISK
char *parse_entry = NULL;
#endif
PS opt = NULLPS;
int parse_line = 1;
extern LLog * log_dsap;

parse_error (a,b)
char *a, *b;
{
char buffer [LINESIZE];

	parse_status++;

	if (print_parse_errors) {
		if (opt == NULLPS) {
			opt = ps_alloc (std_open);
			if (std_setup (opt,stderr) != OK) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("cant open error (parse)..."));
				LLOG (log_dsap,LLOG_EXCEPTIONS,(a,b));
				return;
			}
		}
#ifdef TURBO_DISK
		if ( parse_entry != NULL )
			ps_printf(opt, "key (%s): ", parse_entry);
#else
		if (parse_line != 0)
			ps_printf (opt,"line %d: ",parse_line);
#endif
		ps_printf (opt,a,b);
		ps_printf (opt,"\n");
	} else {
#ifdef TURBO_DISK
		if ( parse_entry != NULL ) {
			(void) sprintf (buffer,"key (%s): ", parse_entry);
			(void) strcat (buffer,a);
			LLOG (log_dsap,LLOG_EXCEPTIONS,(buffer,b));
#else
		if (parse_line != 0) {
			(void) sprintf (buffer,"line %d: ",parse_line);
			(void) strcat (buffer,a);
			LLOG (log_dsap,LLOG_EXCEPTIONS,(buffer,b));
#endif
		} else
			LLOG (log_dsap,LLOG_EXCEPTIONS,(a,b));
	}
}
