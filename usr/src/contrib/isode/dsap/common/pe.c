/* pe.c - General PE utility routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/pe.c,v 7.2 91/02/22 09:19:56 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/pe.c,v 7.2 91/02/22 09:19:56 mrose Interim $
 *
 *
 * $Log:	pe.c,v $
 * Revision 7.2  91/02/22  09:19:56  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:42:32  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:20  mrose
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
#include "psap.h"
#include "quipu/attr.h"               /* for defn of READOUT */

extern LLog * log_dsap;

pe_print (ps, pe, format)
PS     ps;
PE     pe;
int     format;
{
register char * ptr, *s;
register int i, j;
PS sps;
static char hex[] = "0123456789abcdef";
char buffer [LINESIZE];

	if ( format == FILEOUT) {
		(void) pe2ps (ps,pe);
		return;
	}

	if ((format == READOUT) && (pe->pe_len >= LINESIZE)) {
		ps_print (ps,"ASN attribute too big to print here!");
		return;
	}

	if ((sps = ps_alloc (str_open)) == NULLPS)
		return;
	if (str_setup (sps,NULLCP,LINESIZE,0) == NOTOK)
		return;

	if (format != READOUT)
		(void) ps_write (ps, (PElementData)"{ASN}", 5);

	(void) pe2ps (sps,pe);

	s = buffer;

	ptr = sps->ps_base;
	for (i=0, j=0; i<sps->ps_byteno; i++) {
/*
		ps_printf (sps2,fmt,*ptr++ & 255);
*/
		*s++ = hex [((*ptr & 255)/16) % 16];
		*s++ = hex [(*ptr++ & 255) % 16];
		j += 2;
		if ( j >= LINESIZE ) {
			(void) ps_write (ps, (PElementData)buffer, j);
			s = buffer;
			j = 0;
		}
	}
	(void) ps_write (ps, (PElementData)buffer, j);
	(void) ps_write (ps, (PElementData)"00", 2);
	ps_free (sps);

}

PE asn2pe (str)
char * str;
{
char * ptr;
char * pe_ptr;
register int i,j;
PS sps;
void StripSpace ();
int val;
PE pe;

	StripSpace (str);

	j = strlen (str);
	pe_ptr = (char *) smalloc (j+10);
	ptr = pe_ptr;

	for ( i=0 ; i<j; i++) {
	    (void) sscanf (str,"%2x",&val);
	    *ptr++ = val & 0xff;
	    str++; str++;
	}


	if ((sps = ps_alloc (str_open)) == NULLPS)
		return(NULLPE);
	if (str_setup (sps,pe_ptr,j,1) == NOTOK)
		return(NULLPE);

	pe = ps2pe (sps);
	if (sps->ps_errno != PS_ERR_NONE) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("%s in ASN attribute ",ps_error(sps->ps_errno)));
		if (pe) {
			pe_free (pe);
			pe = NULLPE;
		}
	}

	ps_free (sps);

	return (pe);
}
