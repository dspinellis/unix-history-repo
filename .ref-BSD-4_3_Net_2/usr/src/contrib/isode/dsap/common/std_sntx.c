/* std_sntx.c - invoke standard syntax handlers */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/std_sntx.c,v 7.2 91/02/22 09:20:18 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/std_sntx.c,v 7.2 91/02/22 09:20:18 mrose Interim $
 *
 *
 * $Log:	std_sntx.c,v $
 * Revision 7.2  91/02/22  09:20:18  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:42:53  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:32  mrose
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

standard_syntaxes ()
{
	string_syntaxes ();
	cilist_syntax ();
	dn_syntax ();
	psap_syntax ();
	objectclass_syntax ();
	oid_syntax ();
	time_syntax ();
	boolean_syntax ();
	integer_syntax ();
	fax_syntax ();
	post_syntax ();
	telex_syntax ();
	teletex_syntax ();
	pref_deliv_syntax ();
	guide_syntax ();
	certificate_syntax ();
	certificate_pair_syntax ();
}
