/* quipu_sntx.c - invoke quipu syntax handlers */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/quipu_sntx.c,v 7.2 91/02/22 09:20:03 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/quipu_sntx.c,v 7.2 91/02/22 09:20:03 mrose Interim $
 *
 *
 * $Log:	quipu_sntx.c,v $
 * Revision 7.2  91/02/22  09:20:03  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:42:37  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:25  mrose
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

quipu_syntaxes ()
{
	standard_syntaxes ();
	acl_syntax ();
	edbinfo_syntax ();
	schema_syntax ();
	photo_syntax ();
	protected_password_syntax();
	inherit_syntax ();
	audio_syntax ();
	ap_syntax ();

	/* Thorn syntaxes */
	mailbox_syntax ();
	documentStore_syntax ();
}
