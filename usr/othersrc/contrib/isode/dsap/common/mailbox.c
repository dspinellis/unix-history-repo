/* mailbox.c - otherMailbox attribute */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/mailbox.c,v 7.1 91/02/22 09:19:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/mailbox.c,v 7.1 91/02/22 09:19:38 mrose Interim $
 *
 *
 * $Log:	mailbox.c,v $
 * Revision 7.1  91/02/22  09:19:38  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:42:29  mrose
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


/*
	SYNTAX
	mailbox ::= <printablestring> '$' <IA5String>
	
	EXAMPLE
	internet $ quipu-support@cs.ucl.ac.uk
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/syntaxes.h"

static mailbox_free (ptr)
struct mailbox * ptr;
{
	free (ptr->mbox);
	free (ptr->mtype);

	free ((char *) ptr);
}


static struct mailbox * mailbox_cpy (a)
struct mailbox * a;
{
struct mailbox * result;

	result = (struct mailbox *) smalloc (sizeof (struct mailbox));
	result->mbox = strdup (a->mbox);
	result->mtype = strdup (a->mtype);
	return (result);
}

static mailbox_cmp (a,b)
struct mailbox * a;
struct mailbox * b;
{
int res;

	if (a == (struct mailbox *) NULL)
		if (b == (struct mailbox *) NULL)
			return (0);
		else 
			return (-1);

	if ( (res = lexequ(a->mbox,b->mbox)) != 0) 
		return (res);
	if ( (res = lexequ(a->mtype,b->mtype)) != 0)
		return (res);
	return (0);
}


static mailbox_print (ps,mail,format)
register PS ps;
struct   mailbox* mail;
int format;
{
	if (format == READOUT)
		ps_printf (ps,"%s: %s",mail->mtype, mail->mbox);
	else
		ps_printf (ps,"%s $ %s",mail->mtype, mail->mbox);
}


static struct mailbox* str2mailbox (str)
char * str;
{
struct mailbox * result;
char * ptr;
char * mark = NULLCP;

	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("seperator missing in mailbox '%s'",str);
		return ((struct mailbox *) NULL);
	}

	result = (struct mailbox *) smalloc (sizeof (struct mailbox));
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	}
	ptr++;
	result->mtype = strdup (str);
	*ptr++ = '$';
	result->mbox = strdup (SkipSpace(ptr));	

	if (mark != NULLCP)
		*mark = ' ';

	return (result);
}

static PE mail_enc (m)
struct mailbox * m;
{
PE ret_pe;

        (void) encode_Thorn_MailBox (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct mailbox * mail_dec (pe)
PE pe;
{
struct mailbox * m;

	if (decode_Thorn_MailBox (pe,1,NULLIP,NULLVP,&m) == NOTOK) {
		return ((struct mailbox *) NULL);
	}
	return (m);
}

mailbox_syntax ()
{
	(void) add_attribute_syntax ("Mailbox",
		(IFP) mail_enc,		(IFP) mail_dec,
		(IFP) str2mailbox,	mailbox_print,
		(IFP) mailbox_cpy,	mailbox_cmp,
		mailbox_free,		NULLCP,
		NULLIFP,		TRUE);
}
