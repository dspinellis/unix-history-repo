/*
 * Declarations and constants specific to an installation.
 * Ingres 11/70 version 6.
 */
 
/*
 * Sccs Id = "@(#)ing.local.h	2.1 %G%";
 */

#define	LOCAL		'i'		/* Local machine id */
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	DELIVERMAIL	"/etc/delivermail"
					/* Preferred mail deliverer */
#define	EDITOR		"/usr/bin/ex"	/* Name of text editor */
#define	VISUAL		"/usr/bin/vi"	/* Name of display editor */
#define	SHELL		"/bin/sh"	/* Standard shell */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0377		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	CANLOCK				/* Mailbox locking protocol works */
#define	stat	_stat			/* Simulate version 7 */
#define	fstat	_fstat			/* Simulate version 7 */

/*
 * Machine dependent type declarations.
 */

