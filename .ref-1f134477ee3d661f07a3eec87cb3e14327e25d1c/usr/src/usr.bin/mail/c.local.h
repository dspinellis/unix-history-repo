/*
 * Declarations and constants specific to an installation.
 * UCB 11/70 Version 7.0
 */
 
/*
 * Sccs Id = "@(#)c.local.h	2.1 %G%";
 */

#define APPEND				/* New mail to end of mail box */
#define	LOCAL		'y'		/* Local machine id */
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define DELIVERMAIL	"/etc/delivermail"
					/* Name of snazzy mail deliverer */
#define	VISUAL		"/usr/ucb/bin/vi"	/* Name of display editor */
#define	EDITOR		"/usr/ucb/bin/ex"	/* Name of text editor */
#define	SHELL		"/bin/csh"	/* Default shell interpreter */
#define	HELPFILE	"/usr/ucb/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/ucb/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0177777		/* Significant uid bits */
#define	MASTER		"/usr/ucb/lib/Mail.rc"
#define	UTIME				/* System implement utime(2) */
