/*
 *	This file contains the constants that must be reset on each system
 *	when notefiles are first installed.  Other constants exist in
 *	structs.h, but in general should not be modified unless needed.
 */

#define		Sysname	 "ucbmonet"		/* this sytem name */
#define		BERKELEY		/* use machine.user, LFLUSHO, router, mh */
#define         MSTDIR   "/usr/spool/notes"
#define		BIN      "/usr/new"
#define		NOTESUID 1558			/* owner of system */
#define		ANONUID	 1001			/* uid of anon notes */
/*	make ANONUID an unused uid, because it is never allowed into the */
/*	notefile programs  */


#define		ARCHTIME 14			/* default days to archive */
#define		ARCHDIR  "/usr/spool/oldnotes"

#define		NOTESRC	 ".notesrc"		/* default subscription file */
#define		AUTOSEQ  "autoseq"		/* auto-sequencer link */

/*
 *	These define defaults for various Unix functions that can
 *	be overidden by environment variables 
 */
#define		SHELL    "/bin/csh"		/* default shell */
#define		EDITOR   "/usr/ucb/vi"		/* default editor */
#define		MAILER	 "/usr/ucb/Mail"	/* mailer to use */
#define		ROUTER	 "/usr/new/lib/notes/pcomp"
#define		SUPERMAILER			/* using smart mailer */
#define		PAGER	 "/usr/ucb/more"	/* pg/more default */
#define		WRITE	 "/bin/write"		/* user-user communication */

/*
 *	Definitions dependent on the UNIX OS you are using
 */
/*#define	V6				/* version 6 kernel */
/*#define	UNIX4.0				/* Unix 4.0 kernel */
#define	BSD4.1c				/* Berkeley 4.1cBSD kernel */
#define		VFORK				/* vfork call available */
#define		UIDMASK	0177777			/* mask out high UID bits */
#define		GIDMASK 0177777			/* mask out high GID bits */

/*
 *	Some other definitions.
 */
#define	PROMPT	"? "			/* command prompt */
#define		AUTOCREATE			/* auto creation newsgroups */
#define		NOSUCHWARN	"nfmaint"	/* "can't find" net nfs */
#define		OLDGROUP        30    		 /* 30 day group expiration */
#define		NOTESUMASK	022
#define		DUMPCORE	/* trapped internal error dumps core */

/* #define		NEWS */
/* #define		DEMANDNEWS			/* news output on demand */
#define		BNEWS				/* if running Bnews */

#ifdef		BNEWS				/* for B news systems */
#define		TONEWS	"inews -t %s -n %s -f %s"	/* B news insertion */
#else
#define		TONEWS	"news -i %s -n %s"		/* A news insertion */
#endif
