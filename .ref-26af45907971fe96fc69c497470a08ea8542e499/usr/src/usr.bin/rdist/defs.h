/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)defs.h	5.10 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "pathnames.h"

/*
 * The version number should be changed whenever the protocol changes.
 */
#define VERSION	 3

	/* defines for yacc */
#define EQUAL	1
#define LP	2
#define RP	3
#define SM	4
#define ARROW	5
#define COLON	6
#define DCOLON	7
#define NAME	8
#define STRING	9
#define INSTALL	10
#define NOTIFY	11
#define EXCEPT	12
#define PATTERN	13
#define SPECIAL	14
#define OPTION	15

	/* lexical definitions */
#define	QUOTE 	0200		/* used internally for quoted characters */
#define	TRIM	0177		/* Mask to strip quote bit */

	/* table sizes */
#define HASHSIZE	1021
#define INMAX	3500

	/* option flags */
#define VERIFY	0x1
#define WHOLE	0x2
#define YOUNGER	0x4
#define COMPARE	0x8
#define REMOVE	0x10
#define FOLLOW	0x20
#define IGNLNKS	0x40

	/* expand type definitions */
#define E_VARS	0x1
#define E_SHELL	0x2
#define E_TILDE	0x4
#define E_ALL	0x7

	/* actions for lookup() */
#define LOOKUP	0
#define INSERT	1
#define REPLACE	2

#define ISDIR(m) (((m) & S_IFMT) == S_IFDIR)

#define ALLOC(x) (struct x *) malloc(sizeof(struct x))

struct namelist {	/* for making lists of strings */
	char	*n_name;
	struct	namelist *n_next;
};

struct subcmd {
	short	sc_type;	/* type - INSTALL,NOTIFY,EXCEPT,SPECIAL */
	short	sc_options;
	char	*sc_name;
	struct	namelist *sc_args;
	struct	subcmd *sc_next;
};

struct cmd {
	int	c_type;		/* type - ARROW,DCOLON */
	char	*c_name;	/* hostname or time stamp file name */
	char	*c_label;	/* label for partial update */
	struct	namelist *c_files;
	struct	subcmd *c_cmds;
	struct	cmd *c_next;
};

struct linkbuf {
	ino_t	inum;
	dev_t	devnum;
	int	count;
	char	pathname[BUFSIZ];
	char	target[BUFSIZ];
	struct	linkbuf *nextp;
};

extern int debug;		/* debugging flag */
extern int nflag;		/* NOP flag, don't execute commands */
extern int qflag;		/* Quiet. don't print messages */
extern int options;		/* global options */

extern int nerrs;		/* number of errors seen */
extern int rem;			/* remote file descriptor */
extern int iamremote;		/* acting as remote server */
extern char tempfile[];		/* file name for logging changes */
extern struct linkbuf *ihead;	/* list of files with more than one link */
extern struct passwd *pw;	/* pointer to static area used by getpwent */
extern struct group *gr;	/* pointer to static area used by getgrent */
extern char host[];		/* host name of master copy */
extern char buf[];		/* general purpose buffer */

int	 any __P((int, char *));
char	*colon __P((char *));
void	 cleanup __P((int));
void	 define __P((char *));
void	 docmds __P((char **, int, char **));
void	 error __P((const char *, ...));
int	 except __P((char *));
struct namelist *
	 expand __P((struct namelist *, int));
char	*exptilde __P((char [], char *));
void	 fatal __P((const char *, ...));
int	 inlist __P((struct namelist *, char *));
void	 insert __P((char *,
	    struct namelist *, struct namelist *, struct subcmd *));
void	 install __P((char *, char *, int, int));
void	 log __P((FILE *, const char *, ...));
struct namelist *
	 lookup __P((char *, int, struct namelist *));
void	 lostconn __P((int));
struct namelist *
	 makenl __P((char *));
struct subcmd *
	 makesubcmd __P((int));
void	 prnames __P((struct namelist *));
void	 server __P((void));
void	 yyerror __P((char *));
int	 yyparse __P((void));
