/*	defs.h	4.8	83/11/29	*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <netinet/in.h>

#define	MAILCMD		"/usr/lib/sendmail -oi -t"

	/* defines for yacc */
#define EQUAL	1
#define LP	2
#define RP	3
#define SM	4
#define ARROW	5
#define DCOLON	6
#define NAME	7
#define STRING	8
#define INSTALL	9
#define NOTIFY	10
#define EXCEPT	11
#define SPECIAL	12
#define OPTION	13
#define VAR	14

	/* lexical definitions */
#define	QUOTE 	0200		/* used internally for quoted characters */
#define	TRIM	0177		/* Mask to strip quote bit */

	/* table sizes */
#define HASHSIZE	1021
#define INMAX	3500
#define NCARGS	10240
#define GAVSIZ	NCARGS / 6
#define NSTAMPS	15

	/* option flags */
#define VERIFY	0x1
#define WHOLE	0x2
#define YOUNGER	0x4
#define COMPARE	0x8
#define REMOVE	0x10

	/* expand type definitions */
#define E_VARS	0x1
#define E_SHELL	0x2
#define E_TILDE	0x4
#define E_ALL	0x7

#define ISDIR(m) (((m) & S_IFMT) == S_IFDIR)

#define ALLOC(x) (struct x *) malloc(sizeof(struct x))

struct block {
	short	b_type;
	short	b_options;
	char	*b_name;
	struct	block *b_next;
	struct	block *b_args;
};

extern int debug;		/* debugging flag */
extern int nflag;		/* NOP flag, don't execute commands */
extern int qflag;		/* Quiet. don't print messages */
extern int options;		/* global options */

extern int errs;		/* number of errors seen */
extern int rem;			/* remote file descriptor */
extern int iamremote;		/* acting as remote server */
extern int filec;		/* number of files to update */
extern char **filev;		/* list of files/directories to update */
extern char tmpfile[];		/* file name for logging changes */
extern struct passwd *pw;	/* pointer to static area used by getpwent */
extern struct group *gr;	/* pointer to static area used by getgrent */
extern char host[];		/* host name of master copy */
extern char *rhost;		/* host name of remote being updated */
extern struct block *except;	/* list of files to exclude */
extern char buf[];		/* general purpose buffer */
extern int errno;		/* system error number */
extern char *sys_errlist[];

struct block *lookup();
struct block *makeblock();
struct block *expand();
char *exptilde();
char *malloc();
char *rindex();
char *index();
