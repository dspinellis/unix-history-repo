/*	defs.h	4.1	83/09/07	*/

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

#define	MAILCMD		"/usr/lib/sendmail -i -t"

	/* defines for yacc */
#define EQUAL 1
#define ARROW 2
#define LP 3
#define RP 4
#define NAME 5
#define INSTALL 6
#define VERIFY 7
#define NOTIFY 8
#define EXCEPT 9

	/* table sizes */
#define HASHSIZE 1021
#define INMAX 3500

#define ALLOC(x) (struct x *) malloc(sizeof(struct x))

struct block {
	int	b_type;
	char	*b_name;
	struct	block *b_next;
	struct	block *b_args;
};

extern int debug;		/* debugging flag */
extern int nflag;		/* NOP flag, don't execute commands */
extern int qflag;		/* Quiet. don't print messages */
extern int vflag;		/* verify only */
extern int yflag;		/* update iff remote younger than master */

extern int errs;		/* number of errors seen */
extern int rem;			/* remote file descriptor */
extern int iamremote;		/* acting as remote server */
extern int filec;		/* number of files to update */
extern char **filev;		/* list of files/directories to update */
extern char *tmpfile;		/* file name for logging changes */
extern char host[];		/* host name of master copy */
extern char *rhost;		/* host name of remote being updated */
extern struct block *except;	/* list of files to exclude */
extern char buf[];		/* general purpose buffer */
extern int errno;		/* system error number */
extern char *sys_errlist[];

struct block *lookup();
struct block *expand();
char *rindex();
