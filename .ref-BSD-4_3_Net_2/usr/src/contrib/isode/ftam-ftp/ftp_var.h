/* ftp_var.h - FTP global variables */

/* 
 * $Header: /f/osi/ftam-ftp/RCS/ftp_var.h,v 7.1 91/02/22 09:23:32 mrose Interim $
 *
 *
 * $Log:	ftp_var.h,v $
 * Revision 7.1  91/02/22  09:23:32  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:55:07  mrose
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
 *	Shamelessly taken from UCB
 */

/*
 * Options and other state info.
 */
int	trace;			/* trace packets exchanged */
int	hash;			/* print # for each buffer transferred */
int	verbose;		/* print messages to/from server */
int	connected;		/* connected to server */
int	fromatty;		/* input is from a terminal */
int	interactive;		/* interactively prompt on m* cmds */
int	debug;			/* debugging level */
int	bell;			/* ring bell on cmd completion */
int	doglob;			/* glob local file names */
int	autologin;		/* establish user account on connection */

char	typename[32];		/* name of file transfer type */
int	type;			/* file transfer type */
char	structname[32];		/* name of file transfer structure */
int	stru;			/* file transfer structure */
char	formname[32];		/* name of file transfer format */
int	form;			/* file transfer format */
char	modename[32];		/* name of file transfer mode */
int	mode;			/* file transfer mode */
char	bytename[32];		/* local byte size in ascii */
int	bytesize;		/* local byte size in binary */

char	*hostname;		/* name of host connected to */

struct	servent *sp;		/* service spec for tcp/ftp */

#include <setjmp.h>
jmp_buf	toplevel;		/* non-local goto stuff for cmd scanner */

char	line[200];		/* input line buffer */
char	*stringbase;		/* current scan point in line buffer */
char	argbuf[200];		/* argument storage buffer */
char	*argbase;		/* current storage point in arg buffer */
int	margc;			/* count of arguments on input line */
char	*margv[20];		/* args parsed from input line */

int	options;		/* used during socket creation */

/*
 * Format of command table.
 */
struct cmd {
	char	*c_name;	/* name of command */
	char	*c_help;	/* help string */
	char	c_bell;		/* give bell when command completes */
	char	c_conn;		/* must be connected to use command */
	int	(*c_handler)();	/* function to call */
};

extern	char *tail();
extern	char *index();
extern	char *rindex();
extern	char *remglob();
extern	int errno;
extern	int sys_nerr;
extern	char *sys_errlist[];

/* global interface variables */
int ftp_directory;/* TRUE if last ftp_exist was a multiple listing */
char ftp_error_buffer[BUFSIZ];
char *ftp_error; /* points to FTP diagnostic string */
