/* $Header: ftp_var.h,v 2.0 85/11/21 07:22:46 jqj Exp $ */
/* $Log:	ftp_var.h,v $
 * Revision 2.0  85/11/21  07:22:46  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/05/27  06:30:59  jqj
 * Initial revision
 * 
 * Revision 1.1  85/05/27  06:30:59  jqj
 * Initial revision
 * 
 * based on Berkeley tcp/ftp
 */
/*	ftp_var.h	4.6	83/07/26	*/
/*
 * FTP global variables.
 */
#include "Filing4.h"

/*
 * Options and other state info.
 */
int	trace;			/* trace packets exchanged */
int	hash;			/* print # for each buffer transferred */
int	verbose;		/* print messages coming back from server */
int	fromatty;		/* input is from a terminal */
int	interactive;		/* interactively prompt on m* cmds */
int	debug;			/* debugging level */
int	bell;			/* ring bell on cmd completion */
int	doglob;			/* glob local file names */
int	autologin;		/* establish user account on connection */

char	typename[32];		/* name of file transfer type */
int	typevalue;		/* file transfer type */
char	structname[32];		/* name of file transfer structure */
int	stru;			/* file transfer structure */
char	formname[32];		/* name of file transfer format */
int	form;			/* file transfer format */
char	modename[32];		/* name of file transfer mode */
int	mode;			/* file transfer mode */
char	bytename[32];		/* local byte size in ascii */
int	bytesize;		/* local byte size in binary */

char	*hostname;		/* name of host connected to */



char	line[200];		/* input line buffer */
char	*stringbase;		/* current scan point in line buffer */
char	argbuf[200];		/* argument storage buffer */
char	*argbase;		/* current storage point in arg buffer */
int	margc;			/* count of arguments on input line */
char	*margv[20];		/* args parsed from input line */

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

/*
 * Connection information
 */
extern CourierConnection	*connected;	/* connected to server */

/*
 * stuff borrowed from <arpa/ftp.h> since we don't want all of that!
 */

/*
 * Type codes
 */
#define	TYPE_A		1	/* ASCII -- map CR <=> LF */
/* #define	TYPE_E		2	/* EBCDIC */
#define	TYPE_I		3	/* image */
#define	TYPE_L		4	/* local byte size */

/*
 * Form codes
 */
#define	FORM_N		1	/* non-print */
/* #define	FORM_T		2	/* telnet format effectors */
/* #define	FORM_C		3	/* carriage control (ASA) */

/*
 * Structure codes
 */
#define	STRU_F		1	/* file (no record structure) */
/* #define	STRU_R		2	/* record structure */
/* #define	STRU_P		3	/* page structure */

/*
 * Mode types
 */
#define	MODE_S		1	/* stream */
/* #define	MODE_B		2	/* block */
/* #define	MODE_C		3	/* compressed */
