/* $Header: ftp_var.h,v 2.2 86/12/15 11:41:12 jqj Exp $ */
/* $Log:	ftp_var.h,v $
 * Revision 2.2  86/12/15  11:41:12  jqj
 * Added support for more ViewPoint file types (no other attributes, though)
 * 
 * Revision 2.1  86/12/11  06:12:10  jqj
 * Eliminated form, mode, and struct commands.  Started adding support for
 * more file types.
 * 
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
 * Type codes
 */
#define	TYPE_A		2	/* ASCII -- map CR <=> LF and Type=tText */
#define	TYPE_I		0	/* image and Type=tUnspecified */
#define	TYPE_VP		4353	/* ViewPoint: image */
#define TYPE_Interpress	4361	/* VP Interpress master: image */
#define TYPE_VPCanvas	4428	/* VP Canvas: image */
#define TYPE_VPDictionary 4383	/* VP Dictionary: image */
#define TYPE_VPMailNote	4	/* VP Mail Note: ASCII */
#define TYPE_VPReference 4427	/* VP Reference Icon: image */

