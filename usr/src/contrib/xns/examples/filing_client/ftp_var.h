/* $Header: ftp_var.h,v 2.4 87/04/01 13:53:05 ed Exp $ */
/* $Log:	ftp_var.h,v $
 * Revision 2.4  87/04/01  13:53:05  ed
 * include FilingSubset1.h from standard include/xnscourier directory.
 * 
 * Revision 2.3  87/01/09  16:48:41  ed
 * Use FilingSubset Protocol
 * file types moved to filetypes.h to share with server code
 * 
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
#include <xnscourier/FilingSubset1.h>
#include <xnscourier/filetypes.h>

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
int 	usefiling;		/* use Filing instead of FilingSubset */

char	typename[32];		/* name of file transfer type */
LongCardinal	typevalue;		/* file transfer type */

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
 *	Server needed access to the types also, so they were moved to
 *		../filing_common/filetypes.h	ed 1/9/87
 */
