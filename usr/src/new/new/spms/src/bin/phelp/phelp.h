/* $Header$ */

/*
 * phelp definitions
 *
 * Author: Peter J. Nicklin
 */

#define PHELP		EQUAL(*argv, "phelp")
#define PHELP_CMD_FILE	"phelp.cmd"	/* help commands */
#define PHELP_HELP_FILE	"phelp.help"	/* brief help introduction */

#define MAXHELPLEVEL	50		/* max depth of help hierarchy */
#define REQUESTSIZE	128		/* size of help request buffer */

#define MAXLINE		80		/* maximum width of index */
#define MINIMUM_GAP	2		/* minimum gap between index columns */
#define TABSIZE		8		/* number of columns per tab */
