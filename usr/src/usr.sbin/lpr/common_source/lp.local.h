/*	lp.local.h	1.1	81/05/09	*/
/*
 * Possibly, local parameters to the spooling system
 */

/*
 * Magic number mapping for binary files, used by lpr to avoid
 *   printing objects files.
 */

#include <a.out.h>
#include <ar.h>

#ifndef A_MAGIC1	/* must be a VM/UNIX system */
#	define A_MAGIC1	OMAGIC
#	define A_MAGIC2	NMAGIC
#	define A_MAGIC3	ZMAGIC
#	undef ARMAG
#	define ARMAG	0177545
#endif

/*
 * Defaults for line printer capabilities data base
 */
#define	DEFLP		"lp"
#define DEFLOCK		"lock"
#define	DEFSPOOL	"/usr/spool/lpd"
#define	DEFMX		1000
#define	DEFLOGF		"/dev/console"
#define DEFFF		"\f"
#define	DEFDAEMON	"/usr/lib/lpd"
#define	DEFDEVLP	"/dev/lp"
#define	DEFUID		1

/*
 * The system name is normally imported from <whoami.h>,
 *   change it here if you want someting special on the JOB line
 *   of the burst page.
 */

#ifdef SYTEK
#	define	SYSTEM_NAME	"Sytek VAX/UNIX"
#else
#include <whoami.h>

#	define	SYSTEM_NAME	sysname
#endif

/*
 * Some utilities used by lpd
 */
#define PRLOC		"/bin/pr"
#define MAIL		"/bin/mail"

/*
 * When files are created in the spooling area, they are normally
 *   readable only by their owner and the spooling group.  If you
 *   want otherwise, change this mode.
 */
#define FILMOD		0660

/*
 * We choose not to include this from <sys/param.h>
 */
#define NOFILE		20

/*
 * Printer is assumed to support LINELEN (for block chars)
 *   and background character (blank) is a space
 */
#define LINELEN		132
#define BACKGND		' '

#define HEIGHT	9		/* height of characters */
#define WIDTH	8		/* width of characters */
#define DROP	3		/* offset to drop characters with descenders */

