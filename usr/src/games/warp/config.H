/* config.h
 * This file was produced by running the Configure script.
 *
 * Feel free to modify any of this as the need arises.  Note, however,
 * that running Configure again will wipe out any changes you've made.
 */


#/*undef	EUNICE		/* no file linking? */
#/*undef	VMS		/* other assorted ickies? */

/* how to cancel an article */
#define CANCEL "/usr/lib/news/inews -h <%h"	/**/

#define	FCNTL		/* should we include fcntl.h? */

#define	FTIMER		/* do we have the ftime() routine? */

#define	GETHOSTNAME	/* do we have a gethostname function? */
#/*undef	DOUNAME		/* do we have a uname function? */
#/*undef	PHOSTNAME "hostname"	/* how to get host name with popen */

#/*undef	GETPWENT	/* need we include slow getpwent? */

#define	HAVETERMLIB	/* do we have termlib-style routines? */

#/*undef	index strchr	/* cultural */
#/*undef	rindex strrchr	/*  differences? */

#define	IOCTL		/* are ioctl args all defined in one place? */

#define	NORMSIG		/* use signal rather than sigset? */

#/*undef	PORTABLE	/* do we do extra lookups to start up? */

#/*undef	SCOREFULL	/* keep scoreboard by fullname? */

#/*undef	TERMIO		/* is this a termio system? */

#/*undef	USENDIR		/* include ndir.c? */
#/*undef	LIBNDIR		/* include /usr/include/ndir.h? */

#/*undef	vfork fork	/* is vfork too virtual? */

#/*undef	void int	/* is void to be avoided? */

#define	WHOAMI		/* should we include whoami.h? */

#define	PASSNAMES /* do names come from the passwd file? */
				/*  (undef to take name from ~/.fullname) */
#define	BERKNAMES /* if so, are they Berkeley format? */
				/* (that is, ":name,stuff:") */
#/*undef	USGNAMES /* or are they USG format? */
				/* (that is, ":stuff-name(stuff):") */

/* news library, may use only ~ and %l expansion */
#define NEWSLIB "/usr/lib/news"		/**/

/* default shell--ok to be a slow shell like csh */
#define PREFSHELL "/bin/csh"		/**/

/* warp private library, may use ~ expansion, %x and %l */
#define PRIVLIB "/a/lwall/src/warp"		/**/

/* bits produced by the rand() function */
#define RANDBITS 31		/**/

/* How many register declarations are paid attention to? */

#define Reg1 register		/**/
#define Reg2 register		/**/
#define Reg3 register		/**/
#define Reg4 register		/**/
#define Reg5 register		/**/
#define Reg6 register		/**/
#define Reg7 		/**/
#define Reg8 		/**/
#define Reg9 		/**/
#define Reg10 		/**/
#define Reg11 		/**/
#define Reg12 		/**/
#define Reg13 		/**/
#define Reg14 		/**/
#define Reg15 		/**/
#define Reg16 		/**/

/* root uid */
#define ROOTID 0		/**/

/* name of the site.  May be overridden by gethostname, uname, etc. */
#define SITENAME "sdcrdcf"		/**/

