/* RDCHK:
 *	This symbol, if defined, indicates that the rdchk routine is available
 *	to find out if there is input pending on an IO channel.  Generally
 *	the routine is used only if FIONREAD and O_NDELAY aren't available.
 */
#/*undef	RDCHK		/**/

/* SCOREFULL:
 *	This symbol, if defined, indicates that any scoreboard kept by the
 *	program should be kept on the basis of the user's full name as opposed
 *	to the user's login name.  If the user can change his full name he
 *	can enter multiple scores if this is defined.
 */
#/*undef	SCOREFULL	/**/

/* SIGNEDCHAR:
 *	This symbol, if defined, indicates that characters are a signed type.
 *	If not defined, things declared as signed characters (and that make
 *	use of negative values) should probably be declared as shorts instead.
 */
#define	SIGNEDCHAR	/**/

/* TERMIO:
 *	This symbol, if defined, indicates that the program should include
 *	termio.h rather than sgtty.h.  There are also differences in the
 *	ioctl() calls that depend on the value of this symbol.
 */
#/*undef	TERMIO		/**/

/* USENDIR:
 *	This symbol, if defined, indicates that the program should compile
 *	the ndir.c code provided with the package.
 */
/* LIBNDIR:
 *	This symbol, if defined, indicates that the program should include the
 *	system's version of ndir.h, rather than the one with this package.
 */
#/*undef	USENDIR		/**/
#/*undef	LIBNDIR		/**/
#define LIBNDIR

/* WHOAMI:
 *	This symbol, if defined, indicates that the program may include
 *	whoami.h.
 */
#/*undef	WHOAMI		/**/

/* HOSTNAME:
 *	This symbol contains name of the host the program is going to run on.
 *	The domain is not kept with hostname, but must be gotten from MYDOMAIN.
 *	The dot comes with MYDOMAIN, and need not be supplied by the program.
 *	If gethostname() or uname() exist, HOSTNAME may be ignored.
 */
/* MYDOMAIN:
 *	This symbol contains the domain of the host the program is going to
 *	run on.  The domain must be appended to HOSTNAME to form a complete
 *	host name.  The dot comes with MYDOMAIN, and need not be supplied by
 *	the program.  If the host name is derived from PHOSTNAME, the domain
 *	may or may not already be there, and the program should check.
 */
#define HOSTNAME "kazoo"		/**/
#define MYDOMAIN ".uucp"		/**/

/* PASSNAMES:
 *	This symbol, if defined, indicates that full names are stored in
 *	the /etc/passwd file.
 */
/* BERKNAMES:
 *	This symbol, if defined, indicates that full names are stored in
 *	the /etc/passwd file in Berkeley format (name first thing, everything
 *	up to first comma, with & replaced by capitalized login id, yuck).
 */
/* USGNAMES:
 *	This symbol, if defined, indicates that full names are stored in
 *	the /etc/passwd file in USG format (everything after - and before ( is
 *	the name).
 */
#define	PASSNAMES /*  (undef to take name from ~/.fullname) */
#define	BERKNAMES /* (that is, ":name,stuff:") */
#/*undef	USGNAMES  /* (that is, ":stuff-name(stuff):") */

/* PREFSHELL:
 *	This symbol contains the full name of the preferred user shell on this
 *	system.  Usual values are /bin/csh, /bin/ksh, /bin/sh.
 */
#define PREFSHELL "/bin/csh"		/**/

/* RANDBITS:
 *	This symbol contains the number of bits of random number the rand()
 *	function produces.  Usual values are 15, 16, and 31.
 */
#define RANDBITS 15		/**/

/* Reg1:
 *	This symbol, along with Reg2, Reg3, etc. is either the word "register"
 *	or null, depending on whether the C compiler pays attention to this
 *	many register declarations.  The intent is that you don't have to
 *	order your register declarations in the order of importance, so you
 *	can freely declare register variables in sub-blocks of code and as
 *	function parameters.  Do not use Reg<n> more than once per routine.
 */

#define Reg1 register		/**/
#define Reg2 register		/**/
#define Reg3 register		/**/
#define Reg4 		/**/
#define Reg5 		/**/
#define Reg6 		/**/
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

/* ROOTID:
 *	This symbol contains the uid of root, normally 0.
 */
#define ROOTID 0		/**/

/* VOIDFLAGS:
 *	This symbol indicates how much support of the void type is given by this
 *	compiler.  What various bits mean:
 *
 *	    1 = supports declaration of void
 *	    2 = supports arrays of pointers to functions returning void
 *	    4 = supports comparisons between pointers to void functions and
 *		    addresses of void functions
 *
 *	The package designer should define VOIDUSED to indicate the requirements
 *	of the package.  This can be done either by #defining VOIDUSED before
 *	including config.h, or by defining defvoidused in Myinit.U.
 */
#ifndef VOIDUSED
#define VOIDUSED 7
#endif
#define VOIDFLAGS 7
#if (VOIDFLAGS & VOIDUSED) != VOIDUSED
#define void int		/* is void to be avoided? */
#define M_VOID		/* Xenix strikes again */
#endif

/* warp private library, may use ~ expansion, %x and %l */
#define PRIVLIB "/usr/games/lib/warp"		/**/

