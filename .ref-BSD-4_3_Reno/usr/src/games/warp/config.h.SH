case $CONFIG in
'')
    if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
	(echo "Can't find config.sh."; exit 1)
	echo "Using config.sh from above..."
    fi
    . config.sh
    ;;
esac
echo "Extracting config.h (with variable substitutions)"
cat <<!GROK!THIS! >config.h
/* config.h
 * This file was produced by running the config.h.SH script, which
 * gets its values from config.sh, which is generally produced by
 * running Configure.
 *
 * Feel free to modify any of this as the need arises.  Note, however,
 * that running config.h.SH again will wipe out any changes you've made.
 * For a more permanent change edit config.sh and rerun config.h.SH.
 */


/* EUNICE:
 *	This symbol, if defined, indicates that the program is being compiled
 *	under the EUNICE package under VMS.  The program will need to handle
 *	things like files that don't go away the first time you unlink them,
 *	due to version numbering.  It will also need to compensate for lack
 *	of a respectable link() command.
 */
/* VMX:
 *	This symbol, if defined, indicates that the program is running under
 *	VMS.  It is currently only set in conjunction with the EUNICE symbol.
 */
#$d_eunice	EUNICE		/**/
#$d_eunice	VMS		/**/

/* CHARSPRINTF:
 *	This symbol is defined if this system declares "char *sprintf()" in
 *	stdio.h.  The trend seems to be to declare it as "int sprintf()".  It
 *	is up to the package author to declare sprintf correctly based on the
 *	symbol.
 */
#$d_charsprf	CHARSPRINTF 	/**/

/* FCNTL:
 *	This symbol, if defined, indicates to the C program that it should
 *	include fcntl.h.
 */
#$d_fcntl	FCNTL		/**/

/* FTIMER:
 *	This symbol, if defined, indicates that the ftime() routine exists.
 */
#$d_ftime	FTIMER		/**/

/* GETHOSTNAME:
 *	This symbol, if defined, indicates that the C program may use the
 *	gethostname() routine to derive the host name.  See also DOUNAME
 *	and PHOSTNAME.
 */
/* DOUNAME:
 *	This symbol, if defined, indicates that the C program may use the
 *	uname() routine to derive the host name.  See also GETHOSTNAME and
 *	PHOSTNAME.
 */
/* PHOSTNAME:
 *	This symbol, if defined, indicates that the C program may use the
 *	contents of PHOSTNAME as a command to feed to the popen() routine
 *	to derive the host name.  See also GETHOSTNAME and DOUNAME.
 */
#$d_gethname	GETHOSTNAME	/**/
#$d_douname	DOUNAME		/**/
#$d_phostname	PHOSTNAME "$phostname"	/**/

/* GETPWENT:
 *	This symbol, if defined, indicates that the getpwent() routine
 *	should be used instead of the getpw() routine.
 */
#$d_getpwent	GETPWENT	/**/

/* HAVETERMLIB:
 *	This symbol, when defined, indicates that termlib-style routines
 *	are available.  There is nothing to include.
 */
#$d_havetlib	HAVETERMLIB	/**/

/* index:
 *	This preprocessor symbol is defined, along with rindex, if the system
 *	uses the strchr and strrchr routines instead.
 */
/* rindex:
 *	This preprocessor symbol is defined, along with index, if the system
 *	uses the strchr and strrchr routines instead.
 */
#$d_index	index strchr	/* cultural */
#$d_index	rindex strrchr	/*  differences? */

/* IOCTL:
 *	This symbol, if defined, indicates that sys/ioctl.h exists and should
 *	be included.
 */
#$d_ioctl	IOCTL		/**/

/* NORMSIG:
 *	This symbol, if defined, indicates that normal signal handling routines
 *	should be used, as opposed to the ones in 4.1bsd (sigset, etc.).
 */
#$d_normsig	NORMSIG		/**/

/* PORTABLE:
 *	This symbol, if defined, indicates to the C program that it should
 *	not assume that it is running on the machine it was compiled on.
 *	The program should be prepared to look up the host name, translate
 *	generic filenames, use PATH, etc.
 */
#$d_portable	PORTABLE	/**/

/* RDCHK:
 *	This symbol, if defined, indicates that the rdchk routine is available
 *	to find out if there is input pending on an IO channel.  Generally
 *	the routine is used only if FIONREAD and O_NDELAY aren't available.
 */
#$d_rdchk	RDCHK		/**/

/* SCOREFULL:
 *	This symbol, if defined, indicates that any scoreboard kept by the
 *	program should be kept on the basis of the user's full name as opposed
 *	to the user's login name.  If the user can change his full name he
 *	can enter multiple scores if this is defined.
 */
#$d_scorfl	SCOREFULL	/**/

/* SIGNEDCHAR:
 *	This symbol, if defined, indicates that characters are a signed type.
 *	If not defined, things declared as signed characters (and that make
 *	use of negative values) should probably be declared as shorts instead.
 */
#$d_sgndchr	SIGNEDCHAR	/**/

/* TERMIO:
 *	This symbol, if defined, indicates that the program should include
 *	termio.h rather than sgtty.h.  There are also differences in the
 *	ioctl() calls that depend on the value of this symbol.
 */
#$d_termio	TERMIO		/**/

/* USENDIR:
 *	This symbol, if defined, indicates that the program should compile
 *	the ndir.c code provided with the package.
 */
/* LIBNDIR:
 *	This symbol, if defined, indicates that the program should include the
 *	system's version of ndir.h, rather than the one with this package.
 */
#$d_usendir	USENDIR		/**/
#$d_libndir	LIBNDIR		/**/

/* WHOAMI:
 *	This symbol, if defined, indicates that the program may include
 *	whoami.h.
 */
#$d_whoami	WHOAMI		/**/

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
#define HOSTNAME "$hostname"		/**/
#define MYDOMAIN "$mydomain"		/**/

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
#$d_passnames	PASSNAMES /*  (undef to take name from ~/.fullname) */
#$d_berknames	BERKNAMES /* (that is, ":name,stuff:") */
#$d_usgnames	USGNAMES  /* (that is, ":stuff-name(stuff):") */

/* PREFSHELL:
 *	This symbol contains the full name of the preferred user shell on this
 *	system.  Usual values are /bin/csh, /bin/ksh, /bin/sh.
 */
#define PREFSHELL "$prefshell"		/**/

/* RANDBITS:
 *	This symbol contains the number of bits of random number the rand()
 *	function produces.  Usual values are 15, 16, and 31.
 */
#define RANDBITS $randbits		/**/

/* Reg1:
 *	This symbol, along with Reg2, Reg3, etc. is either the word "register"
 *	or null, depending on whether the C compiler pays attention to this
 *	many register declarations.  The intent is that you don't have to
 *	order your register declarations in the order of importance, so you
 *	can freely declare register variables in sub-blocks of code and as
 *	function parameters.  Do not use Reg<n> more than once per routine.
 */

#define Reg1 $reg1		/**/
#define Reg2 $reg2		/**/
#define Reg3 $reg3		/**/
#define Reg4 $reg4		/**/
#define Reg5 $reg5		/**/
#define Reg6 $reg6		/**/
#define Reg7 $reg7		/**/
#define Reg8 $reg8		/**/
#define Reg9 $reg9		/**/
#define Reg10 $reg10		/**/
#define Reg11 $reg11		/**/
#define Reg12 $reg12		/**/
#define Reg13 $reg13		/**/
#define Reg14 $reg14		/**/
#define Reg15 $reg15		/**/
#define Reg16 $reg16		/**/

/* ROOTID:
 *	This symbol contains the uid of root, normally 0.
 */
#define ROOTID $rootid		/**/

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
#define VOIDUSED $defvoidused
#endif
#define VOIDFLAGS $voidflags
#if (VOIDFLAGS & VOIDUSED) != VOIDUSED
#$define void int		/* is void to be avoided? */
#$define M_VOID		/* Xenix strikes again */
#endif

/* $package private library, may use ~ expansion, %x and %l */
#define PRIVLIB "$privlib"		/**/

!GROK!THIS!
