/*
 * drivedefs.h
 *
 * Definitions for Fortran 77 Compiler driver
 * For the VAX, running on the VAX
 *
 * UCSD Chemistry modification history:
 *
 * $Log:	drivedefs.h,v $
 * Revision 1.4  85/02/12  19:25:05  donn
 * Added 'CATNAME' to define the name of the concatenation command.
 * 
 * Revision 1.3  85/01/14  06:42:01  donn
 * Changed to use c2 as the peephole optimizer.
 * 
 * Revision 1.2  84/04/11  19:02:16  donn
 * Added Dave Wasley's fix to load the Unix library (libU77.a) first.
 * 
 */

#if HERE!=VAX || TARGET!=VAX || FAMILY!=PCC
	Wrong Definitions File!
#endif

#define PASS1NAME	"/usr/libexec/f77pass1"
#define PASS2NAME	"/usr/libexec/f1"
#define PASS2OPT	"/usr/libexec/c2"
#define ASMNAME		"/usr/bin/as"
#define LDNAME		"/usr/bin/ld"
#define	CATNAME		"/bin/cat"
#define FOOTNAME	"/usr/lib/crt0.o"
#define PROFFOOT	"/usr/lib/mcrt0.o"
#define	GPRFFOOT	"/usr/lib/gcrt0.o"
#define TEMPPREF	"fort"

static char *liblist [ ] =
		{ "-lU77", "-lF77", "-lI77", "-lm", "-lc", NULL };
static char *p_liblist [ ] =
		{ "-lU77_p", "-lF77_p", "-lI77_p", "-lm_p", "-lc_p", NULL };

#include <paths.h>

#define	_PATH_BSHELL	"/bin/sh"
#define	_PATH_CPP	"/usr/bin/cpp"
#define	_PATH_UBIN	"/usr/bin"
