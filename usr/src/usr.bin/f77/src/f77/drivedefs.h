/*
  Definitions for Fortran 77 Compiler driver
  For the VAX, Running on the VAX, 
*/

#if HERE!=VAX || TARGET!=VAX || FAMILY!=PCC
	Wrong Definitions File!
#endif

#define PASS1NAME	"/usr/lib/f77pass1"
#define PASS2NAME	"/lib/f1"
#define PASS2OPT	"/lib/f2"
#define ASMNAME		"/bin/as"
#define LDNAME		"/bin/ld"
#define FOOTNAME	"/lib/crt0.o"
#define PROFFOOT	"/lib/mcrt0.o"
#define	GPRFFOOT	"/usr/lib/gcrt0.o"
#define TEMPPREF	"fort"

static char *liblist [ ] =
		{ "-lF77", "-lI77", "-lU77", "-lm", "-lc", NULL };
static char *p_liblist [ ] =
		{ "-lF77_p", "-lI77_p", "-lU77_p", "-lm_p", "-lc_p", NULL };
