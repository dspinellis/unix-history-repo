#include <sys/vm.h>
#define	PAGSIZ		(NBPG*CLSIZE)
/*
 *	UNIX/vax debugger
 */

/* unix parameters */
#define DBNAME "adb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"
#define	TXTRNDSIZ	PAGSIZ

TYPE	long TXTHDR[8];
TYPE	long SYMV;

#ifndef vax
struct {short hiword; short loword;}; /* stupid fp-11 */
#endif

/* symbol table in a.out file */
struct symtab {
	char	symc[8];
#ifndef EDDT
	char	symf;
	char	sympad[3];
#endif
	SYMV	symv;
};
#define SYMTABSIZ (sizeof (struct symtab))

#define SYMCHK 057
#define SYMTYPE(symflg) (symflg&41 ? DSYM : NSYM)
