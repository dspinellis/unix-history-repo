/* "@(#)machine.h 4.2 8/17/82" */
/*
 *	UNIX/vax debugger
 */

/* unix parameters */
#define DBNAME "adb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"
#define TXTRNDSIZ PAGSIZ

TYPE	long TXTHDR[8];
TYPE	long SYMV;

#ifndef vax
struct {short hiword; short loword;}; /* stupid fp-11 */
#endif

/* symbol table in a.out file */
struct symtab {
	char	symc[8];
	char	symf;
	char	sympad[3];
	SYMV	symv;
};
#define SYMTABSIZ (sizeof (struct symtab))

#define SYMCHK 057
#define SYMTYPE(symflg) (symflg&41 ? DSYM : NSYM)
