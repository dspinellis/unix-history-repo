#
/*
 *	UNIX/INTERDATA debugger
 */

/* unix parameters */
#define DBNAME "adb\n"
#define LPRMODE "%Q"
#define OFFMODE "+%o"
#define TXTRNDSIZ 8192L

TYPE	unsigned TXTHDR[8];
TYPE	unsigned SYMV;

/* symbol table in a.out file */
struct symtab {
	char	symc[8];
	int	symf;
	SYMV	symv;
};
#define SYMTABSIZ (sizeof (struct symtab))

#define SYMCHK 047
#define SYMTYPE(symflg) (( symflg>=041 || (symflg>=02 && symflg<=04))\
				?  ((symflg&07)>=3 ? DSYM : (symflg&07))\
				: NSYM\
			)
