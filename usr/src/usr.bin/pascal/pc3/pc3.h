    /* Copyright (c) 1980 Regents of the University of California */

    /*	static	char sccsid[] = "@(#)pc3.h 1.1 %G%"; */

    /*
     *	a symbol table entry.
     */
struct symbol {
    char		*name;			/* pointer to string table */
    unsigned char	type;			/* symbol type */
    int			lookup;			/* whether new or old */
    union {					/* either */
	struct {				/*   for a symbol, */
	    struct symbol	*fromp;		/*     its defining .p file */
	    struct symbol	*fromi;		/*     its defining .i file */
	    long		iline;		/*     the .i file line */
	    struct symbol	*rfilep;	/*     its resolving file */
	    long		rline;		/*     resolving file line */
	}			sym_str;
	time_t		modtime;		/*   for a file, its st_mtime */
    }			sym_un;
};

#define	NIL	0

struct fileinfo {
    FILE		*file;
    char		*name;
    time_t		modtime;
    off_t		nextoffset;
};

#define	OARMAG	0177545

    /*
     *	this is used to trim pointers into the range of a mod of a prime.
     */
#define	SHORT_ABS( n )	( n & 077777 )

    /*
     *	a prime number which rounds a struct symboltableinfo up to ~BUFSIZ
     */
#define	SYMBOLPRIME	1021
    /*
     *	number of entries used in this symbol table,
     *	a chain to the next symbol table,
     *	and the entries. (pointers to struct symbols.)
     */
struct symboltableinfo {
    long			used;
    struct symboltableinfo	*chain;
    struct symbol		*entry[ SYMBOLPRIME ];
};

    /*
     *	if new struct symbols are needed,
     *	allocate this much space and hack it up into struct symbols.
     */
#define	SYMBOLALLOC	BUFSIZ

    /*
     *	a prime number which rounds a struct stringtableinfo up to ~BUFSIZ
     */
#define	STRINGPRIME	1021

    /*
     *	number of entries used in this string table,
     *	a chain to the next string table,
     *	and the entries. (pointers to the character table.)
     */
struct stringtableinfo {
    long			used;
    struct stringtableinfo	*chain;
    char			*entry[ STRINGPRIME ];
};

    /*
     *	if more character table space is needed,
     *	allocate this much and hack it up into strings.
     */
#define	CHARALLOC	BUFSIZ

    /*
     *	an enumeration for error types
     */
#define	FATAL	0
#define	WARNING	1

    /*
     *	an enumeration for lookups
     */
#define	NEW	0
#define	OLD	1

    /*
     *	booleans
     */
#define	BOOL	int
#define	FALSE	0
#define	TRUE	1

    /*
     *	function types.
     */
struct symbol	*entersymbol();
struct symbol	*symbolalloc();
long		stringhash();
char		*enterstring();
char		*charalloc();
BOOL		nextelement();
time_t		mtime();
char		*classify();
