/*
 * Definitions of translate tables used for ascii<->ebcdic translation.
 */

#define	INCLUDED_ASCEBC

/*
 * ascii/ebcdic translation information
 */

#define	NASCII	128		/* number of ascii characters */

#define	NEBC	256		/* number of ebcdic characters */

extern unsigned char
	asc_ebc[NASCII], ebc_asc[NEBC];
