/*	config.h	4.1	85/03/19	*/

#ifndef _CONFIG_
#define	_CONFIG_
/*
 * Compiler configuration definitions.
 */

/*
 * These flags control global compiler operation.
 */
#ifndef FORT
#define	ONEPASS		1		/* one-pass compiler */
#endif
#define	BUFSTDERR	1		/* buffer output to stderr */
#define STDPRTREE	1		/* means include prtree */
#define NESTCALLS	1		/* disallow two concurrent store()'s */
#define	FLEXNAMES	1		/* arbitrary length identifiers */

/*
 * Table sizes.
 */
#define TREESZ		1000		/* parse tree table size */
#define BCSZ		100		/* break/continue table size */
#define SYMTSZ		3000		/* symbol table size */
#define DIMTABSZ 	4200		/* dimension/size table size */
#define PARAMSZ		300		/* parameter stack size */
#define SWITSZ		500		/* switch table size */
#define	DELAYS		20		/* delayed evaluation table size */
#define NRECUR		(10*TREESZ)	/* maximum eval recursion depth */

/* in case anyone still uses fixed length names */
#ifndef FLEXNAMES
#define	NCHNAM		8		/* significant chars of identifier */
#endif
#endif
