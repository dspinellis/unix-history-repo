/*	config.h	4.5	90/11/16	*/

#ifndef _CONFIG_
#define	_CONFIG_
/*
 * Compiler configuration definitions.
 */

/*
 * These flags control global compiler operation.
 */
#define	BUFSTDERR	1		/* buffer output to stderr */
#define STDPRTREE	1		/* means include prtree */
#define NESTCALLS	1		/* disallow two concurrent store()'s */
#define	FLEXNAMES	1		/* arbitrary length identifiers */
#ifdef FORT
#define	NOMAIN		1		/* use f1 main routine */
#endif

/*
 * Table sizes.
 */
#define TREESZ		2000		/* parse tree table size */
#define BCSZ		200		/* break/continue table size */
#define SYMTSZ		8000		/* symbol table size */
#define DIMTABSZ 	10000		/* dimension/size table size */
#define PARAMSZ		600		/* parameter stack size */
#define SWITSZ		1000		/* switch table size */
#define	DELAYS		20		/* delayed evaluation table size */
#define NRECUR		(10*TREESZ)	/* maximum eval recursion depth */
#define	MAXSCOPES	200		/* maximum active scopes ('{' depth) */

/* in case anyone still uses fixed length names */
#ifndef FLEXNAMES
#define	NCHNAM		8		/* significant chars of identifier */
#endif
#endif
