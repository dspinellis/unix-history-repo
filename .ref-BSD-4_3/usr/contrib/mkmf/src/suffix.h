/* $Header: suffix.h,v 1.1 85/03/14 15:38:45 nicklin Exp $ */

/*
 * Suffix definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Suffix types 
 */
#define SFXHEAD			'h'	/* header file name suffix */
#define SFXOBJ			'o'	/* object file name suffix */
#define SFXOUT			'x'	/* executable file name suffix */
#define SFXSRC			's'	/* source file name suffix */

/*
 * Suffix table structs
 */
typedef struct _suffix
	{
	char *suffix;			/* points to a suffix */
	int sfxtyp;			/* type of file name suffix */
	int inctyp;			/* type of included file */
	} SUFFIX;

typedef struct _sfxblk
	{
	SUFFIX sfx;			/* suffix struct */
	struct _sfxblk *next;		/* ptr to next suffix list block */
	} SFXBLK;
