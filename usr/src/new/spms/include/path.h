/* $Header$ */

/*
 * Pathname definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Buffer sizes
 */
#define ALIASSIZE	30		/* project directory alias size */
#define TYPESIZE	30		/* project directory type label size */
#define DIRDESCSIZE	128		/* project directory description size */
#define P_BUFSIZE	1024		/* pathname buffer size */
#define	PATHSIZE	256		/* maximum pathname length */
#define PPATHSIZE	128		/* maximum project pathname length */
#define TYPBUFSIZE	256		/* directory type labels buffer */

/* 
 * Nomenclature (WARNING: Check definition usage BEFORE changing)
 */
#define _CDIRC		'.'		/* current directory character */
#define _HDIRC		'~'		/* home directory character */
#define _PDIRC		'^'		/* project root directory character */
#define _PDTSC		'/'		/* project dir type labels sep char */
#define _PPSC		'^'		/* project path separator character */
#define _PSC		'/'		/* pathname separator character */
#define _RDIRC		'/'		/* root directory character */
#define CURDIR		"."		/* current directory */
#define PARENTDIR	".."		/* parent directory */
#define PATHSEP		"/"		/* pathname separator */
#define PPATHSEP	"^"		/* project pathname separator */
#define ROOTDIR		"/"		/* root directory */
#define ROOTPROJECT	"^"		/* root project */
#define USERPROJECT	"~"		/* user's root project */

/*
 * Pathname types
 */
#define P_IFMT		0xf0000		/* project pathname mask */
#define	P_IFNEW		0x00000		/* new directory or file */
#define P_IFREG		0x10000		/* regular directory or file */
#define	P_IFHOME	0x20000		/* root project root directory */
#define P_IFPDIR	0x30000		/* project directory */
#define	P_IFPROOT	0x40000		/* project root directory */

/*
 * Pathname struct
 */
typedef struct _path
	{
	unsigned long p_mode;		/* type of pathname */
	char *p_alias;			/* pathname alias */
	char *p_path;			/* pathname */
	char *p_type;			/* project directory type labels */
	char *p_desc;			/* project directory description */
	char p_buf[P_BUFSIZE];		/* pathname buffer */
	char p_project[PATHSIZE];	/* pathname's project */
	} PATH;
