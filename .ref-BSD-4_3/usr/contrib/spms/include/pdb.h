/* $Header$ */

/*
 * Project database definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Buffer sizes
 */
#define PBUFSIZE	1024		/* project database buffer size */
#define PDBERRSIZE	256		/* database error message buffer size */
/*
 * Database access mode definitions
 */
#define _PACCESS	007		/* access mask */
#define _PREAD		001		/* database open for reading */
#define _PWRITE		002		/* database open for writing */
#define _PAPPEND	004		/* database open for appending */
/*
 * Database status definitions
 */
#define _PSTAT		070		/* status mask */
#define _PUPDATE	010		/* database updated */
#define _PEOF		020		/* EOF */
#define _PERR		040		/* error */
/*
 * Project database structure
 */
typedef struct _pdbdesc			/* database description struct */
	{
	char root[PATHSIZE];		/* database directory pathname */
	char path[PATHSIZE];		/* database pathname */
	FILE *fp;			/* database file pointer */
	char tpath[PATHSIZE];		/* temporary database pathname */
	FILE *tfp;			/* temporary database file ptr */
	unsigned short flag;		/* mode of database access */
	char pbuf[PBUFSIZE];		/* database line buffer */
	char perr[PDBERRSIZE];		/* database error message buffer */
	} PDB;
/*
 * Functions defined for project databases
 */
extern int _closepdb();			/* close database without updating */
extern int closepdb();			/* close database */
extern int errpdb();			/* print database error message */
extern PDB *mustopenpdb();		/* must open database or die */
extern PDB *openpdb();			/* open database */
extern void renamepdb();		/* rename database */
extern void resetpdb();			/* reset current database ptr */
extern void rewindpdb();		/* rewind database */
extern int  pfndent();			/* find and load database entry */
extern int  pgetent();			/* load next entry into buffer */
extern int  pputent();			/* write buffer to database */
extern void prment();			/* remove database entry */
extern void paddkey();			/* add key to specified entries */
extern void pchgkey();			/* change specified key */
extern void prmkey();			/* remove specified key */
extern void paddflag();			/* add flags */
extern void pchgflag();			/* change flags */
extern void pputflag();			/* add or change flags */
extern void prmflag();			/* remove flags */
extern void paddstring();		/* add or change strings */
extern void pchgstring();		/* change strings */
extern void pputstring();		/* add or change strings */
extern void prmstring();		/* remove strings */
