/* $Header$ */

/*
 * Project database buffer definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Field Definitions
 */
#define _PBKS		'|'		/* key field separator character */
#define _PBFS		':'		/* non-key field separator character */
/*
 * Functions defined for project database buffers
 */
extern void pbclear();			/* clear buffer */
extern void pbshrink();			/* shrink buffer */
extern int pbstretch();			/* stretch buffer */
extern int pbaddkey();			/* add key */
extern int pbchgkey();			/* change existing key */
extern int pbcmpkey();			/* compare keys */
extern char *pbcpykey();		/* copy key */
extern char *pbfndkey();		/* find key */
extern char *pbgetkey();		/* get next key */
extern int pblenkey();			/* length of key */
extern void pbrmkey();			/* remove key */
extern char *pbskipkey();		/* skip to next key */
extern int pbaddfield();		/* add non-key field */
extern int pbchgfield();		/* change existing non-key field */
extern int pbcmpfield();		/* compare non-key fields */
extern char *pbcpyfield();		/* copy non-key field */
extern char *pbfndfield();		/* find non-key field */
extern char *pbgetfield();		/* get next non-key field */
extern int pblenfield();		/* length of non-key field */
extern void pbrmfield();		/* remove non-key field */
extern char *pbskipfield();		/* skip to next non-key field */
extern int pbaddflag();			/* add flag field */
extern void pbchgflag();		/* change existing flag field */
extern int pbfndflag();			/* find flag field */
extern void pbrmflag();			/* remove flag field */
extern int pbaddstring();		/* add string field */
extern int pbchgstring();		/* change existing string field */
extern char *pbfndstring();		/* find string field */
extern char *pbgetstring();		/* get specified string field */
extern void pbrmstring();		/* remove string field */
