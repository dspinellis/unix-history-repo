/* $Header$ */

/*
 * Project Link Directory Definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Nomenclature
 */
#define PLDNAME		"..."		/* project link directory */

/*
 * Buffer sizes
 */
#define PD_BUFSIZE	1012		/* project directory entry buf size */

/*
 * Project link directory (PLD) flag definitions
 */
#define PDIRDESC	"dd"		/* directory description */
#define PDIRPATH	"dp"		/* directory path */
#define PDIRTYPE	"dt"		/* directory type */
#define PROOTDIR	"^^"		/* project root directory flag */

/*
 * Project link directory entry struct
 */
typedef struct _pdir
	{
	unsigned long pd_mode;		/* type of project directory */
	unsigned long pd_reclen;	/* actual length of this record */
	unsigned short pd_alen;		/* alias length */
	unsigned short pd_plen;		/* pathname length */
	unsigned short pd_tlen;		/* type labels length */
	unsigned short pd_dlen;		/* directory description length */
	char pd_buf[PD_BUFSIZE];	/* project directory entry buffer */
	} PDIR;

/*
 * Handy macros
 */
#define PD_ALIAS(p)	((p).pd_buf)
#define PD_PATH(p)	((p).pd_buf+(p).pd_alen+1)
#define PD_TYPE(p)	((p).pd_buf+(p).pd_alen+(p).pd_plen+2)
#define PD_DESC(p)	((p).pd_buf+(p).pd_alen+(p).pd_plen+(p).pd_tlen+3)
