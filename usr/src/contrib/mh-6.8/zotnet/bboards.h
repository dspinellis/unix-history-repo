/* bboards.h - definition of a BBoard structure */
/* $Id: bboards.h,v 1.2 1992/05/12 22:04:52 jromine Exp $ */

#define	BBOARDS	"bboards"	/* name in /etc/passwd */
#define	BBDB	"BBoards"	/* file in BBOARDS' home directory */
#define	BBMODE	0644		/* default BBoards mode */
#define	DISTADR	"dist-"		/* prefix for distribution addresses */

#ifdef	POP
#define	POPUID	"pop"		/* name in /etc/passwd */
#define	POPDB	"POP"		/* file in POPUID's home directory */
#define	POMODE	0600		/* default POP subscriber maildrop mode */
#endif /* POP */

struct bboard {
    char   *bb_name;		/* name of the bboard */
    char  **bb_aka;		/* aliases for the bboards */

    char   *bb_file;		/* file it resides in */
    char   *bb_archive;		/* file where archives reside */
    char   *bb_info;		/* file where maxima resides */
    char   *bb_map;		/* file where binary map resides */

    char   *bb_passwd;		/* password for it */

    char  **bb_leader;		/* list of local leaders */

    char   *bb_addr;		/* network address */
    char   *bb_request;		/* network address for requests */
    char   *bb_relay;		/* host acting as relay in local domain */
    char  **bb_dist;		/* distribution list */

    unsigned int    bb_flags;	/* various flags */
#define	BB_NULL	0x0000
#define	BB_ARCH	0x0007		/* archive policy */
#define   BB_ASAV	0x0001	/*   save in archives/ directory */
#define	  BB_AREM	0x0002	/*   remove without saving */
#define BB_INVIS	0x0010	/* invisible to bbc */
#define	BB_REMOTE	0x0020	/* remote to bbc */
#define	BB_SEEN		0x0040	/* seen by bbc */
#define	BBITS	"\020\01ARCHIVE\02REMOVE\05INVIS\06REMOTE\07SEEN"

    union {			/* unassigned */
	unsigned int    un_count;
	long		un_mtime;
    } bb_un;
#define	bb_count	bb_un.un_count
#define	bb_mtime	bb_un.un_mtime
    
    unsigned int    bb_maxima;	/* highest BBoard-Id in it */
    char   *bb_date;		/* date that maxima was written */

    struct bboard *bb_next;	/* unassigned */
    struct bboard *bb_link;	/* unassigned */
    struct bboard *bb_chain;	/* unassigned */
};

				/* flags for setbbent () */
#define	SB_NULL	0x0000
#define	SB_STAY	0x0001		/*   stay open between calls */
#define	SB_FAST	0x0002		/*   fast parse of file */

void	make_lower ();
int     setbbent (), endbbent (), setbbfile (), setbbinfo (), setpwinfo (),
	ldrbb (), ldrchk (), getbbdist ();
long	getbbtime ();
char   *getbberr ();
struct bboard  *getbbent (), *getbbnam (), *getbbaka (), *getbbcpy();
