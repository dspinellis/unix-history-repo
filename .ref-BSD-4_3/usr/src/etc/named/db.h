/*	db.h	4.3	86/06/04	*/

/*
 * Copyright (c) 1985 Regents of the University of California
 *	All Rights Reserved
 */

/*
 * Global structures and variables for data base routines.
 */

#define INVBLKSZ	7	/* # of namebuf pointers per block */
#define INVHASHSZ	919	/* size of inverse hash table */

	/* max length of data in RR data field */
#define MAXDATA		256

/*
 * Hash table structures.
 */
struct databuf {
	struct	databuf *d_next;	/* linked list */
	short	d_refcount;		/* reference count */
	short	d_zone;			/* zone number */
	short	d_class;		/* class number */
	short	d_type;			/* type number */
	u_long	d_ttl;			/* time to live */
	int	d_size;			/* size of data area */
	char	d_data[MAXDATA];	/* malloc'ed as needed */
};
#define DATASIZE(n) (sizeof(struct databuf) - MAXDATA + n)

struct namebuf {
	char	*n_dname;		/* domain name */
	struct	namebuf *n_next;	/* linked list */
	struct	databuf *n_data;	/* data records */
	struct	namebuf *n_parent;	/* parent domain */
	struct	hashbuf *n_hash;	/* hash table for children */
};

struct invbuf {
	struct	invbuf *i_next;		/* linked list */
	struct	namebuf	*i_dname[INVBLKSZ];	/* domain name */
};

struct hashbuf {
	int	h_size;			/* size of hash table */
	int	h_cnt;			/* number of entries */
	struct	namebuf	*h_tab[1];	/* malloc'ed as needed */
};
#define HASHSIZE(s) (s*sizeof(struct namebuf *) + 2*sizeof(int))

#define HASHSHIFT	3
#define HASHMASK	0x1f

/*
 * Flags to updatedb
 */
#define DB_NODATA	0x1		/* data should not exist */
#define DB_MEXIST	0x2		/* data must exist */
#define DB_DELETE	0x4		/* delete data if it exists */

/*
 * Error return codes
 */
#define OK		0
#define NONAME		-1
#define NOCLASS		-2
#define NOTYPE		-3
#define NODATA		-4
#define DATAEXISTS	-5
#define NODBFILE		-6
#define TOOMANYZONES	-7
#define GOODDB		-8
#define NEWDB		-9

extern struct hashbuf *hashtab;		/* root hash table */
extern struct invbuf *invtab[];		/* inverse hash table */

extern struct namebuf *nlookup();
extern struct namebuf *savename();
extern struct databuf *savedata();
extern struct hashbuf *savehash();
extern struct invbuf *saveinv();
extern char *savestr();
