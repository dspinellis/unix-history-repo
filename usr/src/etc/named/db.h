/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)db.h	4.14 (Berkeley) 6/18/88
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
	u_long	d_ttl;			/* time to live */
	short	d_flags;
	short	d_zone;			/* zone number */
	short	d_class;		/* class number */
	short	d_type;			/* type number */
	short	d_mark;			/* place to mark data */
	short	d_size;			/* size of data area */
	u_long	d_nstime;		/* NS response time, milliseconds */
	char	d_data[MAXDATA]; 	/* the data is malloc'ed to size */
};
#define DATASIZE(n) (sizeof(struct databuf) - MAXDATA + n)

/*
 * d_flags definitions
 */
#define DB_F_HINT       0x01	/* databuf belongs to fcachetab */

struct namebuf {
	char	*n_dname;		/* domain name */
	u_int	n_hashval;		/* hash value of n_dname */
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
#define DB_NODATA	0x01	/* data should not exist */
#define DB_MEXIST	0x02	/* data must exist */
#define DB_DELETE	0x04	/* delete data if it exists */
#define DB_NOTAUTH	0x08	/* must not update authoritative data */
#define DB_NOHINTS      0x10	/* don't reflect update in fcachetab */

#define DB_Z_CACHE      (0)	/* cache-zone-only db_dump()  */
#define DB_Z_ALL        (-1)	/* normal db_dump() */

/*
 * Error return codes
 */
#define OK		0
#define NONAME		-1
#define NOCLASS		-2
#define NOTYPE		-3
#define NODATA		-4
#define DATAEXISTS	-5
#define NODBFILE	-6
#define TOOMANYZONES	-7
#define GOODDB		-8
#define NEWDB		-9
#define AUTH		-10

extern struct hashbuf *hashtab;		/* root hash table */
extern struct invbuf *invtab[];		/* inverse hash table */
extern struct hashbuf *fcachetab;	/* hash table for cache read from file*/

extern struct namebuf *nlookup();
extern struct namebuf *savename();
extern struct databuf *savedata();
extern struct databuf *rm_datum();
extern struct hashbuf *savehash();
extern struct invbuf *saveinv();
extern char *savestr();
extern char *malloc(), *realloc(), *calloc();
