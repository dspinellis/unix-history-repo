/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)finger.h	5.5 (Berkeley) %G%
 */

#include <pwd.h>
#include <utmp.h>

/*
 * All unique persons are linked in a list headed by "head" and linkd
 * by the "next" field, as well as kept in a hash table.
 */

typedef struct person {
	struct person *next;		/* link to next person */
	struct person *hlink;		/* link to next person in hash bucket */
	uid_t uid;			/* user id */
	char *dir;			/* user's home directory */
	char *homephone;		/* pointer to home phone no. */
	char *name;			/* login name */
	char *office;			/* pointer to office name */
	char *officephone;		/* pointer to office phone no. */
	char *realname;			/* pointer to full name */
	char *shell;			/* user's shell */
	struct where *whead, *wtail;	/* list of where he is or has been */
} PERSON;

enum status { LASTLOG, LOGGEDIN };

typedef struct where {
	struct where *next;		/* next place he is or has been */
	enum status info;		/* type/status of request */
	short writable;			/* tty is writable */
	time_t loginat;			/* time of (last) login */
	time_t idletime;		/* how long idle (if logged in) */
	char tty[UT_LINESIZE+1];	/* null terminated tty line */
	char host[UT_HOSTSIZE+1];	/* null terminated remote host name */
} WHERE;

#define	HBITS	8			/* number of bits in hash code */
#define	HSIZE	(1 << 8)		/* hash table size */
#define	HMASK	(HSIZE - 1)		/* hash code mask */

PERSON *htab[HSIZE];			/* the buckets */
PERSON *phead, *ptail;			/* the linked list of all people */

int entries;				/* number of people */

PERSON *enter_person(), *find_person(), *palloc();
WHERE *walloc();

extern char tbuf[1024];			/* temp buffer for anybody */
