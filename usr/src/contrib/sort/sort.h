/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Peter McIlroy.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sort.h	5.1 (Berkeley) %G%
 */

#include <sys/param.h>

#include <db.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define NBINS 256
#define MAXMERGE 16

/* values for masks, weights, and other flags. */
#define I 1		/* mask out non-printable characters */
#define D 2		/* sort alphanumeric characters only */
#define N 4		/* Field is a number */
#define F 8		/* weight lower and upper case the same */
#define R 16		/* Field is reversed with respect to the global weight */
#define BI 32		/* ignore blanks in icol */
#define BT 64		/* ignore blanks in tcol */

/* masks for delimiters: blanks, fields, and termination. */
#define BLANK 1		/* ' ', '\t'; '\n' if -T is invoked */
#define FLD_D 2		/* ' ', '\t' default; from -t otherwise */
#define REC_D_F 4	/* '\n' default; from -T otherwise */

#define ND 10	/* limit on number of -k options. */

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define	FCLOSE(file) {							\
	if (EOF == fclose(file))					\
		err(2, "%s", file);					\
}

#define	EWRITE(ptr, size, n, f) {					\
	if (!fwrite(ptr, size, n, f))					\
		 err(2, NULL);						\
}

/* length of record is currently limited to 2^16 - 1 */
typedef u_short length_t;

#define SALIGN(n) ((n+1) & ~1)

/* a record is a key/line pair starting at rec.data. It has a total length
 * and an offset to the start of the line half of the pair.
 */
typedef struct recheader {
	length_t length;
	length_t offset;
	u_char data[1];
} RECHEADER;

typedef struct trecheader {
	length_t length;
	length_t offset;
} TRECHEADER;

/* This is the column as seen by struct field.  It is used by enterfield.
 * They are matched with corresponding coldescs during initialization.
 */
struct column {
	struct coldesc *p;
	int num;
	int indent;
};

/* a coldesc has a number and pointers to the beginning and end of the
 * corresponding column in the current line.  This is determined in enterkey.
 */
typedef struct coldesc {
	u_char *start;
	u_char *end;
	int num;
} COLDESC;

/* A field has an initial and final column; an omitted final column
 * implies the end of the line.  Flags regulate omission of blanks and
 * numerical sorts; mask determines which characters are ignored (from -i, -d);
 * weights determines the sort weights of a character (from -f, -r).
 */
struct field {
	struct column icol;
	struct column tcol;
	u_int flags;
	u_char *mask;
	u_char *weights;
};

union f_handle {
	int top;
	char **names;
};
extern int PANIC;	/* maximum depth of fsort before fmerge is called */
extern u_char ascii[NBINS], Rascii[NBINS], Ftable[NBINS], RFtable[NBINS];
extern u_char alltable[NBINS], dtable[NBINS], itable[NBINS];
extern u_char d_mask[NBINS];
extern int SINGL_FLD, SEP_FLAG, UNIQUE;
extern int REC_D;

#include "extern.h"
