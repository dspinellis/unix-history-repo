/* $Id: uudecode.c,v 4.4.3.1 1992/02/01 03:09:32 $
 * 
 * Decode one or more uuencoded articles back to binary form.
 * Trn version created by Wayne Davison.
 * Formerly the nn version by Kim Storm.
 * From the Berkeley original, modified by MSD, RDR, JPHD & WLS.
 */
/*
 * This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "respond.h"
#include "decode.h"

#define MAXCHAR 256
#define NORMLEN 64	/* allows for 84 encoded chars per line */

#define SEQMAX 'z'
#define SEQMIN 'a'

static char seqc;
static int first, secnd, check, numl;

static char blank;
static int chtbl[MAXCHAR], cdlen[NORMLEN + 3];
static int state;
static bool Xflag;
static int expecting_part;

static int decode_line _((char *));
static void inittbls _((void));
static void gettable _((FILE *));

#define	NO_ADVANCE		0x10

#define	FIND_BEGIN		0x01
#define	AFTER_ERROR_FIND_BEGIN	0x02
#define	DECODE_TEXT		0x03
#define	SKIP_TRAILING	       (0x04 | NO_ADVANCE)
#define	SKIP_LEADING		0x05
#define	FOUND_END	       (0x06 | NO_ADVANCE)
#define DECODE_ERROR	       (0x07 | NO_ADVANCE)
#define OTHER_ERROR	       (0x08 | NO_ADVANCE)
#define NEW_BEGIN	       (0x09 | NO_ADVANCE)

void
uud_start()
{
    Xflag = FALSE;
    expecting_part = 0;
    seqc = SEQMAX;
    check = 1;
    first = 1;
    secnd = 0;
    state = FIND_BEGIN;
}

int
uudecode(in)
FILE *in;
{
    int mode, onedone, lens;
    char buff[LBUFLEN];

    numl = onedone = 0;

    if (state == FIND_BEGIN)
	inittbls();

    /*
     * search for header or translation table line.
     */

    while ((state & NO_ADVANCE) || fgets(buff, sizeof buff, in) != Nullch) {
	numl++;

	switch (state) {
	 case NEW_BEGIN:
	    if (decode_fp != Nullfp) {
		if (expecting_part) {
		    register int got_part = 0;

		    if (strnEQ(buff + 6, "part ", 5)) {
			register char *bp;

			for (bp = buff + 11; islower(*bp); bp++)
			    got_part = got_part * 26 + *bp - 'a';
		    }
		    if (expecting_part == got_part) {
			state = DECODE_TEXT;
			break;
		    }
		    printf("Expecting part %d; got part %d.\n",
			 expecting_part + 1, got_part + 1);
		    if (got_part) {
			state = SKIP_LEADING;
			return -1;
		    }
		}
		decode_end();
		sleep(2);
		Xflag = FALSE;
		expecting_part = 0;
	    }
	    state = FIND_BEGIN;
	    /* fall thru */

	 case FIND_BEGIN:
	 case AFTER_ERROR_FIND_BEGIN:
	    if (strnEQ(buff, "table", 5)) {
		gettable(in);
		continue;
	    }

	    if (strnEQ(buff, "begin ", 6)
	     || strnEQ(buff, "Xbegin ", 7)) {
		lens = strlen(buff)-1;
		if (buff[lens] == '\n')
		    buff[lens] = '\0';

		if(sscanf(buff+6,"%o%s", &mode, decode_fname) != 2) {
		    register char *bp = buff + 6;

		    if (*bp == ' ')
			bp++;
		    if (strnEQ(bp, "part ", 5)) {
			register int got_part = 0;

			for (bp = bp + 5; islower(*bp); bp++)
			    got_part = got_part * 26 + *bp - 'a';
			printf("Expecting part 1; got part %d.\n",
				got_part + 1);
			return -1;
		    }
		    continue;
		}

		Xflag = (*buff == 'X');

		sprintf(decode_dest, "%s/%s", extractdest, decode_fname);

		if ((decode_fp = fopen(decode_dest, FOPEN_WB)) == Nullfp) {
		    printf("Cannot create file: %s\n", decode_dest);
		    goto err;
		}
		chmod(decode_dest, mode);
		printf("Decoding: %s\n", decode_fname);
		state = DECODE_TEXT;
	    }
	    continue;

	 case SKIP_LEADING:
	    state = decode_line(buff);
	    continue;

	 case DECODE_TEXT:
	    state = decode_line(buff);
	    onedone = 1;
	    continue;

	 case FOUND_END:
	    fclose(decode_fp);
	    decode_fp = Nullfp;
	    Xflag = FALSE;
	    expecting_part = 0;
	    state = FIND_BEGIN;
	    printf("Done.\n");
	    continue;

	 case SKIP_TRAILING:
	    printf("(Continued)\n");
	    state = SKIP_LEADING;
	    return 0;

	 case DECODE_ERROR:
	    state = SKIP_TRAILING;
	    continue;

	 case OTHER_ERROR:
	    fclose(decode_fp);
	    decode_fp = Nullfp;
	    Xflag = FALSE;
	    expecting_part = 0;
	    state = AFTER_ERROR_FIND_BEGIN;
	    goto err;
	}
    }

    if (onedone) {
	if (state == DECODE_TEXT) {
	    printf("(Continued)\n");
	    state = SKIP_LEADING;
	}
	return 0;
    }

    if (state == AFTER_ERROR_FIND_BEGIN)
	return -1;
    printf("Couldn't find anything to decode.\n");

 err:
    sleep(2);
    return -1;
}

/*
 * decode one line and write it out using decode_fp
 */

static int
decode_line(buff)
char *buff;
{
    char outl[LBUFLEN];
    register char *bp, *ut;
    register int *trtbl = chtbl;
    register int n;
    register int blen;		/* binary length (from decoded file) */
    register int rlen;		/* calculated input line length */
    register int len;		/* actual input line length */
    register int dash;		/* number of '-'s encountered on a line */
				/* If it's too high, we reject the line */

#   define REJECT(buf,rlen,len) \
	((*buf == 'M' && len > rlen + 5) \
	 || (*buf != 'M' && len != rlen && len != rlen+1) \
	 || (strnEQ(buf, "BEGIN", 5)) \
	 || (strnEQ(buf, "END", 3)))

    if (Xflag) {
	if (*buff == 'X')
	    buff++;
	else
	    *buff = 'x';	/* force a mis-parse of a non-x'ed line */
    }
    len = strlen(buff);
    if (--len <= 0)
	return state;

    buff[len] = '\0';

    /*
     * Get the binary line length.
     */
    if ((blen = trtbl[buff[0]]) < 0) {
	if (state == SKIP_LEADING) {
	    if (strnEQ(buff, "begin ", 6))
		return NEW_BEGIN;

	    return SKIP_LEADING;
	}
	/*
	 * end of uuencoded file ?
	 */
	if (strnEQ(buff, "end", 3))
	    return FOUND_END;

	/*
	 * end of current file ? : get next one.
	 */
	if (strnEQ(buff, "include ", 8)) {
	    for (bp = buff + 8; *bp; bp++) {
		if (bp[0] == '.' && bp[1] == 'u') {
		    expecting_part = (bp[2] - 'a') * 26 + bp[3] - 'a';
		    break;
		}
	    }
	}

	/*
	 * trailing garbage
	 */
	return SKIP_TRAILING;
    }

    rlen = cdlen[blen];
    if (state == SKIP_LEADING && REJECT(buff,rlen,len))
	return SKIP_LEADING;

    /*
     * Is it the empty line before the end line ?
     */
    if (blen == 0)
	return state;

    if (REJECT(buff,rlen,len))
	return SKIP_TRAILING;

    /*
     * Pad with blanks.
     */
    for (bp = buff + len, n = rlen - len; --n >= 0; )
	*bp++ = blank;

    /*
     * Verify
     */
    for (n = rlen, bp = buff, dash = 0; --n >= 0; bp++) {
	if (trtbl[*bp] < 0) {
	    if (state == SKIP_LEADING)
		return SKIP_LEADING;
	    return DECODE_ERROR;
	}
	if (*bp == '-')
	    dash++;
    }
    if (dash * 100 / rlen > 33)		/* more than 1/3 dashes? */
	if (state == SKIP_LEADING)
	    return SKIP_LEADING;	/* -> reject */
	else
	    return SKIP_TRAILING;

    /*
     * Check for uuencodes that append a 'z' to each line....
     */
    if (check)
	if (secnd) {
	    secnd = 0;
	    if (buff[rlen] == SEQMAX)
		check = 0;
	} else if (first) {
	    first = 0;
	    secnd = 1;
	    if (buff[rlen] != SEQMAX)
		check = 0;
	}

    /*
     * There we check.
     */
    if (check) {
	if (buff[rlen] != seqc) {
	    if (state == SKIP_LEADING)
		return SKIP_LEADING;
	    return DECODE_ERROR;
	}

	if (--seqc < SEQMIN)
	    seqc = SEQMAX;
    }

    /*
     * output a group of 3 bytes (4 input characters).
     * the input chars are pointed to by p, they are to
     * be output to file f. blen is used to tell us not to
     * output all of them at the end of the file.
     */
    ut = outl;
    n = blen;
    bp = &buff[1];
    while (--n >= 0) {
	*(ut++) = trtbl[*bp] << 2 | trtbl[bp[1]] >> 4;
	if (n > 0) {
	    *(ut++) = (trtbl[bp[1]] << 4) | (trtbl[bp[2]] >> 2);
	    n--;
	}
	if (n > 0) {
	    *(ut++) = trtbl[bp[2]] << 6 | trtbl[bp[3]];
	    n--;
	}
	bp += 4;
    }
    if (fwrite(outl, 1, blen, decode_fp) <= 0) {
	printf("Error on writing decoded file\n");
	return OTHER_ERROR;
    }

    return DECODE_TEXT;
}



/*
 * Install the table in memory for later use.
 */
static void
inittbls()
{
    register int i, j;

    /*
     * Set up the default translation table.
     */
    for (i = 0; i < ' '; i++)
	chtbl[i] = -1;
    for (i = ' ', j = 0; i < ' ' + 64; i++, j++)
	chtbl[i] = j;
    for (i = ' ' + 64; i < MAXCHAR; i++)
	chtbl[i] = -1;
    chtbl['`'] = chtbl[' '];	/* common mutation */
    chtbl['~'] = chtbl['^'];	/* another common mutation */
    blank = ' ';
    /*
     * set up the line length table, to avoid computing lotsa * and / ...
     */
    cdlen[0] = 1;
    for (i = 1, j = 5; i <= NORMLEN; i += 3, j += 4)
	cdlen[i] = (cdlen[i + 1] = (cdlen[i + 2] = j));
}

static void
gettable(in)
FILE *in;
{
    char buff[LBUFLEN];
    register int c, n = 0;
    register char *cpt;

    for (c = 0; c < MAXCHAR; c++)
	chtbl[c] = -1;

    for (;;) {
	if (fgets(buff, sizeof buff, in) == Nullch) {
	    printf("EOF while in translation table.\n");
	    return;
	}
	numl++;
	if (strnEQ(buff, "begin", 5)) {
	    printf("Incomplete translation table.\n");
	    return;
	}
	cpt = buff + strlen(buff) - 1;
	*cpt = ' ';
	while (*cpt == ' ') {
	    *cpt = 0;
	    cpt--;
	}
	cpt = buff;
	while (c = *cpt) {
	    if (chtbl[c] != -1) {
		printf("Duplicate char in translation table.\n");
		return;
	    }
	    if (n == 0)
		blank = c;
	    chtbl[c] = n++;
	    if (n >= 64)
		return;
	    cpt++;
	}
    }
}

