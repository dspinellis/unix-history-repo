/* $Id: nghash.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "ndir.h"
#include "rcstuff.h"
#include "trn.h"
#include "intrp.h"
#include "final.h"
#include "rcln.h"
#include "util.h"
#include "nntp.h"
#include "ngdata.h"
#include "hash.h"
#include "term.h"

static HASHTABLE *acthash;
static char *actfile;

void
ngdatansrv_init()		/* non-NNTP initialisation */
{
    register long offset;
    char *cp = filexp(ACTIVE);
    struct stat actstat;

    actfp = fopen(cp, "r");
    if (actfp == Nullfp) {
	printf(cantopen, cp) FLUSH;
	finalize(1);
    }

    /* rn was being a slug about rereading the active file over and
     * over; try using a really big buffer to keep it in core. */
    (void) fstat(fileno(actfp), &actstat);
    actfile = safemalloc(actstat.st_size + 1);
    rewind(actfp);
    /*
     * NOTE: this won't work on machines with ints too small to hold
     * the size of the active file.
     */
    if (fread(actfile, 1, (int)actstat.st_size, actfp) != actstat.st_size) {
	fprintf(stderr, "active file not read\n");
	actfile[0] = '\0';
    }
    rewind(actfp);
    actfile[actstat.st_size] = '\0';

    acthash = hashcreate((int)(actstat.st_size/40), (int (*)())NULL);

    /* count entries */
    for (activeitems=0, offset=0; offset < actstat.st_size; activeitems++) {
	register char *p;
	HASHDATUM data;

	data.dat_ptr = actfile + offset;
	if ((p = index(data.dat_ptr, '\n')) == NULL)
	    data.dat_len = actstat.st_size - offset;
	else
	    data.dat_len = p - data.dat_ptr + 1;
	if ((p = index(data.dat_ptr, ' ')) == NULL)
	    p = data.dat_ptr;
	hashstore(acthash, data.dat_ptr, p - data.dat_ptr, data);
	offset += data.dat_len;
    }
}

ACT_POS
findact(outbuf, nam, len, suggestion)
register char *outbuf;
register char *nam;
register int len;
register long suggestion;
{
    register ACT_POS retval;
    extern int debug;

    /* see if we know the right place and can just return */
    if (suggestion != 0 && fseek(actfp, suggestion, 0) >= 0
     && fgets(outbuf, LBUFLEN, actfp) != NULL && outbuf[len] == ' '
     && strnEQ(outbuf, nam, len))
	return suggestion;

    /* hmm, apparently not, gotta look it up. */
#ifdef DEBUG
    if (debug & DEB_SOFT_POINTERS)
	printf("Missed, looking for %s in %sLen = %d\n",nam,outbuf,len) FLUSH;
#endif
    /* can we just do a quick hashed lookup? */
    if (acthash != NULL) {
	HASHDATUM data;

	outbuf[0] = '\0';
	data = hashfetch(acthash, nam, len);
	if (data.dat_ptr == NULL)
	    return -1;
	else {
	    (void) bcopy(data.dat_ptr, outbuf, (int)data.dat_len);
	    outbuf[data.dat_len] = '\0';
	    return data.dat_ptr - actfile;
	}
    }

    /* apparently not.  gotta do it the slow way. */
    (void) fseek(actfp, 0L, 0);
    for (retval=0; fgets(outbuf,LBUFLEN,actfp)!=NULL; retval+=strlen(outbuf))
	if (outbuf[len] == ' ' && strnEQ(outbuf, nam, len))
	    break;
    if (ferror(actfp)) {
	perror("error on active file");
	sig_catcher(0);
	/* NOTREACHED */
    } else if (feof(actfp))
	return -1;	/* no such group */
    return retval;
}

#ifdef EDIT_DISTANCE

/* Edit Distance extension to trn
 *
 *	Mark Maimone (mwm@cmu.edu)
 *	Carnegie Mellon Computer Science
 *	9 May 1993
 *
 *	This code helps trn handle typos in newsgroup names much more
 *   gracefully.  Instead of "... does not exist!!", it will pick the
 *   nearest one, or offer you a choice if there are several options.
 */

#define LENGTH_HACK 5	/* Don't bother comparing strings with lengths
			 * that differ by more than this; making this
			 * smaller than MIN_DIST prunes the number of
			 * calls to edit_dist().  We won't catch any
			 * sequences of MIN_DIST INSERTs or DELETEs,
			 * but we (should!) save some time. */
#define MAX_NG 9	/* Maximum number of options */

#define ABS(x) (((x) < 0) ? -(x) : (x))

static void check_distance _((HASHDATUM*,int));

static char **ngptrs;		/* List of potential matches */
static int ngn;			/* Length of list in ngptrs[] */
static int nglen;		/* Length of name in ngname */
static int best_match;		/* Value of best match */

/* find_close_match -- Finds the closest match for the string given in
 * global ngname.  If found, the result will be the corrected string
 * returned in that global.
 *
 * We compare the (presumably misspelled) newsgroup name with all legal
 * newsgroups, using the Edit Distance metric.  The edit distance between
 * two strings is the minimum number of simple operations required to
 * convert one string to another (the implementation here supports INSERT,
 * DELETE, CHANGE and SWAP).  This gives every legal newsgroup an integer
 * rank.
 *
 * You might want to present all of the closest matches, and let the user
 * choose among them.  But because I'm lazy I chose to only keep track of
 * all with newsgroups with the *single* smallest error, in array ngptrs[].
 * A more flexible approach would keep around the 10 best matches, whether
 * or not they had precisely the same edit distance, but oh well.
 */

int
find_close_match()
{
   int ret = 0;

    best_match = -1;
    ngptrs = (char**)safemalloc(MAX_NG * sizeof (char*));
    ngn = 0;
    nglen = strlen(ngname);

/* Iterate over all legal newsgroups */
    hashwalk(acthash, check_distance, 0);

/* ngn is the number of possibilities.  If there's just one, go with it. */

    switch (ngn) {
        case 0:
	    break;
	case 1: {
	    char *cp = index(ngptrs[0], ' ');
	    *cp = '\0';
#ifdef VERBOSE
	    IF(verbose)
		printf("(I assume you meant %s)\n", ngptrs[0]) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		printf("(Using %s)\n", ngptrs[0]) FLUSH;
#endif
	    set_ngname(ngptrs[0]);
	    *cp = ' ';
	    ret = 1;
	    break;
	}
	default:
	    ret = get_near_miss();
	    break;
    } /* switch */
    free((char*)ngptrs);
    return ret;
} /* find_close_match */

static void
check_distance(data, ngnum)
HASHDATUM *data;
int ngnum;
{
    int value, len;
    char *cp;
    if ((cp = index(data->dat_ptr, ' ')) == NULL)
	return;
    len = cp - data->dat_ptr;

/* Efficiency:  don't call edit_dist when the lengths are already too
 * different */

    if (ABS(len - nglen) > LENGTH_HACK)
	return;

    value = edit_distn(ngname, strlen(ngname), data->dat_ptr, len);
    if (value > MIN_DIST)
	return;

    if (value < best_match)
	ngn = 0;
    if (best_match < 0 || value <= best_match) {
	best_match = value;
	if (ngn < MAX_NG)
	    ngptrs[ngn++] = data->dat_ptr;
    } /* if */
}

/* Now we've got several potential matches, and have to choose between them
 * somehow.  Again, results will be returned in global ngname */

int
get_near_miss()
{
    char promptbuf[256];
    char options[MAX_NG+10], *op = options;
    int i;

#ifdef VERBOSE
    IF(verbose)
	printf("However, here are some close matches:\n") FLUSH;
#endif
    if (ngn > 9)
	ngn = 9;	/* Since we're using single digits.... */
    *options = '\0';
    for (i = 0; i < ngn; i++) {
	char *cp = index(ngptrs[i], ' ');
	*cp = '\0';
	printf("  %d.  %s\n", i+1, ngptrs[i]);
	sprintf(op++, "%d", i+1);	/* Expensive, but avoids ASCII deps */
	*cp = ' ';
    } /* for */

#ifdef VERBOSE
    IF(verbose)
	sprintf(promptbuf, "Which of these would you like? [%sn] ", options);
    ELSE
#endif
#ifdef TERSE
	sprintf(promptbuf, "Which? [%sn] ", options);
#endif
reask:
    in_char(promptbuf, 'A');
    setdef(buf, "1");
#ifdef VERIFY
    printcmd();
#endif
    putchar('\n') FLUSH;
    switch (*buf) {
        case 'n':
	case 'N':
	case 'q':
	case 'Q':
	case 'x':
	case 'X':
	    return 0;
	    break;
	case 'h':
	case 'H':
#ifdef VERBOSE
	    IF(verbose)
		fputs("  You entered an illegal newsgroup name, and these are the nearest possible\n  matches.  If you want one of these, then enter its number.  Otherwise\n  just say 'n'.\n", stdout) FLUSH;
	    ELSE
#endif
#ifdef TERSE
		fputs("Illegal newsgroup, enter a number or 'n'.\n", stdout) FLUSH;
#endif
	    goto reask;
	default:
	    if (isdigit(*buf)) {
		char *s = index(options, *buf);

		i = s ? (s - options) : ngn;
		if (i >= 0 && i < ngn) {
		    char *cp = index(ngptrs[i], ' ');
		    *cp = '\0';
		    set_ngname(ngptrs[i]);
		    *cp = ' ';
		    return 1;
		} /* if */
	    } /* if */
	    fputs(hforhelp, stdout) FLUSH;
	    break;
    } /* switch */

    settle_down();
    goto reask;
} /* get_near_miss */

#endif /* EDIT_DISTANCE */
