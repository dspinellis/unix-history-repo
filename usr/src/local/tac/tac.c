#ifndef lint
static char sccsid[] = "@(#)tac.c	1.1 %G%";
#endif

/*
 * tac.c - Print file segments in reverse order
 *
 * Original line-only version by unknown author off the net.
 * 1985 mods by Jay Lepreau, Univ of Utah, to allocate memory dynamically
 * and handle string bounded segments.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

/*
 * This should be defined for BSD releases later than 4.2 and for Sys V.2,
 * at least.  fwrite is faster than putc only if you have a new speedy fwrite.
 */
#define FAST_FWRITE

#ifdef DEBUG				/* dbx can't handle registers */
#include <ctype.h>
# define register
#endif

/* Default target string and bound */
int right = 1;				/* right or left bounded segments? */
char *targ = "\n";

int readsize = 4096;			/* significantly faster than 1024 */
int bufsize;
int targlen;
int numerr;

extern off_t lseek();
extern char *malloc(), *realloc();

main(argc, argv)
    int argc;
    char **argv;
{
    register char *p, *pastend;
    register int firstchar, targm1len;	/* target length minus 1 */
    int fd, i;
    off_t off;
    char *buf;
    struct stat st;

    if (argc < 2) {
	(void) fprintf(stderr, "Usage: tac [-string | +string] file ...\n");
	exit(1);
    }

#ifdef DEBUG
    if (isdigit(*argv[1])) {
	readsize = atoi(argv[1]);
	argc--, argv++;
    }
#endif
    
    if (argv[1][0] == '+' || argv[1][0] == '-') {
	targ = &argv[1][1];
	right = (argv[1][0] == '+');
	argc--; argv++;
    }
    firstchar = *targ;
    targlen = strlen(targ);
    targm1len = targlen - 1;

    bufsize = (readsize << 1) + targlen + 2;
    if ((buf = malloc((unsigned) bufsize)) == NULL) {
	perror("tac: initial malloc");
	exit(1);
    }

    (void) strcpy(buf, targ);		/* stop string at beginning */
    buf += targlen;

    while (--argc) {
	if (stat(p = *++argv, &st) < 0) {
	    prterr(p);
	    numerr++;
	    continue;
	}
	if ((off = st.st_size) == 0)
	    continue;
	if ((fd = open(p, 0)) < 0) {
	    prterr(p);
	    numerr++;
	    continue;
	}

	/*
	 * Arrange for the first read to lop off enough to
	 * leave the rest of the file a multiple of readsize.
	 * Since readsize can change, this may not always hold during
	 * the pgm run, but since it usually will, leave it here
	 * for i/o efficiency (page/sector boundaries and all that).
	 * Note: Above statement has never been verified!
	 */
	if ((i = off % readsize) == 0)
	    i = readsize;
	off -= i;

	(void) lseek(fd, off, 0);
	if (read(fd, buf, i) != i) {
	    prterr(p);
	    (void) close(fd);
	    numerr++;
	    continue;
	}
	p = pastend = buf + i;		/* pastend always points to end+1 */
	p -= targm1len;

	for (;;) {
	    while ( *--p != firstchar ||
	      (targm1len && strncmp(p+1, targ+1, targm1len)) )
		continue;
	    if (p < buf) {		/* backed off front of buffer */
		if (off == 0) {
		    /* beginning of file: dump last segment */
		    output(p + targlen, pastend);
		    (void) close(fd);
		    break;
		}
		if ((i = pastend - buf) > readsize) {
		    char *tbuf;
		    int newbufsize = (readsize << 2) + targlen + 2;
		    
		    if ((tbuf = realloc(buf-targlen, (unsigned) newbufsize)) == NULL) {
			/* If realloc fails, old buf contents may be lost. */
			perror("tac: segment too long; may have garbage here");
			numerr++;
			i = readsize;
		    }
		    else {
			tbuf += targlen;	/* skip over the stop string */
			p += tbuf - buf;
			pastend += tbuf - buf;
			buf = tbuf;
			bufsize = newbufsize;
			readsize = readsize << 1;
			/* guaranteed to fit now (I think!) */
		    }
		}
		if (off - readsize < 0) {
		    readsize = off;
		    off = 0;
		}
		else
		    off -= readsize;
		(void) lseek(fd, off, 0);	/* back up */
		/* Shift pending old data right to make room for new */
		bcopy(buf, p = buf + readsize, i);
		pastend = p + i;
		if (read(fd, buf, readsize) != readsize) {
		    prterr(*argv);
		    numerr++;
		    (void) close(fd);
		    break;
		}
		continue;
	    }
	    /* Found a real instance of the target string */
	    output(right ? p + targlen : p, pastend);
	    pastend = p;
	    p -= targm1len;
	}
    }
    exit(numerr);
}

/*
 * Dump chars from p to pastend-1.  If right-bounded by target
 * and not the first time through, append the target string.
 */
output(p, pastend)
    register char *p;
    char *pastend;
{
    static short first = 1;

#ifdef FAST_FWRITE
    (void) fwrite(p, 1, pastend - p, stdout);
#else
    while (p < pastend)
	(void) putc(*p++, stdout);
#endif
    if (right && !first)
	(void) fwrite(targ, 1, targlen, stdout);
    first = 0;
    if ferror(stdout) {
	perror("tac: fwrite/putc");
	exit(++numerr > 1 ? numerr : 1);
    }
}

prterr(s)
    char *s;
{

    fprintf(stderr, "tac: ");
    perror(s);
}
