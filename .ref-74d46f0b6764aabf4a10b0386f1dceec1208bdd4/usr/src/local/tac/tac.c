#ifndef lint
static char sccsid[] = "@(#)tac.c	1.5 %G%";
#endif

/*
 * tac.c - Print file segments in reverse order
 *
 * Original line-only version by unknown author off the net.
 * Rewritten in 1985 by Jay Lepreau, Univ of Utah, to allocate memory
 * dynamically, handle string bounded segments (suggested by Rob Pike),
 * and handle pipes.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <signal.h>

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

char *tfile;
char *buf;

int readsize = 4096;			/* significantly faster than 1024 */
int bufsize;
int targlen;
int numerr;

int cleanup();
extern off_t lseek();
extern char *strcpy(), *malloc(), *realloc(), *mktemp();

main(argc, argv)
    int argc;
    char **argv;
{

#ifdef DEBUG
    if (argc > 1 && isdigit(*argv[1])) {
	readsize = atoi(argv[1]);
	argc--, argv++;
    }
#endif
    
    if (argc > 1 && (argv[1][0] == '+' || argv[1][0] == '-') && argv[1][1]) {
	targ = &argv[1][1];
	right = (argv[1][0] == '+');
	argc--; argv++;
    }
    targlen = strlen(targ);

    bufsize = (readsize << 1) + targlen + 2;
    if ((buf = malloc((unsigned) bufsize)) == NULL) {
	perror("tac: initial malloc");
	exit(1);
    }

    (void) strcpy(buf, targ);		/* stop string at beginning */
    buf += targlen;

    if (argc == 1)
	tacstdin();
    while (--argc) {
	if (strcmp(*++argv, "-") == 0)
	    tacstdin();
	else
	    tacit(*argv);
    }
    exit(numerr > 0 ? 1 : 0);
}

tacstdin()
{

    void (*sigint)(), (*sighup)(), (*sigterm)();

    if ((sigint = signal(SIGINT, SIG_IGN)) != SIG_IGN)
	(void) signal(SIGINT, cleanup);
    if ((sighup = signal(SIGHUP, SIG_IGN)) != SIG_IGN)
	(void) signal(SIGHUP, cleanup);
    if ((sigterm = signal(SIGTERM, SIG_IGN)) != SIG_IGN)
	(void) signal(SIGTERM, cleanup);

    savestdin();
    tacit(tfile);
    (void) unlink(tfile);

    (void) signal(SIGINT, sigint);
    (void) signal(SIGHUP, sighup);
    (void) signal(SIGTERM, sigterm);
}

char template[] = "/tmp/tacXXXXXX";
char workplate[sizeof template];

savestdin()
{
    int fd;
    register int n;

    (void) strcpy(workplate, template);
    tfile = mktemp(workplate);
    if ((fd = creat(tfile, 0600)) < 0) {
	prterr(tfile);
	cleanup();
    }
    while ((n = read(0, buf, readsize)) > 0)
	if (write(fd, buf, n) != n) {
	    prterr(tfile);
	    cleanup();
	}
    (void) close(fd);
    if (n < 0) {
	prterr("stdin read");
	cleanup();
    }
}

tacit(name)
    char *name;
{
    register char *p, *pastend;
    register int firstchar, targm1len;	/* target length minus 1 */
    struct stat st;
    off_t off;
    int fd, i;

    firstchar = *targ;
    targm1len = targlen - 1;

    if (stat(name, &st) < 0) {
	prterr(name);
	numerr++;
	return;
    }
    if ((off = st.st_size) == 0)
	return;
    if ((fd = open(name, 0)) < 0) {
	prterr(name);
	numerr++;
	return;
    }

    /*
     * Arrange for the first read to lop off enough to
     * leave the rest of the file a multiple of readsize.
     * Since readsize can change, this may not always hold during
     * the pgm run, but since it usually will, leave it here
     * for i/o efficiency (page/sector boundaries and all that).
     * Note: the efficiency gain has not been verified.
     */
    if ((i = off % readsize) == 0)
	i = readsize;
    off -= i;

    (void) lseek(fd, off, 0);
    if (read(fd, buf, i) != i) {
	prterr(name);
	(void) close(fd);
	numerr++;
	return;
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
		prterr(name);
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

cleanup()
{

    (void) unlink(tfile);
    exit(1);
}
