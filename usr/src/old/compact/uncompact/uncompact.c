#ifndef lint
static char sccsid[] = "@(#)uncompact.c	4.6 (Berkeley) %G%";
#endif

/*
 *  Uncompact adaptive Huffman code input to output
 *
 *  On - line algorithm
 *
 *  Input file does not contain decoding tree
 *
 *  Written by Colin L. Mc Master (UCB) February 14, 1979
 */
#include "compact.h"
#include <strings.h>

union	cio c;
union	cio d;
char	*infname;			/* input file's name */
char	fname[MAXPATHLEN+1];		/* output file's name */
struct	stat status;			/* compacted file status */

int	verbose = 0;

main(argc, argv)
	int argc;
	char *argv[];
{
	register short j;

	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-v") == 0) {
		verbose++;
		argc--, argv++;
	}
	dir[513].next = NULL;
	for (head = dir + (j = 513); j--; ) {
		dirp = head--;
		head->next = dirp;
	}
	bottom = dirp->pt = dict;
	dict[0].top[LEFT] = dict[0].top[RIGHT] = dirp;
	dirq = dirp->next;
	in[EF].flags = FBIT | SEEN;
	if (argc == 0)
		exit(uncompact("-"));
	for (j = 0; j < argc; j++) {
		if (uncompact(argv[j]))
			exit(1);
		if (verbose && argc > 0)
			printf("%s uncompacted to %s\n", argv[j], fname);
	}
	exit(0);
}

uncompact(file)
	char *file;
{
	int ignore;
	FILE *setup();

	bottom->top[1]->next = flist;
	bottom->top[1] = dirp;
	flist = dirq;
	if (strcmp(file, "-") != 0) {
		char *cp;

		strcpy(fname, file);
		cp = rindex(fname, '.');
		if (cp == 0 || strcmp(cp, ".C") != 0) {
			fprintf(stderr,
			    "uncompact: %s: File must have .C suffix.\n", file);
			return;
		}
		*cp = '\0';
		cfp = fopen(file, "r");
		if (cfp == NULL) {
			fprintf(stderr, "uncompact: "), perror(file);
			return;
		}
		(void) fstat(fileno(cfp), &status);
	} else
		cfp = stdin;
	infname = file;
	uncfp = setup(cfp, &ignore);
	if (uncfp == NULL) {
		if (ignore)
			goto done;
		goto bad;
	}
	decompress(cfp, uncfp);
	fflush(uncfp);
	if (ferror(uncfp) || ferror(cfp)) {
		if (uncfp != stdout) {
			if (ferror(uncfp))
				perror(fname);
			else
				perror(infname);
			(void) unlink(fname);
		}
		goto bad;
	}
	if (uncfp != stdout && unlink(infname) < 0)
		fprintf(stderr, "uncompact: "), perror(infname);
done:
	if (uncfp != NULL && uncfp != stdout)
		fclose(uncfp);
	if (cfp != NULL)
		fclose(cfp);
	return (0);
bad:
	fprintf(stderr, "uncompact: ");
	if (strcmp(infname, "-") != 0)
		perror(infname);
	else
		fprintf(stderr,
	    "Unsuccessful uncompact of standard input to standard output.\n");
	return (1);
}

decompress(cfp, uncfp)
	register FILE *cfp, *uncfp;
{
	register struct node *p;
	register short j;
	register int m;
	register struct cio *dp = &d;
	char b;

	p = dict;
	while ((c.integ = getc (cfp)) != EOF) {
		for (m = 0200; m; ) {
			b = (m & c.integ ? 1 : 0);
			m >>= 1;
			if (p->fath.flags & (b ? RLEAF : LLEAF)) {
				dp->integ = p->sp[b].ch;
				if (dp->integ == EF)
					break;
				if (dp->integ == NC) {
					uptree(NC);
					dp->integ = 0;
					for (j = 8; j--; m >>= 1) {
						if (m == 0) {
							c.integ = getc(cfp);
							m = 0200;
						}
						dp->integ <<= 1;
						if (m & c.integ)
							dp->integ++;
					}
					insert(dp->integ);
				}
				uptree(dp->integ);
				putc(dp->chars.lob, uncfp);
				p = dict;
			} else
				p = p->sp[b].p;
		}
	}
}

FILE *
setup(cfp, ignore)
	FILE *cfp;
	int *ignore;
{
	FILE *uncfp = NULL;
	register union cio *dp = &d;
	register union cio *cp = &c;

	dp->integ = getc(cfp);
	if (*ignore = (dp->integ == EOF))
		goto bad;
	cp->integ = getc(cfp);
	if (*ignore = (cp->integ == EOF))
		goto bad;
	dp->chars.hib = cp->integ & 0377;
	if ((dp->integ &= 0177777) != COMPACTED) {
		if (dp->integ == PACKED)
			fprintf(stderr, "%s: File is packed, use unpack.\n",
			    infname);
		else
			fprintf(stderr, "%s: Not a compacted file.\n", infname);
		*ignore = 1;
		goto bad;
	}
	if (strcmp(infname, "-") != 0) {
		uncfp = fopen(fname, "w");
		if (uncfp == NULL) {
			perror(fname);
			goto bad;
		}
		(void) fchmod(fileno(uncfp), status.st_mode);
	} else
		uncfp = stdout;
	cp->integ = getc(cfp);
	if (cp->integ == EOF)
		goto bad;
	putc(cp->chars.lob, uncfp);

	in[NC].fp = in[EF].fp = dict[0].sp[LEFT].p = bottom = dict + 1;
	bottom->count[LEFT] = bottom->count[RIGHT] =
	    dict[0].count[RIGHT] = 1;
	dirp->next = dict[0].top[RIGHT] = bottom->top[LEFT] =
	    bottom->top[RIGHT] = dirq = NEW;
	dirq->next = NULL;
	dict[0].fath.fp = NULL;
	dirq->pt = bottom->fath.fp = in[cp->integ].fp = dict;
	in[cp->integ].flags = (FBIT | SEEN);
	in[NC].flags = SEEN;
	dict[0].fath.flags = RLEAF;
	bottom->fath.flags = (LLEAF | RLEAF);
	dict[0].count[LEFT] = 2;

	dict[0].sp[RIGHT].ch = cp->integ;
	bottom->sp[LEFT].ch = NC;
	bottom->sp[RIGHT].ch = EF;
	return (uncfp);
bad:
	if (uncfp && uncfp != stdout) {
		perror(fname);
		(void) unlink(fname);
		fclose(uncfp);
	}
	return (NULL);
}
