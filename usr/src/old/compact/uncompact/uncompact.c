#ifndef lint
static char sccsid[] = "@(#)uncompact.c	4.8 (Berkeley) 12/21/87";
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
	dict[0].sons[LEFT].top = dict[0].sons[RIGHT].top = dirp;
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

	bottom->sons[RIGHT].top->next = flist;
	bottom->sons[RIGHT].top = dirp;
	flist = dirq;
	uncfp = cfp = NULL;
	if (strcmp(file, "-") != 0) {
		char *cp;

		strcpy(fname, file);
		cp = rindex(fname, '.');
		if (cp == 0 || strcmp(cp, ".C") != 0) {
			fprintf(stderr,
			    "uncompact: %s: File must have .C suffix.\n", file);
			goto bad;
		}
		*cp = '\0';
		cfp = fopen(file, "r");
		if (cfp == NULL) {
			fprintf(stderr, "uncompact: "), perror(file);
			goto bad;
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
		fprintf(stderr, "uncompact: ");
		if (uncfp != stdout) {
			if (ferror(uncfp))
				perror(fname);
			else
				perror(infname);
			(void) unlink(fname);
		} else
			fprintf(stderr,
	    "Unsuccessful uncompact of standard input to standard output.\n");
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
	if (cfp != NULL)
		fclose(cfp);
	return (1);
}

decompress(cfp, uncfp)
	register FILE *cfp, *uncfp;
{
	register struct node *p;
	register short j;
	register int m;
	register union cio *dp = &d;
	char b;

	p = dict;
	while ((c.integ = getc (cfp)) != EOF) {
		for (m = 0200; m; ) {
			b = (m & c.integ ? 1 : 0);
			m >>= 1;
			if (p->fath.flags & (b ? RLEAF : LLEAF)) {
				dp->integ = p->sons[b].sp.ch;
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
				p = p->sons[b].sp.p;
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

	*ignore = 0;
	dp->integ = getc(cfp);
	if (dp->integ != EOF) {	
		cp->integ = getc(cfp);
		if (cp->integ != EOF)
			dp->chars.hib = cp->integ & 0377;
	} else
		dp->integ = 0;
	if ((dp->integ &= 0177777) != COMPACTED) {
		fprintf(stderr, "uncompact: ");
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
		if (uncfp == NULL)
			goto bad2;
		(void) fchmod(fileno(uncfp), status.st_mode);
	} else
		uncfp = stdout;
	cp->integ = getc(cfp);
	if (cp->integ == EOF)
		goto bad2;
	putc(cp->chars.lob, uncfp);

	in[NC].fp = in[EF].fp = dict[0].sons[LEFT].sp.p = bottom = dict + 1;
	bottom->sons[LEFT].count = bottom->sons[RIGHT].count =
	    dict[0].sons[RIGHT].count = 1;
	dirp->next = dict[0].sons[RIGHT].top = bottom->sons[LEFT].top =
	    bottom->sons[RIGHT].top = dirq = NEW;
	dirq->next = NULL;
	dict[0].fath.fp = NULL;
	dirq->pt = bottom->fath.fp = in[cp->integ].fp = dict;
	in[cp->integ].flags = (FBIT | SEEN);
	in[NC].flags = SEEN;
	dict[0].fath.flags = RLEAF;
	bottom->fath.flags = (LLEAF | RLEAF);
	dict[0].sons[LEFT].count = 2;

	dict[0].sons[RIGHT].sp.ch = cp->integ;
	bottom->sons[LEFT].sp.ch = NC;
	bottom->sons[RIGHT].sp.ch = EF;
	return (uncfp);
bad2:
	fprintf(stderr, "uncompact: ");
	perror(fname);
bad:
	if (uncfp && uncfp != stdout) {
		(void) unlink(fname);
		fclose(uncfp);
	}
	return (NULL);
}
