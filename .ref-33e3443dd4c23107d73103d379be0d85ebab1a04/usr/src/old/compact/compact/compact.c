#ifndef lint
static char sccsid[] = "@(#)compact.c	4.7 (Berkeley) %G%";
#endif

/*
 *  Adaptive Huffman code input to output
 *
 *  On - line algorithm
 *
 *  Does not prepend decoding tree
 *
 *  Written by Colin L. Mc Master (UCB) February 28, 1979
 */
#include <strings.h>
#include "compact.h"

union	cio d;
union	cio c;
int	bits;
char	*infname;			/* input file's name */
char	fname[MAXPATHLEN+1];		/* output file's name */
struct	stat ucfstatus;			/* uncompacted file status */

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
		exit(compact("-"));
	for (j = 0; j < argc; j++) {
		if (verbose && argc > 0)
			printf("%s: ", argv[j]);
		if (compact(argv[j]))
			exit(1);
	}
	exit(0);
}

/*
 * Compact a single file ("-" implies stdin).
 */
compact(file)
	char *file;
{
	int j, ignore;
	FILE *setup();

	for (j = 256; j--; )
		in[j].flags = 0;
	bottom->sons[RIGHT].top->next = flist;
	bottom->sons[RIGHT].top = dirp;
	flist = dirq;
	cfp = uncfp = NULL;
	if (strcmp(file, "-") != 0) {
		char *cp, *tp;

		/* verify .C suffix fits */
		cp = rindex(file, '/');
		if (cp == 0)
			cp = file;
		else
			cp++;
		tp = index(cp, '\0');
		if (tp - cp > MAXNAMLEN || strlen(file) + 2 >= MAXPATHLEN) {
			fprintf(stderr, "%s: File name too long\n", file);
			goto bad;
		}
		uncfp = fopen(file, "r");
		if (uncfp == NULL) {
			fprintf(stderr, "compact: "), perror(file);
			goto bad;
		}
		fstat(fileno(uncfp), &ucfstatus);
		if ((ucfstatus.st_mode & S_IFMT) != S_IFREG) {
			fprintf(stderr, "%s: Not a regular file.\n", file);
			goto bad;
		}
	} else
		uncfp = stdin;
	infname = file;

	cfp = setup(uncfp, &ignore);
	if (cfp == NULL) {
		if (ignore)
			goto done;
		goto bad;
	}
	if (compress(uncfp, cfp)) {
		if (cfp != stdout)
			(void) unlink(fname);
		goto bad2;
	}
	encode(EF);
	if (bits) {
		d.integ <<= 16 - bits;
		putc(d.chars.hib, cfp);
		if (bits > 8)
			putc(d.chars.lob, cfp);
		bits = 0;
	}
	fflush(cfp);
	if (ferror(uncfp) || ferror(cfp)) {
		if (cfp != stdout) {
			fprintf(stderr, "compact: ");
			if (ferror(cfp))
				perror(fname);
			else
				perror(infname);
			(void) unlink(fname);
		}
		goto bad;
	}
	if (cfp != stdout) {
		struct stat cfstatus;
		longint csize, ucsize;

		(void) fstat(fileno(cfp), &cfstatus);
		csize = cfstatus.st_size;
		ucsize = ucfstatus.st_size;
		if (csize >= ucsize) {
			fprintf("%s: ", infname);
			(void) unlink(fname);
			printf("Not compacted, does not save bytes.\n");
			goto done;
		}
		if (verbose) {
			FILE *fd;
			longint n, m;

			while (ucsize - csize > 21474) {
				ucsize /= 10;
				csize /= 10;
			}
			n = 100000 * (ucsize - csize) / ucsize + 5;
			m = (n % 1000) / 10;
			bits = m % 10 + '0';
			c.integ = m / 10 + '0';
			printf("%ld.%c%c%% compression\n",
			    n / 1000, c.chars.lob, bits);
		}
		if (unlink(infname) < 0)
			fprintf(stderr, "compact: "), perror(infname);
	}
done:
	if (cfp != NULL && cfp != stdout)
		fclose(cfp);
	if (uncfp != NULL)
		fclose(uncfp);
	return (0);
bad2:
	fprintf(stderr, "compact: ");
	if (cfp != stdout)
		perror(infname);
	else
		fprintf(stderr,
	    "Unsuccessful compact of standard input to standard output.\n");
bad:
	if (cfp != NULL && cfp != stdout)
		fclose(cfp);
	if (uncfp != NULL)
		fclose(uncfp);
	return (1);
}

encode(ch)
	int ch;
{
	register struct node *pp;
	int stack[17];
	register int stbits = 1, *sp = &stack[0], rbits = bits;
	register union cio *dp = &d;
	union cio c;

	c.integ = ch;
	*sp = in[ch].flags & FBIT;
	pp = in[ch].fp;

	while (pp->fath.fp) {
		*sp <<= 1;
		if (pp->fath.flags & FBIT)
			(*sp)++;
		stbits++;
		if ((stbits &= 017) == 0)
			sp++;
		pp = pp->fath.fp;
	}

	/* pop the output stack */
	do {
		while (stbits-- > 0) {
			dp->integ <<= 1;
			if (*sp & 01)
				dp->integ++;
			++rbits;
			if ((rbits &= 017) == 0) {
				putc(dp->chars.hib, cfp);
				putc(dp->chars.lob, cfp);
				if (ferror(cfp))
					goto done;
			}
			*sp >>= 1;
		}
		stbits = 16;
	} while (--sp >= &stack[0]);
done:
	bits = rbits;
}

compress(uncfp, cfp)
	register FILE *uncfp, *cfp;
{
	register union cio *dp = &d;
	register union cio *cp = &c;

	cp->integ = (dp->integ >> 8) & 0377;
	for (; cp->integ != EOF; cp->integ = getc(uncfp)) {
		if ((in[cp->integ].flags & SEEN) == 0) {
			register short j, m;

			encode(NC);
			uptree(NC);
			insert(cp->integ);

			m = 0200;
			for (j = 8; j--; m >>= 1) {
				dp->integ <<= 1;
				if (m & cp->integ)
					dp->integ++;
				++bits;
				if ((bits &= 017) == 0) {
					putc(dp->chars.hib, cfp);
					putc(dp->chars.lob, cfp);
				}
			}
		} else
			encode(cp->integ);
		if (ferror(cfp)) {
			perror(fname);
			return (1);
		}
		uptree(cp->integ);
	}
	if (ferror(uncfp)) {
		perror(infname);
		return (1);
	}
	return (0);
}

FILE *
setup(uncfp, ignore)
	FILE *uncfp;
	int *ignore;
{
	FILE *cfp = NULL;
	register union cio *dp = &d;
	register union cio *cp = &c;

	dp->integ = getc(uncfp);
	if (*ignore = (dp->integ == EOF))
		goto bad;
	cp->integ = getc(uncfp);
	if (*ignore = (cp->integ == EOF))
		goto bad;
	dp->chars.hib = cp->integ & 0377;
	if ((dp->integ &= 0177777) == COMPACTED) {
		fprintf(stderr, "%s: Already compacted.\n", infname);
		*ignore = 1;
		goto bad;
	}
	if (dp->integ == PACKED) {
		fprintf(stderr,
		    "%s: Already packed using pack program, use unpack.\n",
		    infname);
		*ignore = 1;
		goto bad;
	}
	if (strcmp(infname, "-") != 0) {
		sprintf(fname, "%s.C", infname);
		cfp = fopen(fname, "w");
		if (cfp == NULL)
			goto bad2;
		(void) fchmod(fileno(cfp), ucfstatus.st_mode);
	} else
		cfp = stdout;
	cp->integ = COMPACTED;
	putc(cp->chars.lob, cfp);
	putc(cp->chars.hib, cfp);
	if (ferror(cfp))
		goto bad2;
	bits = 8;
	cp->integ = dp->integ & 0377;

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
	return (cfp);
bad2:
	if (cfp && cfp != stdout) {
		fprintf(stderr, "compact: "), perror(fname);
		(void) unlink(fname);
	}
bad:
	if (cfp && cfp != stdout)
		fclose(cfp);
	return (NULL);
}
