/*
 *  Adaptive Huffman code input to output
 *
 *  On - line algorithm
 *
 *  Does not prepend decoding tree
 *
 *  Written by Colin L. Mc Master (UCB) February 28, 1979
 */


#include "compact.h"

union cio d;
int bits;


main (argc, argv)
short argc;
char *argv [ ];
{
	register short i, j;
	register int m;
	union cio c;
	short l;
	longint ic, n;
	char *cp, fname [LNAME];

	dir [513] . next = NULL;
	for (head = dir + (j = 513); j--; ) {
		dirp = head--;
		head -> next = dirp;
	}
	bottom = dirp -> pt = dict;
	dict [0] . top [0] = dict [0] . top [1] = dirp;
	dirq = dirp -> next;
	in [EF] . flags = FBIT | SEEN;

	for (i = 1; ; i++) {
		ic = oc =  0;
		(bottom -> top [1]) -> next = flist;
		bottom -> top [1] = dirp;
		flist = dirq;
		if (i >= argc) {
			uncfp = stdin;
			cfp = stdout;
		}
		else {
			m = -1;
			cp = fname;
			for (l = 0; l < (LNAME - 3) && (*cp = argv [i][l]); l++)
				if (*cp++ == '/') m = l;
			if (l >= (LNAME - 3) || (l - m) > 13) {
				fprintf (stderr, "%s: File name too long\n", argv [i]);
				if (i == argc - 1) break;
				continue;
			}
			if ((uncfp = fopen (argv [i], "r")) == NULL) {
				perror (argv [i]);
				if (i == argc - 1) break;
				continue;
			}
		}

		fstat (fileno (uncfp), &status);
		if ((status.st_mode & 040000) == 040000) {
			fprintf (stderr, "%s: Can't compact a directory\n", argv [i]);
			if (i < argc) goto closein;
			break;
		}

		if ((d . integ = getc (uncfp)) != EOF) {
			ic++;
			if ((c . integ = getc (uncfp)) != EOF) {
				d . chars . hib = c . integ & 0377;
				if ((d . integ &= 0177777) == COMPACTED) {
					fprintf (stderr, "%s: Already compacted.\n", argv [i]);
					if (i < argc) goto closein;
					break;
				}
				if (d . integ == PACKED) {
					fprintf (stderr, "%s: Already packed using program pack.  Use unpack.\n", argv [i]);
					if (i < argc) goto closein;
					break;
				}
				if (i < argc) {
					*cp++ = '.'; *cp++ = 'C'; *cp = '\0';
					if ((cfp = fopen (fname, "w")) == NULL) {
						perror (fname);
						goto closein;
					}
					chmod (fname, status.st_mode);
				}
				c . integ = COMPACTED;
				putc (c . chars . lob, cfp);
				putc (c . chars . hib, cfp);
				if (ferror (cfp))
					if (i < argc) {
						perror (fname);
						unlink (fname);
						goto closeboth;
					}
					else goto fail;
				bits = 8;
				oc = 2;
				c . integ = d . integ & 0377;
	
				in [NC] . fp = in [EF] . fp = dict [0] . sp [0] . p = bottom = dict + 1;
				bottom -> count [0] = bottom -> count [1] = dict [0] . count [1] = 1;
				dirp -> next = dict [0] . top [1] = bottom -> top [0] = bottom -> top [1] = dirq = NEW;
				dirq -> next = NULL;
				dict [0] . fath . fp = NULL;
				dirq -> pt = bottom -> fath . fp = in [c . integ] . fp = dict;
				in [c . integ] . flags = (FBIT | SEEN);
				in [NC] . flags = SEEN;
				dict [0] . fath . flags = RLEAF;
				bottom -> fath . flags = (LLEAF | RLEAF);
				dict [0] . count [0] = 2;
	
				dict [0] . sp [1] . ch = c . integ;
				bottom -> sp [0] . ch = NC;
				bottom -> sp [1] . ch = EF;
	
				for (c . integ = ((d . integ >> 8) & 0377); c . integ != EOF; c . integ = getc (uncfp)) {
					ic++;
					if (in [c . integ] . flags & SEEN) encode (c . integ);
					else {
						encode (NC);
						uptree (NC);
						insert (c . integ);
	
						m = 0200;
						for (j = 8; j--; m >>= 1) {
							d . integ <<= 1;
							if (m & c . integ) d . integ++;
							++bits;
							if ((bits &= 017) == 0) {
								putc (d . chars . hib, cfp);
								putc (d . chars . lob, cfp);
								oc += 2;
							}
						}
					}
					if (ferror (cfp))
						if (i < argc) {
							perror (fname);
							unlink (fname);
							goto closeboth;
						}
						else goto fail;
					uptree (c . integ);
	
				}

				if (ferror (uncfp))
					if (i < argc) {
						perror (argv [i]);
						unlink (fname);
						goto closeboth;
					}
					else goto fail;
	
				encode (EF);
	
				if (bits) {
					d . integ <<= (16 - bits);
					oc++;
					putc (d . chars . hib, cfp);
					if (bits > 8) {
						oc++;
						putc (d . chars . lob, cfp);
					}
					bits = 0;
				}
			}
			else oc = ic;
		}

		if (ferror (uncfp) || ferror (cfp))
			if (i < argc) {
				if (ferror (cfp))
					perror (fname);
				else
					perror (argv [i]);
				if (oc > 1) {
					unlink (fname);
					goto closeboth;
				}
				goto closein;
			}
			else goto fail;
		else {
			if (oc >= ic) {
				if (i < argc) fprintf (stderr, "%s: ", argv [i]);
				if (i < argc) fprintf (stderr, "Not compacted.  ");
				fprintf (stderr, "Does not save bytes.\n");
				if (i < argc) {
					if (oc > 1) {
						unlink (fname);
						goto closeboth;
					}
					goto closein;
				}
				else break;
			}
			while ((ic - oc) > 21474) {
				ic /= 10;
				oc /= 10;
			}
			n = 100000 * (ic - oc) / ic + 5;
			m = (n % 1000) / 10;
			bits = m % 10 + '0';
			c . integ = m / 10 + '0';
			if (i < argc) fprintf (stderr, "%s:  ", argv [i]);
			fprintf (stderr, "Compression : %4ld.%c%c%%\n", n / 1000, c . chars . lob, bits);
		}

			    if (i >= argc) break;
			    unlink (argv [i]);
		closeboth : fclose (cfp);
		closein   : fclose (uncfp);
			    if (i == argc - 1) break;
			    for (j = 256; j--; ) in [j] . flags = 0;
			    continue;
		fail 	  : fprintf (stderr, "Unsuccessful compact of standard input to standard output.\n");
		            break;
	}
}

encode (ch)
int ch;
{

	register struct node *pp;
	register char j;
	union cio c;
	int stack [17], stp, stbits;

	c . integ = ch;
	stack [stp = 0] = in [c . integ] . flags & FBIT;
	stbits = 1;
	pp = in [c . integ] . fp;

	while (pp -> fath . fp) {
		stack [stp] <<= 1;
		if (pp -> fath . flags & FBIT) stack [stp]++;
		stbits++;
		if ((stbits &= 017) == 0) stp++;
		pp = pp ->  fath . fp;
	}

	/* pop the output stack */

	for (stp++; stp--; ) {
		for (j = 0; j < stbits; j++) {
			d . integ <<= 1;
			if (stack [stp] & 01) d . integ++;
			++bits;
			if ((bits &= 017) == 0) {
				putc (d . chars . hib, cfp);
				putc (d . chars . lob, cfp);
				if (ferror (cfp)) return;
				oc += 2;
			}
			stack [stp] >>= 1;
		}
		stbits = 16;
	}
}
