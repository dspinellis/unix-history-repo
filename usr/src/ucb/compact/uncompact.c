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


main (argc, argv)
short argc;
char *argv [ ];
{
	register short i;
	register struct node *p;
	register short j;
	register int m;
	union cio c, d;
	char b;
	longint ic, n;
	char fname [LNAME], *cp;

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
		ic = oc = 0;
		(bottom -> top [1]) -> next = flist;
		bottom -> top [1] = dirp;
		flist = dirq;
		if (i >= argc) {
			uncfp = stdout;
			cfp = stdin;
		}
		else {
			m = -1;
			cp = fname;
			for (j = 0; j < (LNAME - 3) && (*cp = argv [i][j]); j++)
				if (*cp++ == '/') m = j;
			if (cp [-1] == 'C' && cp [-2] == '.') cp [-2] = 0;
			else {
				fprintf (stderr, "%s: File name must end with \".C\"\n", argv [i]);
				if (i == argc - 1) break;
				continue;
			}
			if (j >= (LNAME - 3) || (j - m) > 15) {
				fprintf (stderr, "File name too long -- %s\n", argv [i]);
				if (i == argc - 1) break;
				continue;
			}
			if ((cfp = fopen (argv [i], "r")) == NULL) {
				perror (argv [i]);
				if (i == argc - 1) break;
				continue;
			}
			if ((uncfp = fopen (fname, "w")) == NULL) {
				perror (fname);
				fclose (cfp);
				if (i == argc - 1) break;
				continue;
			}
			fstat (fileno (cfp), &status);
			chmod (fname, status.st_mode & 07777);
		}

		if ((c . integ = getc (cfp)) != EOF) {
			if ((d . integ = getc (cfp)) != EOF) {
				c . chars . hib = d . integ & 0377;
				c . integ &= 0177777;
				if (c . integ != COMPACTED) goto notcompact;
				if ((c . integ = getc (cfp)) != EOF) {
					putc (c . chars . lob, uncfp);
					ic = 3;
		
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
		
					p = dict;
					while ((c . integ = getc (cfp)) != EOF) {
						ic++;
						for (m = 0200; m; ) {
							b = (m & c . integ ? 1 : 0);
							m >>= 1;
							if (p -> fath . flags & (b ? RLEAF : LLEAF)) {
								d . integ = p -> sp [b] . ch;
								if (d . integ == EF) break;
								if (d . integ == NC) {
									uptree (NC);
									d . integ = 0;
									for (j = 8; j--; m >>= 1) {
										if (m == 0) {
											c . integ = getc (cfp);
											ic++;
											m = 0200;
										}
										d . integ <<= 1;
										if (m & c . integ) d . integ++;
									}
									insert (d . integ);
								}
								uptree (d . integ);
								putc (d . chars . lob, uncfp);
								oc++;
								p = dict;
							}
							else p = p -> sp [b] . p;
						}
					}
				}
			}
			else goto notcompact;
		}
		else {
			notcompact : if (i < argc) {
					     fprintf (stderr, "%s: ", argv [i]);
					     unlink (fname);
				     }
				     if (c . integ == PACKED) fprintf (stderr, "File is packed. Use unpack.\n");
				     else fprintf (stderr, "Not a compacted file.\n");
				     if (i >= argc) break;
				     goto closeboth;
		}

		if (ferror (uncfp) || ferror (cfp))
			if (i < argc) {
				if (ferror (uncfp))
					perror (fname);
				else
					perror (argv [i]);
				fprintf (stderr, "Unable to uncompact %s\n", argv [i]);
				unlink (fname);
				goto closeboth;
			}
			    if (i >= argc) break;
			    fprintf (stderr, "%s uncompacted to %s\n", argv [i], fname);
			    unlink (argv [i]);
		closeboth : fclose (cfp);
		closein   : fclose (uncfp);
			    if (i == argc - 1) break;
			    for (j = 256; j--; ) in [j] . flags = 0;
			    continue;
		fail 	  : fprintf (stderr, "Unsuccessful uncompact of standard input to standard output.\n");
		            break;
	}
}
