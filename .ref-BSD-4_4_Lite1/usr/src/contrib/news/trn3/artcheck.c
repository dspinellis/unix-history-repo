/* $Id: artcheck.c,v 3.0 1992/02/01 03:09:32 davison Trn $
*/

/* A program to check an article's validity and print warnings if problems
** are found.
**
** Usage: artcheck <article> <maxLineLen> <newsgroupsFile>
*/

#include "EXTERN.h"
#include "common.h"

#define MAXNGS 100

int
main(argc, argv)
int argc;
char *argv[];
{
    FILE *fp, *fp2;
    char buff[LBUFLEN], *cp, *cp2;
    char *ngptrs[MAXNGS];
    int nglens[MAXNGS];
    int i, col, max_col_len, line_num = 0, ngcnt = 0;
    int found_newsgroups = 0;

    if (argc != 4 || !(max_col_len = atoi(argv[2]))) {
	fprintf(stderr, "\
Usage: artcheck <article> <maxLineLen> <newsgroupsFile>\n");
	exit(1);
    }

    if ((fp = fopen(argv[1], "r")) == NULL) {
	fprintf(stderr, "artcheck: unable to open article `%s'.\n", argv[1]);
	exit(1);
    }

    /* Check the header for proper format and report on the newsgroups */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	buff[strlen(buff)-1] = '\0';
	if (!*buff)
	    break;
	if (*buff == ' ' || *buff == '\t')
	    continue;
	if (!(cp = index(buff, ':'))) {
	    printf("\nERROR: line %d is an invalid header line:\n%s\n",
		   line_num, buff);
	    break;
	}
	if (cp[1] != ' ') {
	    printf("\n\
ERROR: header on line %d does not have a space after the colon:\n%s\n",
		   line_num, buff);
	}
	if (cp - buff == 10 && strnEQ(buff, "Newsgroups", 10)) {
	    found_newsgroups = 1;
	    for (cp = buff + 11; *cp == ' '; cp++)
		;
	    if (index(cp, ' ')) {
		printf("\n\
ERROR: the \"Newsgroups:\" line has spaces in it that MUST be removed. The\n\
only allowable space is the one separating the colon (:) from the contents.\n\
Use a comma (,) to separate multiple newsgroup names.\n");
		continue;
	    }
	    while (*cp) {
		if (!(cp2 = index(cp, ',')))
		    cp2 = cp + strlen(cp);
		else
		    *cp2++ = '\0';
		if (ngcnt < MAXNGS) {
		    nglens[ngcnt] = strlen(cp);
		    ngptrs[ngcnt] = malloc(nglens[ngcnt]+1);
		    if (!ngptrs[ngcnt]) {
			fprintf(stderr,"Out of memory.\n");
			exit(1);
		    }
		    strcpy(ngptrs[ngcnt], cp);
		    ngcnt++;
		}
		cp = cp2;
	    }
	    if (!ngcnt) {
		printf("\n\
ERROR: the \"Newsgroups:\" line lists no newsgroups.\n");
		continue;
	    }
	}
    }
    if (!found_newsgroups) {
	printf("\nERROR: the \"Newsgroups:\" line is missing from the header.\n");
    }

    /* Check the body of the article for long lines */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	buff[strlen(buff)-1] = '\0';
	col = 0;
	for (cp = buff; *cp; cp++) {
	    if (*cp == '\t')
		col += 8 - (col%8);
	    else
		col++;
	}
	if (col > max_col_len) {
	    printf("\n\
Warning: posting exceeds %d columns.  Line %d is the first long one:\n%s\n",
		   max_col_len, line_num, buff);
	    break;
	}
    }
    if (ngcnt) {
	/* Print a note about each newsgroup */
	printf("\nYour article's newsgroup%s:\n", ngcnt == 1? "" : "s");
	if ((fp2 = fopen(argv[3], "r")) != NULL) {
	    while (fgets(buff, LBUFLEN, fp2)) {
		for (i = 0; i < ngcnt; i++) {
		    if (ngptrs[i]) {
			if ((buff[nglens[i]] == '\t' || buff[nglens[i]] == ' ')
			  && strnEQ(ngptrs[i], buff, nglens[i])) {
			    printf("%s", buff);
			    free(ngptrs[i]);
			    ngptrs[i] = 0;
			}
		    }
		}
	    }
	}
	for (i = 0; i < ngcnt; i++) {
	    if (ngptrs[i]) {
		printf("%s\t[no description available]\n", ngptrs[i]);
		free(ngptrs[i]);
	    }
	}
    }
    return 0;
}
