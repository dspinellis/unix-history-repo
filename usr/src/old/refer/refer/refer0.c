/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)refer0.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include "refer..c"

FILE *in = stdin;
FILE *fo = stdout;
FILE *ftemp = stdout;
int endpush = 0;
int sort = 0;
int labels = 0;
int keywant = 0;
int bare = 0;
int biblio = 0;
int science = 0;
int postpunct = 0;
int authrev = 0;
char *smallcaps = "";
char *keystr = "AD";
char *convert = "X.AP";
int nmlen = 0, dtlen = 0;
char *rdata[NSERCH];
char **search = rdata;
int refnum = 0;
char reftext[NRFTXT];
char *reftable[NRFTBL];
char *rtp = reftext;
int sep = '\n';
char tfile[NTFILE];
char ofile[NTFILE];
char gfile[NTFILE];
char hidenam[NTFILE];
char *Ifile = "standard input";
int Iline = 0;
