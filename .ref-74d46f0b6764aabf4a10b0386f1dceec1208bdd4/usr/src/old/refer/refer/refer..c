/*	refer..c	4.4	87/10/22	*/

#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#define FLAG 003
#define AFLAG 007
#define NRFTXT 10000
#define NRFTBL 500
#define NTFILE 20
#define QLEN 512
#define ANSLEN 1024
#define TAGLEN 400
#define NSERCH 20
#define MXSIG 200		/* max bytes in aggregate signal */

extern FILE *in;
extern int endpush, sort, labels, keywant, bare;
extern int biblio, science, postpunct;
extern char *smallcaps;
extern char comname;
extern char *keystr;
extern char *convert;
extern int authrev;
extern int nmlen, dtlen;
extern char *rdata[], **search;
extern int refnum;
extern char *reftable[];
extern char *rtp, reftext[];
extern int sep;
extern char tfile[];
extern char gfile[];
extern char ofile[];
extern char hidenam[];
extern char *Ifile; extern int Iline;
extern FILE *fo, *ftemp;
extern char *input(),*lookat();
extern char *class(),*caps(),*revauth();
extern char *artskp(),*fpar();
extern char *trimnl();

extern char *getenv(), *strcpy(), *strcat();
