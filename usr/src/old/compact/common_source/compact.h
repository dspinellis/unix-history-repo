/*	compact.h	4.2	83/02/23	*/

#define VAX 11/780


#if defined(vax) || defined(sun)
typedef int longint;
#else
typedef long longint;
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

#define LNAME 80
#define NEW flist; flist = flist -> next
#define LLEAF 010
#define RLEAF 04
#define SEEN 02
#define FBIT 01
#define COMPACTED 017777
#define PACKED 017437
#define EF 0400
#define NC 0401

struct charac {
#if defined(vax) || defined(pdp11)
	char lob, hib;
#else
	char hib, lob;
#endif
};

union cio {
	struct charac chars;
	short integ;
};

struct fpoint {
	struct node *fp;
	int flags;
} in [258];

struct index {
	struct node *pt;
	struct index *next;
} dir [514], *head, *flist, *dirp, *dirq;

union treep {
	struct node *p;
	int ch;
};

struct node {
	struct fpoint fath;
	union treep sp [2];
	struct index *top [2];
	longint count [2];
} dict [258], *bottom;

longint oc;

FILE *cfp, *uncfp;

struct stat status;
