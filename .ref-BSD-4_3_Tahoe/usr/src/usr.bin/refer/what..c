/*	what..c	4.2	83/08/11	*/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#define NFILES 100
#define NAMES 2000
#define NFEED 5
extern exch(), comp();
struct filans {
	char *nm;
	long fdate;
	long size;
	int uid;
	};
extern struct filans files[NFILES];
extern char fnames[NAMES];
