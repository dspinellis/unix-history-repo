#if	defined(__TURBOC__) || defined(__STDC__) || defined(LATTICE)
#define	ANSIPROTO	1
#endif

#ifndef	__ARGS
#ifdef	ANSIPROTO
#define	__ARGS(x)	x
#else
#define	__ARGS(x)	()
#endif
#endif

/* Server-side data structure for reading keys file during login */
struct skey {
	FILE *keyfile;
	char buf[256];
	char *logname;
	int n;
	char *seed;
	char *val;
	long	recstart; /*needed so reread of buffer is efficient*/


};

/* Client-side structure for scanning data stream for challenge */
struct mc {
	char buf[256];
	int skip;
	int cnt;
};

void f __ARGS((char *x));
int keycrunch __ARGS((char *result,char *seed,char *passwd));
char *btoe __ARGS((char *engout,char *c));
char *put8 __ARGS((char *out,char *s));
int etob __ARGS((char *out,char *e));
void rip __ARGS((char *buf));
int skeychallenge __ARGS((struct skey *mp,char *name, char *challenge));
int skeylookup __ARGS((struct skey *mp,char *name));
int skeyverify __ARGS((struct skey *mp,char *response));
int skeyverify __ARGS((struct skey *mp,char *response));
