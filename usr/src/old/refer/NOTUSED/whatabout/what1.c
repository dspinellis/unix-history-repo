/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)what1.c	4.3 (Berkeley) %G%";
#endif /* not lint */

#include "what..c"

struct filans files[NFILES];
char fnames[NAMES];
int relfeed = 0;
char *rb[NFEED];
char rbb[200], *rbp = rbb;
;

main(argc,argv)
char *argv[];
{
	char *s, *t, *xargv[50], *wd[50];
	struct filans *af;
	int xargc = 0, nw, nf, i;
	while (argc>1 && argv[1][0]=='-')
	{
		switch(argv[1][1])
		{
		case 'r': 
			relfeed=1; 
			break;
		}
		argc--; 
		argv++;
	}
	if (argc<=1)
	{
		printf("No query.\n");
		return(0);
	}
	nf = doclook(argc,argv,0);
	printf("   %d files\n",nf);
	if (relfeed && nf>NFEED)
	{
		wrdoc(NFEED, argc, argv, 1);
		nw = freqwd (rb, wd, argc);
		for(i=0; rb[i]; i++)
			unlink(rb[i]);
	}
	for(i=0; i<argc; i++)
		xargv[xargc++]=argv[i];
	if (relfeed)
	{
		printf("Adding: ");
		for(i=0; i<nw; i++)
			if (!lfind(wd[i], xargc, xargv))
				printf("%s ", xargv[xargc++]=wd[i]);
		printf("\n");
		nf = doclook(xargc, xargv, relfeed? xargc/4 : 0);
		printf("  %d files\n",nf);
	}
	shell (nf, comp, exch);
	wrdoc(nf, xargc, xargv, 0);
	return(0);
}

exch( i1, i2 )
{
	struct filans *p1, *p2;
	struct filans xt;
	p1=files+i1;
	p2=files+i2;
	xt = *p1;
	*p1 = *p2;
	*p2 = xt;
}

comp(i1, i2)
{
	struct filans *p1, *p2;
	p1 = files+i1;
	p2= files+i2;
	if (p1->fdate != p2->fdate)
		return(p2->fdate > p1->fdate);
	return(p2->uid >= p1->uid);
}

wrdoc ( np, argc, argv, relfeed )
{
	struct filans *af;
	char *s, *t, buf[200], *ctime();
	int eval, k, pid;
	FILE *rf = NULL;
	FILE *ans = NULL;
	pid=getpid();
	for(af=files; af<files+np; af++)
	{
		t = ctime(&af->fdate);
		getpw(af->uid, s=buf);
		while (*s && *s!=':') s++;
		*s=0;
		printf("%s (%.20s)  %s, %ld bytes\n",af->nm,t+4,buf,af->size);
		if (relfeed)
		{
			k=af-files;
			_assert (k<NFEED);
			sprintf(rb[k]=rbp, "rf%d.%d",pid, k);
			rf = fopen(rb[k], "w");
			while (*rbp++);
		}
		describe(af->nm, argc,argv, rf);
		if (relfeed)
		{
			printf("You like that one?");
			fflush(stdout);
			fgets(buf, 100, stdin);
			switch(buf[0])
			{
			case 'y': 
			case 'Y': 
				eval=1; 
				break;
			case 'n': 
			case 'N': 
				eval = -1; 
				break;
			default: 
				eval=0; 
				break;
			}
			fclose(rf);
			if (eval<=0)
			{
				unlink(rb[k]);
				rb[k][0]=0;
			}
		}
	}
	if (relfeed) rb[np]=0;
}

lfind( wl, n, wds)
char *wl, *wds[];
{
	int i;
	for(i=0; i<n; i++)
		if (str6cmp(wl, wds[i])==0)
			return(1);
	return(0);
}

str6cmp(s, t)
char *s, *t;
{
	int i = 0, c;
	while ( (c= *s++ ) == ( *t++))
		if (c==0 || ++i ==6)
			return(0);
	return(1);
}
