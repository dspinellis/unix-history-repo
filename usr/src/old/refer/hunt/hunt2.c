#ifndef lint
static char *sccsid = "@(#)hunt2.c	4.4 (Berkeley) %G%";
#endif

#include "refer..c"

static int *coord = 0;
int hh[50]; 
extern int *hfreq, hfrflg, hcomp(), hexch();
extern int prfreqs;

doquery(hpt, nhash, fb, nitem, qitem, master)
long *hpt;
FILE *fb;
char *qitem[];
union ptr {
	unsigned *a; 
	long *b;
} master;
{
	long k;
	union ptr prevdrop;
	int nf = 0, best = 0, nterm = 0, i, g, j;
	int *prevcoord;
	long lp;
	extern int lmaster, colevel, reached;
	long getl(); 
	extern int iflong;

# if D1
	fprintf(stderr, "entering doquery nitem %d\n",nitem);
	fprintf(stderr, "first few hashes are %ld %ld %ld %ld %ld\n", hpt[0],hpt[1],hpt[2],hpt[3],hpt[4]);
	fprintf(stderr, "and frequencies are  %d %d %d %d %d\n",hfreq[0],hfreq[1],hfreq[2],hfreq[3],hfreq[4]);
# endif
	_assert (lmaster>0);
	if (coord==0)
		coord = (int *) zalloc(lmaster, sizeof(lmaster));
	if (colevel>0)
	{
		if (iflong)
			prevdrop.b = (long *) zalloc(lmaster, sizeof(long));
		else
			prevdrop.a = (unsigned *) zalloc(lmaster, sizeof(int));
		prevcoord = (int *) zalloc(lmaster, sizeof(lmaster));
	}
	else
	{
		prevdrop.a=master.a;
		prevcoord=coord;
	}
# if D1
	fprintf(stderr, "nitem %d\n",nitem);
# endif
	for(i=0; i<nitem; i++)
	{
		hh[i] = hash(qitem[i])%nhash;
# if D1
		fprintf(stderr,"query wd X%sX has hash %d\n", qitem[i], hh[i]);
# endif
	}
# if D1
	fprintf(stderr, "past that loop nhash %d hpt is %lo\n", nhash, hpt);
# endif
	if (prfreqs)
		for(i=0; i<nitem; i++)
			fprintf(stderr,"item %s hash %d hfreq %d\n",qitem[i], hh[i], hfreq[hh[i]]);
	/* if possible, sort query into decreasing frequency of hashes */
	if (hfrflg)
		shell (nitem, hcomp, hexch);
# if D1
	for(i=0; i<nitem; i++)
		fprintf(stderr, "item hash %d frq %d\n", hh[i], hfreq[hh[i]]);
# endif
	lp = hpt [hh[0]];
# if D1
	fprintf(stderr,"first item hash %d lp %ld 0%lo\n", hh[0],lp,lp);
# endif
	_assert (fb!=NULL);
	_assert (fseek(fb, lp, 0) != -1);
	for(i=0; i<lmaster; i++)
	{
		if (iflong)
			master.b[i] = getl(fb);
		else
			master.a[i] = getw(fb);
		coord[i]=1;
# if D2
		if (iflong)
			fprintf(stderr,"master has %ld\n",(master.b[i]));
		else
			fprintf(stderr,"master has %d\n",(master.a[i]));
# endif
		_assert (i<lmaster);
		if (iflong)
		{
			if (master.b[i] == -1L) break;
		}
		else
		{
			if (master.a[i] == -1) break;
		}
	}
	nf= i;
	for(nterm=1; nterm<nitem; nterm++)
	{
# ifdef D1
		fprintf(stderr, "item %d, hash %d\n", nterm, hh[nterm]);
# endif
		if (colevel>0)
		{
			for(j=0; j<nf; j++)
			{
				if (iflong)
					prevdrop.b[j] = master.b[j];
				else
					prevdrop.a[j] = master.a[j];
				prevcoord[j] = coord[j];
			}
		}
		lp = hpt[hh[nterm]];
		_assert (fseek(fb, lp, 0) != -1);
# if D1
		fprintf(stderr,"item %d hash %d seek to %ld\n",nterm,hh[nterm],lp);
# endif
		g=j=0;
		while (1)
		{
			if (iflong)
				k = getl(fb);
			else
				k = getw(fb);
			if (k== -1) break;
# if D2
			fprintf(stderr,"next term finds %ld\n",k);
# endif
# if D3
			if (iflong)
				fprintf(stderr, "bfwh j %d nf %d master %ld k %ld\n",j,nf,prevdrop.b[j],(long)(k));
			else
				fprintf(stderr, "bfwh j %d nf %d master %ld k %ld\n",j,nf,prevdrop.a[j],(long)(k));
# endif
			while (j<nf && (iflong?prevdrop.b[j]:prevdrop.a[j])<k)
			{
# if D3
				if (iflong)
					fprintf(stderr, "j %d nf %d prevdrop %ld prevcoord %d colevel %d nterm %d k %ld\n",
					j,nf,prevdrop.b[j], prevcoord[j], colevel, nterm, (long)(k));
				else
					fprintf(stderr, "j %d nf %d prevdrop %ld prevcoord %d colevel %d nterm %d k %ld\n",
					j,nf,prevdrop.a[j], prevcoord[j], colevel, nterm, (long)(k));
# endif
				if (prevcoord[j] + colevel <= nterm)
					j++;
				else
				{
					_assert (g<lmaster);
					if (iflong)
						master.b[g] = prevdrop.b[j];
					else
						master.a[g] = prevdrop.a[j];
					coord[g++] = prevcoord[j++];
# if D1
					if (iflong)
						fprintf(stderr, " not skip g %d doc %d coord %d note %d\n",g,master.b[g-1], coord[g-1],master.b[j-1]);
					else
						fprintf(stderr, " not skip g %d doc %ld coord %d nterm %d\n",g,master.a[g-1], coord[g-1],nterm);
# endif
					continue;
				}
			}
			if (colevel==0 && j>=nf) break;
			if (j<nf && (iflong? prevdrop.b[j]: prevdrop.a[j]) == k)
			{
				if (iflong)
					master.b[g]=k;
				else
					master.a[g]=k;
				coord[g++] = prevcoord[j++]+1;
# if D1
				if (iflong)
					fprintf(stderr, " at g %d item %ld coord %d note %ld\n",g,master.b[g-1],coord[g-1],master.b[j-1]);
				else
					fprintf(stderr, " at g %d item %d coord %d note %d\n",g,master.a[g-1],coord[g-1],master.a[j-1]);
# endif
			}
			else
				if (colevel >= nterm)
				{
					if (iflong)
						master.b[g]=k;
					else
						master.a[g]=k;
					coord[g++] = 1;
				}
		}
# if D1
		fprintf(stderr,"now have %d items\n",g);
# endif
		if (colevel>0)
			for ( ; j<nf; j++)
				if (prevcoord[j]+colevel > nterm)
				{
					_assert(g<lmaster);
					if (iflong)
						master.b[g] = prevdrop.b[j];
					else
						master.a[g] = prevdrop.a[j];
					coord[g++] = prevcoord[j];
# if D3
					if(iflong)
						fprintf(stderr, "copied over %ld coord %d\n",master.b[g-1], coord[g-1]);
					else
						fprintf(stderr, "copied over %d coord %d\n",master.a[g-1], coord[g-1]);
# endif
				}
		nf = g;
	}
	if (colevel>0)
	{
		best=0;
		for(j=0; j<nf; j++)
			if (coord[j]>best) best = coord[j];
# if D1
		fprintf(stderr, "colevel %d best %d\n", colevel, best);
# endif
		reached = best;
		for(g=j=0; j<nf; j++)
			if (coord[j]==best)
			{
				if (iflong)
					master.b[g++] = master.b[j];
				else
					master.a[g++] = master.a[j];
			}
		nf=g;
# if D1
		fprintf(stderr, "yet got %d\n",nf);
# endif
	}
# ifdef D1
	fprintf(stderr, " returning with %d\n",nf);
# endif
	if (colevel)
	{
		free(prevdrop, lmaster, iflong?sizeof(long): sizeof(int));
		free(prevcoord, lmaster, sizeof (lmaster));
	}
# if D3
	for(g=0;g<nf;g++)
		if(iflong)
			fprintf(stderr,":%ld\n",master.b[g]);
		else
			fprintf(stderr,":%d\n",master.a[g]);
# endif
	return(nf);
}

long
getl(fb)
FILE *fb;
{
	return(getw(fb));
}

putl(ll, f)
long ll;
FILE *f;
{
	putw(ll, f);
}

hcomp( n1, n2)
{
	return (hfreq[hh[n1]]<=hfreq[hh[n2]]);
}

hexch( n1, n2 )
{
	int t;
	t = hh[n1];
	hh[n1] = hh[n2];
	hh[n2] = t;
}
