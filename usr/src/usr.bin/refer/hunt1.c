#ifndef lint
static char *sccsid = "@(#)hunt1.c	4.3 (Berkeley) 1/9/85";
#endif

# include <stdio.h>
# include <assert.h>
extern char refdir[];
extern int keepold;
extern char *fgnames[];
extern char **fgnamp;
FILE *fd =NULL;
int lmaster =500;
int *hfreq, hfrflg;
int colevel =0;
int measure=0;
int soutlen =1000;
int reached =0;
int iflong =0;
int prfreqs =0;
char usedir[100];
char * calloc(); 
char * todir();
char gfile[50];
static int full =1000;
static int tags =0;
char *sinput, *soutput, *tagout;
long indexdate =0, gdate();

main(argc,argv)
char *argv[];
{
	/* read query from stdin, expect name of indexes in argv[1] */
	static FILE *fa, *fb, *fc;
	char nma[100], nmb[100], nmc[100], *qitem[100], *rprog = NULL;
	char nmd[100], grepquery[256];
	static char oldname[30] ;
	static int was =0;
	/* these pointers are unions of pointer to int and pointer to long */
	long *hpt;
	unsigned *master =0;
	int falseflg, nhash, nitem, nfound, frtbl, kk;

	/* special wart for refpart: default is tags only */

	while (argv[1][0] == '-')
	{
		switch(argv[1][1])
		{
		case 'a': /* all output, incl. false drops */
			falseflg = 1; 
			break;
		case 'r':
			argc--; 
			argv++;
			rprog = argv[1];
			break;
		case 'F': /* put out full text */
			full = setfrom(argv[1][2]);
			break;
		case 'T': /* put out tags */
			tags = setfrom(argv[1][2]);
			break;
		case 'i': /* input in argument string */
			argc--; 
			argv++;
			sinput = argv[1];
			break;
		case 's': /*text output to string */
		case 'o':
			argc--; 
			argv++;
			soutput = argv[1];
			if ((int) argv[2]<16000)
			{
				soutlen = (int)argv[2];
				argc--; 
				argv++;
			}
			break;
		case 't': /*tag output to string */
			argc--; 
			argv++;
			tagout = argv[1];
			break;
		case 'l': /* length of internal lists */
			argc--; 
			argv++;
			lmaster = atoi(argv[1]);
			break;
		case 'g': /* suppress fgrep search on old files */
			keepold = 0;
			break;
		case 'C': /* coordination level */
			colevel = atoi(argv[1]+2);
# if D1
			fprintf(stderr, "colevel set to %d\n",colevel);
# endif
			break;
		case 'P': /* print term freqs */
			prfreqs=1; 
			break;
		case 'm':
			measure=1; 
			break;
		}
		argc--; 
		argv++;
	}
	strcpy (nma, todir(argv[1]));
	if (was == 0 || strcmp (oldname, nma) !=0)
	{
		strcpy (oldname,nma);
		strcpy (nmb, nma); 
		strcpy (nmc, nmb); 
		strcpy(nmd,nma);
		strcat (nma, ".ia");
		strcat (nmb, ".ib");
		strcat (nmc, ".ic");
		strcat (nmd, ".id");
		if (was)
		{
			fclose(fa); 
			fclose(fb); 
			fclose(fc);
		}

		fa = fopen(nma, "r");
		if (fa==NULL)
		{
			strcpy(*fgnamp++ = calloc(strlen(oldname)+2,1), oldname);
			fb=NULL;
			goto search;
		}
		fb = fopen(nmb, "r");
		fc = fopen(nmc, "r");
		was =1;
		if (fb== NULL || fc ==NULL)
		{
			err("Index incomplete %s", nmb);
			exit(1);
		}
		indexdate = gdate(fb);
		fd = fopen(nmd, "r");
	}
	fseek (fa, 0L, 0);
	fread (&nhash, sizeof(nhash), 1, fa);
	fread (&iflong, sizeof(iflong), 1, fa);
	if(master==0)
		master = (unsigned *) calloc (lmaster, iflong? sizeof(long): sizeof(unsigned));
	hpt = (long *) calloc(nhash, sizeof(*hpt));
	kk=fread( hpt, sizeof(*hpt), nhash, fa);
# if D1
	fprintf(stderr,"read %d hashes, iflong %d, nhash %d\n", kk, iflong, nhash);
# endif
	_assert (kk==nhash);
	hfreq = (int *) calloc(nhash, sizeof(*hfreq));
	_assert (hfreq != NULL);
	frtbl = fread(hfreq, sizeof(*hfreq), nhash, fa);
	hfrflg = (frtbl == nhash);
# if D1
	fprintf(stderr, "read freqs %d\n", frtbl);
# endif

search:
	while (1)
	{
		nitem = getq(qitem);
		if (measure) tick();
		if (nitem==0) continue;
		if (nitem < 0) break;
		if (tagout) tagout[0]=0;
		if (fb!=NULL)
		{
			nfound = doquery(hpt, nhash, fb, nitem, qitem, master);
# if D1
			fprintf(stderr,"after doquery nfound %d\n", nfound);
# endif
			fgnamp=fgnames;
			if (falseflg == 0)
				nfound = baddrop(master, nfound, fc, nitem, qitem, rprog, full);
# if D1
			fprintf(stderr,"after baddrop nfound %d\n", nfound);
# endif
		}
		if (fgnamp>fgnames)
		{
			char **fgp, tgbuff[100];
			int k;
# if D1
			fprintf(stderr, "were %d bad files\n", fgnamp-fgnames);
# endif
			grepquery[0]=0;
			for(k=0; k<nitem; k++)
			{
				strcat(grepquery, " ");
				strcat(grepquery, qitem[k]);
			}
# if D1
			fprintf(stderr, "grepquery %s\n",grepquery);
# endif
			for(fgp=fgnames; fgp<fgnamp; fgp++)
			{
# if D1
				fprintf(stderr, "Now on %s query /%s/\n", *fgp, grepquery);
# endif
				makefgrep(*fgp);
# if D1
				fprintf(stderr, "grepmade\n");
# endif
				if (tagout==0)
					tagout=tgbuff;
				grepcall(grepquery, tagout, *fgp);
# if D1
				fprintf(stderr, "tagout now /%s/\n", tagout);
# endif
				if (full)
				{
					char bout[1000];
					char *tagp;
					char *oldtagp;
					tagp = tagout;
					while (*tagp) {
						oldtagp = tagp;
						while (*tagp && (*tagp != '\n'))
							tagp++;
						if (*tagp)
							tagp++;
						findline(oldtagp, bout, 1000);
						fputs(bout,stdout);
					}
				}
			}
		}
		if (tags)
			result (master, nfound >tags ? tags: nfound, fc);
		if (measure) tock();
	}
}

char *
todir(t)
char *t;
{
	char *s;
	s=t;
	while (*s) s++;
	while (s>=t && *s != '/') s--;
	if (s<t) return(t);
	*s++ = 0;
	t = (*t ? t : "/");
	chdir (t);
	strcpy (usedir,t);
	return(s);
}
setfrom(c)
{
	switch(c)
	{
	case 'y': 
	case '\0':
	default:
		return(1000);
	case '1':
	case '2': 
	case '3': 
	case '4': 
	case '5':
	case '6': 
	case '7': 
	case '8': 
	case '9':
		return(c-'0');
	case 'n': 
	case '0':
		return(0);
	}
}
