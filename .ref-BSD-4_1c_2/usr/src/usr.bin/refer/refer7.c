# include "refer..c"
# define NRS 500
int newr[NRS];
chkdup ( tag )
	char *tag;
{
int i;
for(i=1; i<=refnum; i++)
	{
	if (strcmp(reftable[i], tag)==0)
		return(i);
	}
reftable[refnum+1] = rtp;
if (refnum>=NRFTBL)
	err("too many references (%d) for table", refnum);
strcpy (rtp, tag);
while (*rtp++);
if (rtp > reftext + NRFTXT)
	err("reftext too big (%d)", rtp-reftext);
return(0);
}
dumpold()
{
FILE *fi;
int c, g1 = 0, nr = 1;
if (!endpush) return;
fclose(fo);
# ifdef D1
fprintf(stderr, "closing %o\n",fo);
# endif
fo=NULL;
# if D1
fprintf(stderr, "dumpold for %s ftemp is %o stdout %o\n", tfile, ftemp, stdout);
# endif
if (sort)
	{
	char comm[100];
	sprintf(comm, "sort -fd %s -o %s", tfile, tfile);
	system(comm);
	}
fi = fopen(tfile, "r");
# if D1
fprintf(stderr, "reopened %s as %o\n",tfile,fi);
# endif
if (fi==NULL) return;
flout();
fprintf(ftemp,".]<\n");
while ( (c = getc(fi)) >=0)
	{
	if (c=='\n')
		{
		nr++;
		g1 = 0;
		}
	if (c==sep)
		c = '\n';
	if (c== FLAG)
		{
		/* make old-new ref number table */
		char tb[20];
		int irr;
		char *s = tb;
		while ( (c=getc(fi)) != FLAG)
			*s++ = c;
		*s=0;
		if (g1++ == 0)
			{
			irr = atoi(tb);
			if (irr>=NRS)
				err("too many references to renumber",0);
			newr[irr]=nr;
			}
# if D1
		fprintf(stderr, "nr %d assigned to atoi(tb) %d\n",nr,irr);
# endif
		if (!labels)
			fprintf(ftemp,"%d", nr);
		else
			expkey (irr, nr, ftemp);
		continue;
		}
	putc(c, ftemp);
	}
fclose(fi);
# ifndef D1
unlink(tfile);
# endif
fprintf(ftemp, ".]>\n");
}

recopy (fnam)
	char *fnam;
{
int c;
fclose(ftemp);
# if D1
fprintf(stderr, "recopy, fnam %s\n",fnam);
# endif

ftemp = fopen(fnam, "r");
if (ftemp==NULL)
	{
	fprintf(stderr, "Can't reopen %s\n", fnam);
	exit(1);
	}
# if D1
fprintf(stderr, "open ok ftemp %o\n",ftemp);
# endif
while ( 1)
	{
	c = fgetc(ftemp);
# if D1
fprintf(stderr, "read char %o /%c/\n",c,c);
# endif
	if (c == FLAG)
		{
		char tb[10];
		char *s = tb;
		int tbx;
		while ( (c = getc(ftemp)) != FLAG && c != EOF)
			*s++ = c;
		*s=0;
		tbx = atoi(tb);
# if D1
		fprintf(stderr, "prelim number is %d\n",tbx);
# endif
		if (!labels)
			printf("%d", newr[tbx]);
		else
			expkey (tbx, newr[tbx], stdout);
		continue;
		}
	if (c==EOF) break;
	putchar(c);
	}
# if D1
fprintf(stderr, "finished reading file \n");
# endif
fclose(ftemp);
unlink(fnam);
# if D1
fprintf(stderr, "leaving recopy\n");
# endif
}
