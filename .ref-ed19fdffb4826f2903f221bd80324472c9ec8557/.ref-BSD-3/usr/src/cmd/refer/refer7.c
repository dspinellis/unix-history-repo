# include "refer..c"
int newr[250];
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
fo = NULL;
if (sort)
	{
	char comm[100];
	sprintf(comm, "sort %s -o %s", tfile, tfile);
	system(comm);
	}
fi = fopen(tfile, "r");
if (fi==NULL) return;
flout();
fprintf(ftemp,".]<\n");
while ( (c = getc(fi)) >0)
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
		char *s = tb;
		while ( (c=getc(fi)) != FLAG)
			*s++ = c;
		*s=0;
		if (g1++ == 0)
			newr[atoi(tb)] = nr;
# if D1
		fprintf(stderr, "nr %d assigned to atoi(tb) %d\n",nr,atoi(tb));
# endif
		fprintf(ftemp,"%d", nr);
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

ftemp = fopen(fnam, "r");
if (ftemp==NULL)
	{
	fprintf(stderr, "Can't reopen %s\n", fnam);
	exit(1);
	}
while ( (c = getc(ftemp)) != EOF)
	{
	if (c == FLAG)
		{
		char tb[10];
		char *s = tb;
		while ( (c = getc(ftemp)) != FLAG)
			*s++ = c;
		*s=0;
		printf("%d", newr[atoi(tb)]);
		continue;
		}
	putchar(c);
	}
fclose(ftemp);
unlink(fnam);
}
