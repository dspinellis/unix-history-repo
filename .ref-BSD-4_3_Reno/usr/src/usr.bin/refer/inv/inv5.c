#ifndef lint
static char *sccsid = "@(#)inv5.c	4.3 (Berkeley) 4/24/88";
#endif

#include <stdio.h>

recopy (ft, fb, fa, nhash)
FILE *ft, *fb, *fa;
{
	/* copy fb (old hash items/pointers) to ft (new ones) */
	int n, i, iflong;
	long getl();
	int getw();
	int *hpt_s;
	long *hpt_l;
	long k, lp;
	if (fa==NULL)
	{
		err("No old pointers",0);
		return;
	}
	fread(&n, sizeof(n), 1, fa);
	fread(&iflong, sizeof(iflong), 1, fa);
	if (iflong)
	{
		hpt_l = (long *) calloc(sizeof(*hpt_l), n+1);
		n =fread(hpt_l, sizeof(*hpt_l), n, fa);
	}
	else
	{
		hpt_s =  (int *) calloc(sizeof(*hpt_s), n+1);
		n =fread(hpt_s, sizeof(*hpt_s), n, fa);
	}
	if (n!= nhash)
		fprintf(stderr, "Changing hash value to old %d\n",n);
	fclose(fa);
	for(i=0; i<n; i++)
	{
		if (iflong) {
			lp = hpt_l[i];
			fseek(fb, lp, 0);
			while ( (k= getl(fb) ) != -1)
				fprintf(ft, "%04d %06ld\n",i,k);
		} else {
			lp = hpt_s[i];
			fseek(fb, lp, 0);
			while ( (k= getw(fb) ) != -1)
				fprintf(ft, "%04d %06ld\n",i,k);
		}
	}
	fclose(fb);
	return(n);
}
