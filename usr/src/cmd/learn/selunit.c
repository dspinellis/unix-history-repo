#include "stdio.h"
#include "lrnref"

int	nsave	= 0;

selunit()
{
	char fnam[20], s[50];
	static char dobuff[50];
	char posslev[20][20];
	int diff[20], i, k, m, n, best, alts;
	FILE *f;
	char zb[200];
	static char saved[20];

	while (ask) {
		printf("What lesson? ");
		fflush(stdout);
		gets(dobuff);
		if (strcmp(dobuff, "bye") == 0)
			wrapup(0);
		level = todo = dobuff;
		sprintf(s, "../../%s/L%s", sname, dobuff);
		if (access(s, 04) == 0)
			return;
		printf("no such lesson\n");
	}
	alts = 0;
retry:
	f=scrin;
	if (f==NULL) {
		sprintf(fnam, "../../%s/L%s", sname, level);
		f = fopen(fnam, "r");
		if (f==NULL) {
			fprintf(stderr, "No script for lesson %s.\n", level);
			wrapup(1);
		}
		while (fgets(zb, 200, f)) {
			trim(zb);
			if (strcmp(zb, "#next")==0)
				break;
		}
	}
	if (feof(f)) {
		printf("Congratulations; you have finished this sequence.\n");
		fflush(stdout);
		todo = 0;
		return;
	}
	for(i=0; fgets(s, 50, f); i++) {
		sscanf(s, "%s %d", posslev[i], &diff[i]);
	}
	best = -1;
	/* cycle through lessons from random start */
	/* first try the current place, failing that back up to
	     last place there are untried alternatives (but only one backup) */
	n = grand()%i;
	for(k=0; k<i; k++) {
		m = (n+k)%i;
		if (already(posslev[m],0)) continue;
		if (best<0) best=m;
		/* real alternatives */
		alts++;
		if (abs(diff[m]-speed) < abs(diff[best]-speed))
			best=m;
	}
	if (best < 0 && nsave) {
		nsave--;
		strcpy(level, saved);
		goto retry;
	}
	if (best <0) {
		/* lessons exhausted or missing */
		printf("Sorry, there are no alternative lessons at this stage.\n");
		printf("See someone for help.\n");
		fflush(stdout);
		todo = 0;
		return;
	}
	strcpy (dobuff, posslev[best]);
	if (alts>1) {
		nsave=1;
		strcpy (saved, level);
	}
	todo = dobuff;
	fclose(f);
}

abs(x)
{
	return(x>=0? x: -x);
}

grand()
{
	static int garbage;
	int a[2], b;

	time(a);
	b = a[1]+10*garbage++;
	return(b&077777);
}
