/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)stats.c 4.1 10/10/80";

#include	"stdio.h"
#include	"h00vars.h"
#include	"h01errs.h"
#define		HZ 60		/* interrupt frequency */

backtrace(errnum)
long	errnum;
{
register struct stack **mydp, *ap;
	 struct stack **dp, *disp[20];
register char *cp;
register long i;
	 long linum;

fetchdp(&dp,&linum);
for (i=0; i<20; i++)
	disp[i] = display[i];
if (errnum == EINTR)
	fputs("\n\tInterrupted in \"",stderr);
else if (errnum == EHALT)
	fputs("\n\tHalted in \"",stderr);
else
	fputs("\n\tError in \"",stderr);
if (linum <= 0)
	return;
mydp = dp;
for (;;){
	ap = *mydp;
	i = linum - (((ap)->entry)->offset & 0177777);
	fprintf(stderr,"%s\"",(ap->entry)->name);
	if (nodump == 0)
		fprintf(stderr,"+%1d near line %1d.",i,linum);
	fputc('\n',stderr);
	*mydp = (ap)->disp;
	if (mydp <= &display[1]){
		for (i=0; i<20; i++)
			display[i] = disp[i];
		return;
		}
	mydp = (ap)->dp;
	linum = (ap)->lino;
	fputs("\tCalled by \"",stderr);
	}
}

stats()
{
struct	{
	long	usr_time;
	long	sys_time;
	long	child_usr_time;
	long	child_sys_time;
	} tbuf;
register double l;
register long count;

if (nodump)
	return(0);
times(&tbuf);
#ifdef profile
datafile = fopen(proffile,"r");
if (datafile != NULL) {
	count = fread(&profdata,sizeof(profdata),1,datafile);
	if (count != 1) {
		for (count = 0;  count < numops;  count++)
			profdata.counts[count] = 0.0;
		profdata.runs = 0;
		profdata.startdate = time(0);
		profdata.usrtime = 0;
		profdata.systime = 0;
		profdata.stmts = 0;
		}
	for (count = 0;  count < numops;  count++)
		profdata.counts[count] += profcnts[count];
	profdata.runs += 1;
	profdata.stmts += stcnt;
	profdata.usrtime += tbuf.usr_time;
	profdata.systime += tbuf.sys_time;
	datafile = freopen(proffile,"w",datafile);
	if (datafile != NULL) {
		fwrite(&profdata,sizeof(profdata),1,datafile);
		fclose(datafile);
		}
	}
#endif
l = tbuf.usr_time;
l = l / HZ;
fprintf(stderr,"\n%1ld statements executed in %04.2f seconds cpu time.\n",
	stcnt,l);
}
