/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)utilities.c 1.1 %G%";

#include	"vars.h"
#include	"panics.h"
#include	"h02opcs.h"

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

	if (_nodump)
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
	fprintf(stderr,
		"\n%1ld statements executed in %04.2f seconds cpu time.\n",
		_stcnt,l);
}

backtrace(errnum)
	long	errnum;
{
	register struct disp *mydp;
	register struct stack *ap;
	register char *cp;
	register long i, linum;
	struct disp disp[MAXLVL];

	if (_lino <= 0) {
		fputs("Program was not executed.\n",stderr);
		return;
	}
	for (i=0; i<MAXLVL; i++)
		disp[i] = _display[i];
	if (errnum == PINTR)
		fputs("\n\tInterrupted in \"",stderr);
	else if (errnum == PHALT)
		fputs("\n\tHalted in \"",stderr);
	else
		fputs("\n\tError in \"",stderr);
	mydp = _dp;
	linum = _lino;
	for (;;) {
		ap = mydp->stp;
		i = linum - (((ap)->entry)->offset & 0177777);
		fprintf(stderr,"%s\"",(ap->entry)->name);
		if (_nodump == 0)
			fprintf(stderr,"+%1d near line %1d.",i,linum);
		fputc('\n',stderr);
		*mydp = (ap)->odisp;
		if (mydp <= &_display[1]){
			for (i=0; i<MAXLVL; i++)
				_display[i] = disp[i];
			psexit(errnum);
		}
		mydp = (ap)->dp;
		linum = (ap)->lino;
		fputs("\tCalled by \"",stderr);
	}
}

psexit(code)

	long	code;
{
	if (_pcpcount != 0)
		PMFLUSH(_cntrs, _rtns, _pcpcount);
	if (_mode == PIX) {
		fputs("Execution terminated",stderr);
		if (code)
			fputs(" abnormally",stderr);
		fputc('.',stderr);
		fputc('\n',stderr);
	}
	stats();
	exit(code);
}
