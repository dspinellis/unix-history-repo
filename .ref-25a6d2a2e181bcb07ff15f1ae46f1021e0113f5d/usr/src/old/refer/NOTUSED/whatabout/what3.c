#ifndef lint
static char *sccsid = "@(#)what3.c	4.2 (Berkeley) %G%";
#endif

#include "what..c"
#include "pathnames.h"

doclook(argc, argv, colevel)
char *argv[];
{
	int fpa[2], fpb[2], fpc[2], pid1, pid2, st;
	int iarg;
	char *s;
	FILE *ansf;
	struct filans *af;
	struct stat statbuf;
# define RD 0
# define WR 1
# define fmv(x,y) close(y); dup(x); close(x);
	/* we want to run chkbib and then lhunt and pipe in & out */
	pipe (fpa); /* from this program to chkbib */
	pipe (fpb); /* from chkbib to lhunt */
	pipe (fpc); /* from lhunt to us */
	if (  (pid1 = fork())  ==0)
	{
		fmv(fpa[RD], 0);
		fmv(fpb[WR], 1);
		close(fpa[WR]); 
		close(fpb[RD]); 
		close(fpc[RD]); 
		close(fpc[WR]); 
		execl(_PATH_MKEY, "mkey", "-s", 0);
		_assert(0);
	}
	if (  (pid2 = fork()) == 0)
	{
		char coarg[20];
		sprintf(coarg, "-C%d", colevel);
		fmv(fpb[RD], 0);
		fmv(fpc[WR], 1);
		close(fpa[RD]); 
		close(fpa[WR]); 
		close(fpb[WR]); 
		close(fpc[RD]);
		execl(_PATH_HUNT, "hunt",
		/* "-P", */
		coarg, "-Ty", "-Fn", _PATH_ALL, 0);
		_assert(0);
	}
	_assert (pid1 != -1); 
	_assert(pid2 != -1);
	close(fpb[RD]); 
	close(fpb[WR]); 
	close(fpa[RD]); 
	close(fpc[WR]);
	ansf = fopen(_PATH_DEVNULL, "r");
	fmv (fpc[RD], ansf->_file);
	for(iarg=1; iarg<argc; iarg++)
		prod(fpa[WR], argv[iarg]);
	close(fpa[WR]);
	s=fnames;
	af=files;
	while (af < files+NFILES)
	{
		if (fgets(af->nm=s, NAMES, ansf)==0)
			break;
		trimnl(s);
		if (*s==0) continue;
		while (*s++);
		_assert(s<fnames+NAMES);
		st = stat(af->nm, &statbuf);
		if (st<0) continue;
		af->uid = statbuf.st_uid;
		af->fdate = statbuf.st_mtime;
		af->size = statbuf.st_size;
		af++;
	}
	fclose(ansf);
	return(af-files);
}

prod(f,s)
char *s;
{
	write (f, s, strlen(s));
	write (f, "\n", 1);
}
