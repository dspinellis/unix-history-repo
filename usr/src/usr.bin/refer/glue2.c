#ifndef lint
static char *sccsid = "@(#)glue2.c	4.1 (Berkeley) 5/6/83";
#endif

char refdir[50];

savedir()
{
	if (refdir[0]==0)
		corout ("", refdir, "/bin/pwd", "", 50);
	trimnl(refdir);
}

restodir()
{
	chdir(refdir);
}
