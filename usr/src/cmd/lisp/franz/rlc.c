static char *sccsid = "@(#)rlc.c	35.1 5/6/81";

#define TRUE 1
extern char holend[], end[];
extern int usehole;
extern char *curhbeg;

rlc()
{
	char *cp, *dp;
	
	brk(end);
	dp = holend;
	cp = dp - HOLE;
	while (dp < end)
		*dp++ = *cp++;
	curhbeg = holend - HOLE;	/* set up the hole */
	usehole = TRUE;
}
