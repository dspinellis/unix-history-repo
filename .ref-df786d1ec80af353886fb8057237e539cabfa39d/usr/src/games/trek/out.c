#ifndef lint
static char sccsid[] = "@(#)out.c	4.1	(Berkeley)	%G%";
#endif not lint

# include	"trek.h"

/*
**  Announce Device Out
*/

out(dev)
int	dev;
{
	register struct device	*d;

	d = &Device[dev];
	printf("%s reports %s ", d->person, d->name);
	if (d->name[length(d->name) - 1] == 's')
		printf("are");
	else
		printf("is");
	printf(" damaged\n");
}
