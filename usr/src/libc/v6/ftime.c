#include <sys/types.h>
#include <sys/timeb.h>

static struct timeb gorp = {
	0L,
	0,
	5*60,
	1
};

ftime(gorpp)
struct timeb *gorpp;
{
	*gorpp = gorp;
	return(0);
}
