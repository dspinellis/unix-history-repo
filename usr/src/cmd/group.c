#include <grp.h>
#define	NULL	0

struct	group *getgrgid(), *grp;

main()
{
	if((grp=getgrgid(getgid())) == NULL) 
		printf("%d\n", getgid());
	else
		printf("%s\n", grp->gr_name);
}
