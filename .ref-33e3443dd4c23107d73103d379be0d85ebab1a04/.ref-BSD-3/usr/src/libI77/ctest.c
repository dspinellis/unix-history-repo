#include "stdio.h"
char buf[256];
main()
{	int w,dp,sign;
	char *s;
	double x;
	for(;;)
	{
		scanf("%d %lf",&w,&x);
		if(feof(stdin)) exit(0);
		s=fcvt(x,w,&dp,&sign);
		strcpy(buf,s);
		printf("%d,%f:\t%d\t%s\n",w,x,dp,buf);
		s=ecvt(x,w,&dp,&sign);
		printf("\t\t%d\t%s\n",dp,s);
	}
}
