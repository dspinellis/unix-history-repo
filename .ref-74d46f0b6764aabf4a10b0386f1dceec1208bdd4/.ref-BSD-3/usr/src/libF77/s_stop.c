#include <stdio.h>

s_stop(s, n)
char *s;
long int n;
{
int i;

if(n > 0)
	{
	fprintf(stderr, "STOP ");
	for(i = 0; i<n ; ++i)
		putc(*s++, stderr);
	fprintf(stderr, " statement executed\n");
	}
f_exit();
exit(0);
}
