#include "mh.h"
#include <stdio.h>

m_send(arg, file)
char *arg, *file;
{
	char *vec[10];
	int ivec;

	ivec = 0;
	vec[ivec++] = "send";
	vec[ivec++] = file;
	if(*arg == 'v')
		vec[ivec++] = "-verbose";
	vec[ivec++] = 0;
	m_update();
	fflush(stdout);
	execv(sendproc, vec);
	fprintf(stderr, "Can't exec %s.\n", sendproc);
	return(0);

}
