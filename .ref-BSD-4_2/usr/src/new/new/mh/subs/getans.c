#include "mh.h"
#include <signal.h>
#include <stdio.h>

int g_sigint;   /* sensed an interrupt */
int g_sig();

char **getans(prompt, ansp)
struct swit *ansp;
{
	static char ansbuf[128];
	register char *cp, **cpp;
	register int i;
	struct swit *ap;

	signal(SIGINT, g_sig);
	for(;;) {
		printf("%s", prompt);
		fflush(stdout);
		cp = ansbuf;
		while((i = getchar()) != '\n') {
			if(i == EOF || g_sigint)  {
				g_sigint = 0;
				return(0);
			}
			if(cp < &ansbuf[127])
				*cp++ = i;
		}
		*cp = 0;
		if(ansbuf[0] == '?' || cp == ansbuf) {
			printf("Options are:\n");
			printsw(ALL, ansp, "");
			continue;
		}
		cpp = brkstring(ansbuf, " ", 0);
		switch(smatch(*cpp, ansp)) {
		case -2:ambigsw(*cpp, ansp);                   /* ambiguous */
			continue;
		case -1:                                       /* unknown   */
			printf(" -%s unknown. Hit <CR> for help.\n", *cpp);
			continue;
		default:
			return(cpp);            /* list, edit, quit, send */
		}
	}
}


g_sig()
{
	signal(SIGINT, g_sig);
	g_sigint = 1;
	return;
}
