#include "mh.h"
#include <stdio.h>

gans(prompt, ansp)
struct swit *ansp;
{
	char ansbuf[32];
	register char *cp;
	register int i;
	struct swit *ap;

    for(;;) {
	printf("%s", prompt);
	fflush(stdout);
	cp = ansbuf;
	while((i = getchar()) != '\n') {
		if(i == EOF)
			return(0);
		if(cp < &ansbuf[31]) {
			if(i >= 'A' && i <= 'Z')
				i += 'a'-'A';
			*cp++ = i;
		}
	}
	*cp = 0;
	if(ansbuf[0] == '?' || cp == ansbuf) {
		printf("Options are:\n");
		for(ap = ansp; ap->sw; ap++)
			printf("  %s\n", ap->sw);
		continue;
	}
	if((i = smatch(ansbuf, ansp)) < 0) {
		printf("%s: %s.\n", ansbuf, i == -1? "unknown":"ambiguous");
		continue;
	}
	return(i);
    }
}
