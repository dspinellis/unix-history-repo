#include "uucp.h"


#define SNAMESIZE 7

/*******
 *	versys(name)	verify system names n1 and n2
 *	char *name;
 *
 *	return codes:  0  |  FAIL
 */

versys(name)
char *name;
{
	FILE *fp;
	char **fnp;
	char line[300];
	char s1[SNAMESIZE + 1];
	char myname[SNAMESIZE + 1];
	char *index();

	sprintf(myname, "%.7s", Myname);
	sprintf(s1, "%.7s", name);
	if (strcmp(s1, myname) == 0)
		return(0);
	for (fnp = Sysfiles; *fnp != NULL; fnp++) {
		fp = fopen(*fnp, "r");
		if (fp == NULL)
			continue;
	
		while (fgets(line, 300, fp) != NULL) {
			*(index(line, ' ')) = '\0';
			line[7] = '\0';
			if (strcmp(s1, line) == SAME) {
				fclose(fp);
				return(0);
			}

		}
		fclose(fp);
	}
	return(FAIL);
}
