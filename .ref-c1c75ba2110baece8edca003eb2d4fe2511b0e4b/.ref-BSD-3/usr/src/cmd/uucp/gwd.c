#include "uucp.h"

/*******
 *	gwd(wkdir)	get working directory
 *
 *	return codes  0 | FAIL
 */

gwd(wkdir)
char *wkdir;
{
	FILE *fp;
	extern FILE *popen(), *pclose();
	char *c;

	if ((fp = popen("pwd", "r")) == NULL)
		return(FAIL);
	if (fgets(wkdir, 100, fp) == NULL) {
		pclose(fp);
		return(FAIL);
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	pclose(fp);
	return(0);
}
