#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: pattern.c,v 1.7 85/01/18 15:34:07 notes Rel $";
#endif	RCSIDENT

/*
 *	dopat - matches notesfile names and calls the desired routine with
 *	each matched arguement!
 *
 *	Ray Essick			Feb 25, 1982
 */

FILE * popen ();					/* declare the function! */

dopat (p, what)
char   *p;						/* the pattern to match */
int     (*what) ();					/* routine to be called */
{
    char    cmd[CMDLEN];
    int     c,
            i;
    FILE * zfile;

    sprintf (cmd, "(cd %s;echo %s)", Mstdir, p);
    zfile = popen (cmd, "r");				/* read the output */
    if (zfile == (FILE *) NULL)
	return (0);					/* failed somewhere */
							/* pseudo-failure */

    do
    {
	i = 0;
	while ((c = getc (zfile)) != ' ' && (c != '\n') && (c != EOF))
	    cmd[i++] = c;
	if (i == 0)
	    break;
	cmd[i++] = '\0';
	i = (*what) (cmd);				/* call his routine for him */
    } while ((c != EOF) && (i != QUITFAST) && (i != QUITUPD));
    pclose (zfile);					/* close that pipe! */

    return i;						/* return the return code */
}
