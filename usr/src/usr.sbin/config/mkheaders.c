/*
 * mkheaders.c	1.1	81/02/24
 * Make header files for EVERYTHING
 */

#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "y.tab.h"



headers()
{
    register struct device *ip;
    register struct file_list *tp;
    register int count;

    for (tp = ftab; tp != NULL; tp = tp->f_next)
    {
	if (tp->f_needs == NULL)
	    continue;
	count = 0;
	for (ip = dtab; ip != NULL; ip = ip->d_next)
	{
	    if (eq(tp->f_needs, ip->d_name))
		count++;
	}
	do_header(tp->f_needs, count);
    }
}

/*
 * tomacro
 *	Convert a two character name to a NXX
 */

tomacro(nm)
register char *nm;
{
    static char ret[80];
    register char *cp;

    cp = ret;
    *cp++ = 'N';
    while(*nm)
	*cp++ = toupper(*nm++);
    *cp++ = '\0';
    return ret;
}

/*
 * do_header:
 *	See if a header file needs to be changed
 */

do_header(dev, count)
char *dev;
int count;
{
    register FILE *f;
    char file[80];
    register char *w;
    register int oldcount;

    strcpy(file, "_unix/");
    strcat(file, dev);
    strcat(file, ".h");
    oldcount = -1;
    if ((f = fopen(file, "r")) != NULL)
    {
	/*
	 * Throw out the #define and the NXX
	 */
	if ((w = get_word(f)) != EOF && w != NULL)
	    if ((w = get_word(f)) == EOF && w != NULL)
		oldcount = atoi(get_word(f));
	fclose(f);
    }
    if (oldcount != count)
    {
	f = fopen(file, "w");
	fprintf(f, "#define %s %d\n", tomacro(dev), count);
	fclose(f);
    }
}
