/*
 *	mkheaders.c	1.4	81/02/26
 * Make all the .h files for the optional entries
 */

#include <stdio.h>
#include <ctype.h>
#include "config.h"

/*
 * This macro reads a line of the form
 *	#define STRING <number>
 * and assigns STRING to wd and <number> to count
 */
#define rdln(f, wd, count) {\
	register char *iwd;\
	if ((wd = get_word(f)) != NULL && wd != EOF)\
	    if ((wd = get_word(f)) != NULL && wd != EOF) {\
		iwd = ns(wd);\
		if ((wd = get_word(f)) != NULL && wd != EOF) {\
		    count = atoi(wd);\
		    wd = get_word(f);\
		    wd = iwd;\
		}\
	    }\
	}

headers()
{
    register struct file_list *fl;

    for (fl = ftab; fl != NULL; fl = fl->f_next)
	if (fl->f_needs != NULL)
	    do_count(fl->f_needs, fl->f_needs, TRUE);
}

/*
 * do_count:
 *	Count all the devices of a certain type and recurse to count
 *	whatever the device is connected to
 */

do_count(dev, hname, search)
register char *dev, *hname;
bool search;
{
    register struct device *dp, *mp;
    register int count;

    for (count = 0,dp = dtab; dp != NULL; dp = dp->d_next)
	if (dp->d_unit != -1 && eq(dp->d_name, dev))
	{
	    count++;
	    if (search)
	    {
		mp = dp->d_conn;
		if (mp != NULL && mp != -1 && mp->d_conn != -1)
		{
		    do_count(mp->d_name, hname, FALSE);
		    search = FALSE;
		}
	    }
	}
    do_header(dev, hname, count);
}

do_header(dev, hname, count)
char *dev, *hname;
int count;
{
    char *file, *name, *inw, *toheader(), *tomacro();
    struct file_list *fl, *fl_head;
    FILE *inf, *outf;
    int inc, oldcount;

    file = toheader(hname);
    name = tomacro(dev);
    inf = fopen(file, "r");
    oldcount = -1;
    if (inf == NULL)
    {
	outf = fopen(file, "w");
	fprintf(outf, "#define %s %d\n", name, count);
	fclose(outf);
	return;
    }
    fl_head = NULL;
    while(1)
    {
	rdln(inf, inw, inc);
	if (inw == EOF)
	    break;
	if (eq(inw, name))
	{
	    oldcount = inc;
	    inc = count;
	}
	fl = (struct file_list *) malloc(sizeof *fl);
	fl->f_fn = inw;
	fl->f_type = inc;
	fl->f_next = fl_head;
	fl_head = fl;
    }
    fclose(inf);
    if (count == oldcount)
    {
	for (fl = fl_head; fl != NULL; fl = fl->f_next)
	    free(fl);
	return;
    }
    if (oldcount == -1)
    {
	fl = (struct file_list *) malloc(sizeof *fl);
	fl->f_fn = name;
	fl->f_type = count;
	fl->f_next = fl_head;
	fl_head = fl;
    }
    outf = fopen(file, "w");
    for (fl = fl_head; fl != NULL; fl = fl->f_next)
    {
	fprintf(outf, "#define %s %d\n", fl->f_fn, fl->f_type);
	free(fl);
    }
    fclose(outf);
}

/*
 * toheader:
 *	Convert a dev name to a .h file nae
 */

char *toheader(dev)
char *dev;
{
    static char hbuf[80];

    strcpy(hbuf, path(dev));
    strcat(hbuf, ".h");
    return hbuf;
}

/*
 * tomacro:
 *	Convert a dev name to a macro name
 */

char *tomacro(dev)
register char *dev;
{
    static char mbuf[20];
    register char *cp;

    cp = mbuf;
    *cp++ = 'N';
    while(*dev)
	*cp++ = toupper(*dev++);
    *cp++ = '\0';
    return mbuf;
}
