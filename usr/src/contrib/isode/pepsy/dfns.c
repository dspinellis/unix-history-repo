/* dfns.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/dfns.c,v 7.3 91/02/22 09:48:46 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/dfns.c,v 7.3 91/02/22 09:48:46 mrose Interim $
 *
 *
 * $Log:	dfns.c,v $
 * Revision 7.3  91/02/22  09:48:46  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/12/23  18:42:03  mrose
 * update
 * 
 * Revision 7.1  90/11/04  19:18:19  mrose
 * update
 * 
 * Revision 7.0  90/07/01  19:54:14  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "general.h"
#include "mine.h"
#include "pepsydefs.h"
#include "pass2.h"

id_entry *id_table[TABLESIZE];

#define my_error(mesg)	(fprintf(stderr, "%s\n",mesg),exit(1))

extern char *notidtoid(), *my_new_str(), *my_strcat();
extern char *
proc_name(), *mymodule;

/*
 * Lookup the hash table (id_table) for the string t and insert it at
 * the start of the appropriate chain if it is not already there.
 * The argument flag indicates whether t is being defined (1) or used
 * (0).
 */
char   *
proc_name(t, flag)
char   *t;
int     flag;
{
    int     i;
    static int curr = 0;
    id_entry *ptr;

    i = hash_val(t);
    for (ptr = id_table[i]; ptr != NULL && strcmp(t, ptr->h_value); ptr = ptr->next);

    if (ptr == NULL) {
	if ((ptr = (id_entry *) malloc(sizeof(id_entry))) == NULL)
	    my_error("proc_name: Out of memory");
	ptr->h_value = t;
	ptr->r_value = my_strcat(notidtoid(t), notidtoid(mymodule));
	ptr->count = 1;
	if (flag) {
	    ptr->def_bit = flag;
	    ptr->def_value = curr++;
	}
	ptr->next = id_table[i];
	id_table[i] = ptr;
    } else if (!ptr->def_bit)
	ptr->def_bit = flag;

    return ptr->r_value;
}

/*
 * output a sequence of #define statements (one for each value stored
 * in the hash table) to the file specified by fp
 */
out_final_defs(fp)
FILE   *fp;
{
    int     j;
    id_entry *ptr;

    for (j = 0; j < TABLESIZE; j++)
	for (ptr = id_table[j]; ptr != NULL; ptr = ptr->next) {
	    if (ptr->def_bit)
		(void) fprintf(fp, "#define %s%s\t%d\n", PREFIX, ptr->r_value, ptr->def_value);
	    else
		ferrs(0, "the identifier %s is used but not defined\n", ptr->h_value);
	    if (ptr->count > 1)	/* not used */
		(void) printf("The id %s has a count of %d\n", ptr->r_value, ptr->count);
	}
}

/*
 * return a copy of the string s with '-' replaced by '_'
 */
char   *
notidtoid(s)
char   *s;
{

    char   *t, *r;

    t = my_new_str(s);
    for (r = t; *r != '\0'; r++)
	if (*r == '-')
	    *r = '_';
    return t;
}

/*
 * return a copy of the string s
 */
char   *
my_new_str(s)
char   *s;
{

    char   *t;

    if ((t = (char *) malloc(strlen(s) + 1)) == NULL)
	my_error("my_new_str: Out of memory");

    strcpy(t, s);
    return t;
}

/*
 * return the concatenation of the strings s1 and s2
 */
char   *
my_strcat(s1, s2)
char   *s1, *s2;
{
    char   *s3, *s, *t;

    if (s1 == NULL || *s1 == '\0')
	return my_new_str(s2);

    if ((s3 = (char *) malloc(strlen(s1) + strlen(s2) + 1)) == NULL)
	my_error("my_strcat: Out of memory");

    for (s = s1, t = s3; *s != '\0'; s++, t++)
	*t = *s;
    strcpy(t, s2);
    return s3;
}

/*
 * a simple hash function
 */
hash_val(s)
char   *s;
{
    int     i, sum;
    char   *t;

    sum = 0;
    for (i = 1, t = s; *t != '\0'; i++, t++)
	sum = sum + *t * i;
    return (sum % TABLESIZE);
}

/*
 * initialize the table id_table
 */
init()
{
    int     i;

    for (i = 0; i <= TABLESIZE; i++)
	id_table[i] = NULL;
}
#define BUFSIZE		128
#define MAX(a, b)	((a) > (b) ? (a) : (b))

static char *buf = NULL;
static int len = 0;

/*
 * Return in a static buffer the two strings concatenated
 */
char   *
concat(s1, s2)
char   *s1, *s2;
{
    int     tot;

    tot = strlen(s1) + strlen(s2) + 1;

    if (tot > len) {
	len = MAX(BUFSIZE, tot);
	if (buf == NULL) {
	    if ((buf = malloc(len)) == NULL)
		ferr(1, "concat:malloc failed\n");
	} else if ((buf = realloc(buf, len)) == NULL)
	    ferr(1, "concat:realloc failed\n");
    }
    strcpy(buf, s1);
    strcat(buf, s2);

    return (buf);
}

/*
 * Generate a free call given the name of the parameter, the module
 * name, and the name of the type
 */
char   *
gfree(module, id, parm)
char   *module;			/* name of module we are in (usually
				 * mymodule) */
char   *id;			/* name of type we want to free */
char   *parm;			/* name of the pointer to the data */
{
    char   *p1 = notidtoid(module);
    char   *p2 = notidtoid(id);

    if (buf == NULL) {
	if ((buf = malloc(BUFSIZE)) == NULL)
	    ferr(1, "gfree:malloc failed\n");
	len = BUFSIZ;
    }
    (void) sprintf(buf, "(void) fre_obj((char *) %s, %s%s%s.md_dtab[%s%s%s], &%s%s%s, 1)",
	    parm,
	    PREFIX, p1, MODTYP_SUFFIX,
	    PREFIX, p2, p1,
	    PREFIX, p1, MODTYP_SUFFIX);
    free(p1);
    free(p2);

    return (buf);
}
