#ifndef lint
static char sccsid[] = "@(#)makekey.c	2.2	%G%";
#endif not lint
#

# include "stdio.h"
# include "ctype.h"
# include "bib.h"

char    commlist[MAXCOMM]=   /*  list of strings of common words         */
     "";
int firsttime = 1;

/*  makekey(p,max_klen,common):  compresses *p into a key
        folds upper to lower case.  ignores non-alphanumeric
        drops keys of length <= 1.
        drops words in common (name of file of words, one per line)
            (first call determines common for all later calls)
*/
makekey(p,max_klen,common)
char *p;
int  max_klen;          /* max key length */
char *common;
{   register char *from, *to, *stop;

    if (firsttime) {firsttime= 0; load_comm(common); }

    from= p; to= p; stop= max_klen+p;
    while (*from != NULL  &&  to < stop)
    {   if      (islower(*from))      *to++ = *from++;
        else if (isdigit(*from))      *to++ = *from++;
        else if (isupper(*from))    { *to++ = tolower(*from);  from++; }
        else                          from++;
    }
    *to= NULL;

    if (to<=p+1 ||
        lookup(commlist, p) )  *p= NULL;
}

/*  list is a string of null terminated strings, final string is null.
    p is a null terminated string.
    return 1 if p is a string in list, 0 ow.
*/
int lookup(list,p)
char *list, *p;
{   int len;
    len= strlen(list);
    while (len!=0 && strcmp(list,p)!=0)
    {   list += (len+1);
        len= strlen(list);
    }
    return(len!=0);
}

/*  read file common into commlist
*/
load_comm(common)
char *common;
{   FILE    *commfile;          /*  stream of common words                  */
    char *p, *stop;
    commfile= fopen(common,"r");
    if (commfile==NULL) fprintf(stderr, "cannot open '%s'\n", common);
    else
    {   /* read commfile into commlist  */
            p= commlist;    stop= commlist+MAXCOMM-1;
            while (p<stop && ((*p= getc(commfile))!=EOF))
            {   if (*p=='\n')   *p= NULL;
                p++;
            }
            if  (*p==EOF)  *p= NULL;
            else
            {   fprintf(stderr, "invert: too many common words\n");
                commlist[0]= NULL;
            }
        fclose(commfile);
    }
}

