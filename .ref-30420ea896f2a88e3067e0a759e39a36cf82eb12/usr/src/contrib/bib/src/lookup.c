#ifndef lint
static char sccsid[] = "@(#)lookup.c	2.2	%G%";
#endif not lint
# include "stdio.h"
# include "streams.h"
# include "bib.h"

char *locate();

int     max_klen =   6;     /*  max length of keys                      */
char    *common =           /*  name of file of common words            */
            COMFILE;
char    INDEX[maxstr] =     /*  name of index file                      */
            INDXFILE;

int     argc;
char    **argv;

main(argcount,arglist)
int argcount;
char **arglist;
{   char *refs;
    char keys[maxstr];
    char *p,*q;
    char one_index[maxstr];

    argc= argcount-1;
    argv= arglist+1;
    flags();

    /*  add SYSINDEX to search path.  all names are comma terminated */
	strcat(INDEX, ",");
	strcat(INDEX, SYSINDEX);
	strcat(INDEX, ",");

    while (fgets(keys,maxstr,stdin)!=NULL)
    {   for (p = one_index, q = INDEX; *q != 0 ; q++)
	    if (*q == ',' )
	    {   *p = 0;
	        refs = locate(keys, one_index, max_klen, common);
		if( refs==NULL )
		{   fprintf(stderr,
			"%s removed from index list.\n", one_index);
		    /* delete this file name (shift remainder on top) */
			strcpy(q-strlen(one_index),q+1);
			q = q-strlen(one_index)-1;
		}
                if (refs!=NULL && *refs!=NULL) break;
	        p = one_index;
	    }
	    else *p++ = *q;

        if (refs==NULL || *refs==NULL)  printf("No references found.\n");
        else                            printf("%s", refs);
        if (refs!=NULL) free(refs);
    }
    exit(0);
}

# define    operand     (strlen(*argv+2)==0 ? (argv++,argc--,*argv) : *argv+2)

flags()
{   for (; argc>0 && *argv[0]=='-';  argc--,argv++)
    {   switch ((*argv)[1])
        {   case 'l':   max_klen= atoi(operand);
                        break;
            case 'c':   common=  operand;
                        break;
            case 'p':   strcpy(INDEX,operand);
                        break;
            default:    fprintf(stderr, "unknown flag '%s'\n", *argv);
        }
    }
}
