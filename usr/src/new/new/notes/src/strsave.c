static char *sccsid = "@(#)strsave.c	1.1\t1/23/83";

char *malloc();

char *strsave(s)
char *s;
{
    char *p;

    p = malloc(strlen(s)+1);
	
    strcpy(p,s);

    return(p);
}
