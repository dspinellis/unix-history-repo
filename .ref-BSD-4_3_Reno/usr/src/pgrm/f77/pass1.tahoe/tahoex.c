#include <stdio.h>
#include "defines.h"
#include "machdefs.h"



prchars(fp, s)
FILEP fp;
int *s;
{

fprintf(fp, ".byte 0%o,0%o\n", s[0], s[1]);
}



pruse(fp, s)
FILEP fp;
char *s;
{
fprintf(fp, "\t%s\n", s);
}



prskip(fp, k)
FILEP fp;
ftnint k;
{
fprintf(fp, "\t.space\t%ld\n", k);
}





prcomblock(fp, name)
FILEP fp;
char *name;
{
fprintf(fp, LABELFMT, name);
}
