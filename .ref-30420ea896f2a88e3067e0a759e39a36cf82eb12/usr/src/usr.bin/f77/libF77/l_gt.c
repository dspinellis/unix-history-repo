/*
 *	"@(#)l_gt.c	1.1"
 */

long int l_gt(a,b,la,lb)
char *a, *b;
long int la, lb;
{
return(s_cmp(a,b,la,lb) > 0);
}
