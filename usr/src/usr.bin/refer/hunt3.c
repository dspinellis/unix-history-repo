# include "refer..c"
getq(v)
	char *v[];
{
# define BSIZ 250
static char buff[BSIZ];
static int eof = 0;
extern char *sinput;
char *p;
int c, n = 0, las = 0;
if (eof) return(-1);
p = buff;
while ( (c = (sinput ? *sinput++ : getchar()) ) > 0)
	{
	if (c== '\n')
		break;
	if (isalpha(c) || isdigit(c))
		{
		if (las==0)
			{
			v[n++] = p;
			las=1;
			}
		if (las++ <= 6)
			*p++ = c;
		}
	else
		{
		if (las>0)
			*p++ = 0;
		las=0;
		}
	}
*p=0;
assert(p<buff+BSIZ);
if (sinput==0 && c<= 0) eof=1;
n = keycomp(v,n);
# if D1
fprintf(stderr, "no. keys %d\n",n);
for(c=0; c<n; c++)
 fprintf(stderr, "keys X%sX\n", v[c]);
# endif
return(n);
}
keycomp(v,n) /* compress keys */
	char *v[];
{
int i, j, k;
for(i=j=0; i<n; i++)
	{
	for(k=0; k<j; k++)
		if (strcmp(v[i], v[k])==0)
			break;
	if (k<j) /* found it */
		continue;
	v[j++] = v[i];
	}
return(j);
}
