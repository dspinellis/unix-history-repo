# define bufflen 100
IEH3revp (c)
char c;
{
/* reversed line IEH3outputter */
extern char *IEH3outp, *IEH3outlim;
*IEH3outp++ = c;
if (IEH3outp > IEH3outlim+100)
	IEH3err("unprint producing too many chars");
}
