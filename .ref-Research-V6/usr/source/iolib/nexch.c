IEH3nxch ()
/* returns next character which is not IEH3spce */
{
	char ch, cgetc();
	extern int IEH3sfil;
while ((ch = cgetc(IEH3sfil)) > 0 && IEH3spce(ch));
if  (ch > 0)
	return (ch);
return (-1);
}
