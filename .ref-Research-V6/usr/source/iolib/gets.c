gets (s)
char *s;
{ /* gets (s) - read a string with cgetc and store in s */
char *p;
extern int cin;
if (nargs () == 2)
	IEHzap("gets  ");
p=s;
while ((*s = cgetc(cin)) != '\n' && *s != '\0') s++;
if (*p == '\0') return (0);
*s = '\0';
return (p);
}
