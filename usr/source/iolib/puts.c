puts(str)
char *str;
{
auto char *p,c;
auto int f;
extern int cout;
if (nargs() != 1)
   IEHzap("puts  ");
p = str;
while( (c = *p++)!= '\0') cputc(c,cout);
cputc('\n',cout);
return(str);
}
