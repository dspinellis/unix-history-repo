_clenf (s) /* counts length of string */
char *s;
{
int n;
for (n=0; *s++ != '\0'; n++);
return (n);}
