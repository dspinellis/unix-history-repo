main()
{
char *ttyname() ;
register char *tty , *cp ;
 
cp  = ttyname(0) ;
cp++ ;
while (*cp++ != '/') ;
tty = cp ;
while (*cp)cp++ ;
*++cp = '\n' ;
*++cp = '\0' ;
write(1,tty,10) ;
}
