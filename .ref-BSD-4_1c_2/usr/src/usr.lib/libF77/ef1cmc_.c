/* EFL support routine to compare two character strings */

long int ef1cmc_(a, la, b, lb)
int *a, *b;
long int *la, *lb;
{
return( s_cmp( (char *)a, (char *)b, *la, *lb) );
}
