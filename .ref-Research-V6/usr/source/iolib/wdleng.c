wdleng ()
/* returns number of bits in a machine integer */
/* written so kernighan can tell where he is running */
{
int k, leng;
k = leng = 1;
while (k =<< 1)
	leng++;
return (leng);
}
