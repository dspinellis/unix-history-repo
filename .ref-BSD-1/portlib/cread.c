/**
 **	block read
 **/

cread(buff, itemlen, nitems, fn)
char	*buff;
int	itemlen, nitems;
int	fn;
{
	register int	n;
	register int	r;

	n = read(fn, buff, itemlen * nitems);
	r = n < 0 ? -1 : n / itemlen;
	return (r);
}
