lseek(f, o0, o1, p)
{
	register r;

	if((o0 == 0 && o1 >= 0) || (o0 == -1 && o1 < 0))
		return(seek(f, o1, p));
	if(p > 2)
		return(-1);
	r = (o0<<7) | ((o1>>9)&0177);
	if(seek(f, r, p+3) < 0)
		return(-1);
	return(seek(f, o1&0777, 1));
}
