char *
valloc(npages, pagesize)
	int npages;
	register int pagesize;
{
	int off;
	char *base, *sbrk();

	base = sbrk(0);
	off = (int)base & (pagesize - 1);
	if (off) {
		(void) sbrk(pagesize - off);
		base += pagesize - off;
	}
	if (sbrk(npages * pagesize) == (char *)-1)
		return ((char *)0);
	return (base);
}
