char c;
short w;
long l;

f() {
	if ((unsigned)c <= 0177)
		c = 0;
	if ((unsigned)w <= 0xefff)
		w = 0;
	if ((unsigned)l <= 0xefffffff)
		l = 0;
}
