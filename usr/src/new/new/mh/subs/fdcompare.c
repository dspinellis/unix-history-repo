fdcompare(fd1, fd2)
{
	int n1, n2, resp;
	register int i;
	register char *c1, *c2;
	char b1[512], b2[512];

	resp = 1;
	while((n1 = read(fd1, b1, 512)) >= 0 &&
	      (n2 = read(fd2, b2, 512)) >= 0 &&
	       n1 == n2) {

		c1 = b1; c2 = b2;
		for(i = n1 < 512? n1 : 512; i--; )
			if(*c1++ != *c2++) {
				resp = 0;
				goto leave;
			}
		if(n1 < 512)
			goto leave;
	}
	resp = 0;
leave:
	lseek(fd1, 0l, 0);
	lseek(fd2, 0l, 0);
	return(resp);
}
