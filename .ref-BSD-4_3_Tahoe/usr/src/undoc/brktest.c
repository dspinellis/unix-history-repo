	int	a[1000];

main()
{
	int	asize = 1000,i,inc = 0,new;

	srand(1);
	for (i = 0 ; i < asize ; i++)
		a[i] = i;
	while (1)  {
		new = rand() % 4000;
		if (((rand() % 1000) > 750) && ((asize - new) > 1000))
			new = -new;
		for (i = 0 ;  i < asize ; i++ )
			if (a[i] != i)
				printf("bad a %x\n",i);
			if (sbrk(4 * new) != -1)  {
				if (new > 0)  {
					for (i = 0 ; i < new ; i++)
						a[asize + i] = asize + i;  }
				asize += new;  }
			else  {
				new = 1000 - asize;
				sbrk(4 * new);
				asize += new;  }
		sleep((rand() % 4) + 1);  }
}
