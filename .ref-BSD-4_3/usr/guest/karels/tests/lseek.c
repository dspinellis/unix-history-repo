main()
{
	if (lseek(2,-1,0) < 0)
		perror("lseek");
}
