typedef	unsigned short dev_t;

main()
{
	dev_t x = (dev_t)(-1);

	if (x == (dev_t)(-1))
		printf("ok\n");
	else
		printf("nope\n");
}
