main()
{
	unsigned short a = 0, b = 65532;
	register short d;

	d += (char)(a-b);
	d += (unsigned char)(a-b);
}
