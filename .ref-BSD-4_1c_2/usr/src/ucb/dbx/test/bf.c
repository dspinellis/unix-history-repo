main()
{
    struct {
	int first;
	int second;
	int a : 8;
	int b : 8;
	int c;
    } x;

    x.a = 2;
    x.b = 10;
    x.c = 1;
}
