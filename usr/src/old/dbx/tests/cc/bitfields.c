typedef unsigned int uint;

struct dot {
    uint  cost		 :24;
    uint  type	         : 3;
    uint  dirToCenter	 : 3;
    uint  pad		 : 1;
    uint  pin		 : 1;
    uint  traceback	 : 3;
    uint  traceforward   : 3;
    uint  expanded	 : 1;
    uint  underDir	 : 3;
    uint  underOffset	 : 4;
    uint  start		 : 1;
    uint  target	 : 1;
    uint  owner		 : 6;
    uint  segment	 : 7;
    uint  intrinsicCost  : 3;
};

main()
{
    struct dot junk;

    junk.owner = 63;
    junk.segment = 1;
    junk.intrinsicCost = 1;

    printf("owner = %d, segment = %d, intrinsicCost = %d\n",
	junk.owner, junk.segment, junk.intrinsicCost);
    printf("done\n");
    oldmain();
}

oldmain()
{
    struct {
	int first;
	int second;
	int a : 8;
	int b : 8;
	int c;
    } x;

    x.first = 0;
    x.second = 0;
    x.a = 2;
    x.b = 10;
    x.c = 1;
}
