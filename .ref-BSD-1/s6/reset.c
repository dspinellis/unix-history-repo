main()
{
	struct tty {
		char ispeed,ospeed;
		char erase,kill;
		int trash;
	} tet;
	gtty(2,&tet);
	tet.erase = 010;
	tet.kill = '@';
	tet.trash = 0332;
	stty(2,&tet);
	exit(0);
/*
 * reset - set the teletype mode bits to be sensible, erase to ^H, kill to @
 *
 * Kurt Shoens
 *
 * Very useful after crapping out in raw.
 */
}
