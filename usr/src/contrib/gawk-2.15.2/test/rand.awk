BEGIN {
	srand()
	for (i = 0; i < 19; i++) 
		printf "%3d ", (1 + int(100 * rand()))
	print ""
}
