BEGIN {
    for (i = 0; i <= 25; i++)
	printf "gawk sez -- square root of %2d is %15.12f\n", i, sqrt(i)
}
