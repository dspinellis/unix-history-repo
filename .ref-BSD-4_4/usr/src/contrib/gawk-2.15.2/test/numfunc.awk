BEGIN {
	y = 8
	x = 1
	while (x < 256) {
		print "arctan", y/x, atan2(y , x)
		x += x
	}
	print ""
	pi8 = atan2(1, 1) / 2
	arg = 0
	for (i = 0; i <= 8; i++) {
		print "cos sin", arg, cos(arg), sin(arg)
		arg += pi8
	}
	print ""
	for (i = -5; i<= 5; i++) {
		print "exp log", i, exp(i), log(exp(i))
	}
}
