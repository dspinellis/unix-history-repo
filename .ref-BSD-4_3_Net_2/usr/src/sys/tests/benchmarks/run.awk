$2 == "syscall"	{
	ncalls = $3; getline; time = substr($2, 0, 4);
	print "System Call Overhead: ", (time * 1000000) / ncalls, "us"; }
