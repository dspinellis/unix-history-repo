BEGIN {
	firstchar = "@";
}

{
	c = substr($2,2,1);
	if (c != firstchar)
		printf(".LB %s\n", c);
	firstchar = c;
	print;
}
