# input file should have lines which start with "@incl" followed by
# a name of a file to include
{
	if ((NF == 2) && ($1 == "@incl")) {
		print "  -- included file --  ", $2
		while ((getline line < $2) > 0)
			print line
		close ($2)
		printf "\t***\n"
	} else {
		print
	}
}
