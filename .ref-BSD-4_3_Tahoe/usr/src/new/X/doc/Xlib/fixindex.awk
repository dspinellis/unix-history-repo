BEGIN {
	FS = "\t";
}

NR == 1 {
	if ($3 != "")
		printf(".Ib \"%s\"\n", $2);
	major = $2;
	minor = $3;
	pageno = $1;
	pagelist = $1;
}

NR != 1 {
	if ($2 == major && $3 == minor)		# neither has changed
	{
		if ($1 != pageno)		# new page number, append
			pagelist = pagelist "," $1;
	}
	else					# one has changed
	{
		if (minor != "")		# dump full record
			printf(".I< \"%s\" \"%s\" \"%s\"\n", major, minor, pagelist);
		else
			printf(".I> \"%s\" \"%s\"\n", major, pagelist);
		pagelist = $1;			# restart pagelist
		if ($2 != major && $3 != "")	# major has changed, minor not null
			printf(".Ib \"%s\"\n", $2);
	}
	major = $2;
	minor = $3;
	pageno = $1;
}

END {
	if (minor != "")			# dump full record
		printf(".I< \"%s\" \"%s\" \"%s\"\n", major, minor, pagelist);
	else
		printf(".I> \"%s\" \"%s\"\n", major, pagelist);
}
