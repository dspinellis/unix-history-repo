{
	lim = split ($0, line)
	out = ""
	if (lim > 0) {
		i = 0
		while (i < lim) {
			i++
			if (i % 2)
				out = out sprintf("%s ", toupper(line[i]))
			else
				out = out sprintf("%s ", tolower(line[i]))
		}
	}
	print out
}
