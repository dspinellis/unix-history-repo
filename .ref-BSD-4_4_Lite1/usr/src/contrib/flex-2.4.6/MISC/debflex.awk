# Clarify the flex debug trace by substituting first line of each rule.
# Francois Pinard <pinard@iro.umontreal.ca>, July 1990.
#
# Sample usage:
#	flex -d PROGRAM.l
#	gcc -o PROGRAM PROGRAM.c -lfl
#	PROGRAM 2>&1 | gawk -f debflex.awk PROGRAM.l
#
# (VP's note: this script presently does not work with either "old" or
#  "new" awk [I haven't tried mawk]; fixes so it does will be welcome)

BEGIN {
	# Insure proper usage.

	if (ARGC != 2) {
		print "usage: gawk -f debflex.awk FLEX_SOURCE <DEBUG_OUTPUT"
		exit (1)
	}

	# Remove and save the name of flex source.

	source = ARGV[1]
	ARGC--

	# Swallow the flex source file.

	line = 0
	section = 1
	while (getline <source) {

		# Count the lines.

		line++

		# Count the sections.  When encountering section 3,
		# break out of the awk BEGIN block.

		if (match ($0, /^%%/)) {
			section++
			if (section == 3) {
				break
			}
		}

		# Only the lines in section 2 which do not begin in a
		# tab or space might be referred to by the flex debug
		# trace.  Save only those lines.

		if (section == 2 && match ($0, /[^ \t]/)) {
			rules[line] = $0
		}
	}
}

# Simplify trace of buffer reloads.

/^--\(end of buffer or a NUL\)/ {
	print "-----------------------------------------------------------"
	next
}

# Remove trace of newlines.  This is debatable, but adequate for the
# precise application this was developped for.

/^--accepting rule at line [0-9]+ \("$/ {
	next
}

/^"\)$/ {
	next
}

# Modify other trace lines to ease GNU emacs next-error processing,
# also insert the related first line of flex source.

/^--accepting rule at line [0-9]+ \(".*"\)$/ {
	if (rules[$5]) {
		string = substr ($0, index ($0, "(") + 1)
		string = substr (string, 1, length (string) - 1)
		printf "%s(%d): %-8s -- %s\n", source, $5, string, rules[$5]
	}
	else {
		print
		printf "%s(%d): *** No such rule.\n", source, $5
	}
	next
}

# Copy everything else verbatim.

	{ print }
