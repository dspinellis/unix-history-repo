# Clarify the flex debug trace by substituting first line of each rule.
# Francois Pinard <pinard@iro.umontreal.ca>, July 1990.
#
# Rewritten to process correctly \n's in scanner input.
# BEGIN section modified to correct a collection of rules.
# Michal Jaegermann <michal@phys.ualberta.ca>, December 1993
#
# Sample usage:
#	flex -d PROGRAM.l
#	gcc -o PROGRAM PROGRAM.c -lfl
#	PROGRAM 2>&1 | gawk -f debflex.awk PROGRAM.l
#
# (VP's note: this script presently does not work with either "old" or
#  "new" awk; fixes so it does will be welcome)

BEGIN {
    # Insure proper usage.

    if (ARGC != 2) {
	print "usage: gawk -f debflex.awk FLEX_SOURCE <DEBUG_OUTPUT";
	exit (1);
    }

    # Remove and save the name of flex source.

    source = ARGV[1];
    ARGC--;

    # Swallow the flex source file.

    line = 0;
    section = 1;
    while (getline <source) {

	# Count the lines.

	line++;

	# Count the sections.  When encountering section 3,
	# break out of the awk BEGIN block.

	if (match ($0, /^%%/)) {
	    section++;
	    if (section == 3) {
		break;
	    }
	}
	else {
	    # Only the lines in section 2 which do not begin in a
	    # tab or space might be referred to by the flex debug
	    # trace.  Save only those lines.

	    if (section == 2 && match ($0, /^[^ \t]/)) {
		rules[line] = $0;
	    }
	}
    }
    dashes = "-----------------------------------------------------------";
    collect = "";
    line = 0;
}

# collect complete rule output from a scanner
$0 !~ /^--/ {
    collect = collect "\n" $0;
    next;
}
# otherwise we have a new rule - process what we got so far
{
    process();
}
# and the same thing if we hit EOF
END {
    process();
}

function process() {

    # splitting this way we loose some double dashes and
    # left parentheses from echoed input - a small price to pay
    n = split(collect, field, "\n--|[(]");

    # this loop kicks in only when we already collected something
    for (i = 1; i <= n; i++) {
	if (0 != line) {
	    # we do not care for traces of newlines.
	    if (0 == match(field[i], /\"\n+\"[)]/)) {
		if (rules[line]) {
		    text = field[i];
		    while ( ++i <= n) {
			text = text field[i];
		    }
		    printf("%s:%d: %-8s -- %s\n",
			   source, line, text, rules[line]);
		}
		else {
		    print;
		    printf "%s:%d: *** No such rule.\n", source, line;
		}
	    }
	    line = 0;
	    break;
	}
	if ("" != field[i]) {
	    if ("end of buffer or a NUL)" == field[i]) {
		print dashes;  # Simplify trace of buffer reloads
		continue;
	    }
	    if (match(field[i], /accepting rule at line /)) {
		# force interpretation of line as a number
		line = 0 + substr(field[i], RLENGTH);
		continue;
	    }
	    # echo everything else
	    printf("--%s\n", field[i]);
	}
    }
    collect = "\n" $0;  # ... and start next trace
}
