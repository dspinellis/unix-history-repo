awk '{
	if (NR == 1) {
		printf "static double at1[256] = {\n"
		printf "\t%s,\n", $0
	} else if (NR == 256) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double at0[64] = {\n"
	} else if (NR == 320) {
		printf "\t%s\n", $0
		printf "};\n"
	} else
		printf "\t%s,\n", $0
}' $*
