cat $* | sed -e "s/\(.*\)\(........\)/0x\1, 0x\2/" | awk '
	BEGIN {
		printf "struct doubleword {\n"
		printf "\tunsigned lw0,\n"
		printf "\t         lw1;\n"
		printf "};\n\n"
	}
	{
	if (NR == 1) {
		printf "struct doubleword _ep0[12] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
		printf "\t%s,\n", $0
	} else if (NR == 11) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _ep1[128] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 138) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _ep2[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 393) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _ep3[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 648) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _ep4[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 903) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _en0[12] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 914) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _en1[128] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 1041) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _en2[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 1296) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _en3[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 1551) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "struct doubleword _en4[256] = {\n"
		printf "\t0x40800000, 0x00000000,\n"
	} else if (NR == 1806) {
		printf "\t%s\n", $0
		printf "};\n"
	} else
		printf "\t%s,\n", $0
}'
