cat $* | sed -e '/.*\..*/!s/$/.0/' | awk '{
	if (NR == 1) {
		printf "static double rn0[256] = {\n"
		printf "\t%s,\n", $0
	} else if (NR == 256) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double ln0[256] = {\n"
	} else if (NR == 512) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double rn1[256] = {\n"
	} else if (NR == 768) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double ln1[256] = {\n"
	} else if (NR == 1024) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double rp0[256] = {\n"
	} else if (NR == 1280) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double lp0[256] = {\n"
	} else if (NR == 1536) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double rp1[128] = {\n"
	} else if (NR == 1664) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double lp1[128] = {\n"
	} else if (NR == 1792) {
		printf "\t%s\n", $0
		printf "};\n\n"
		printf "static double le[256] = {\n"
	} else if (NR == 2048) {
		printf "\t%s\n", $0
		printf "};\n"
	} else
		printf "\t%s,\n", $0
}'
