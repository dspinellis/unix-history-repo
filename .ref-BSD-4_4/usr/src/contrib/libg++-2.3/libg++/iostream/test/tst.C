#include "iostream.h"
#include "stdio.h"

main(int argc, char** argv)
{
    for (int i = 0; i < argc; i++) {
	fprintf(stdout, "arg%d=%s ", i, argv[i]);
	cout << "[" << i << " " << argv[i] << "]\n";	
    }
    if (argc > 1) {
	FILE * stream = fopen(argv[1], "r");
	if (stream == NULL) {
	    fprintf(stderr, "Failed to open: %s\n", argv[1]);
	    return 0;
	}
	for (;;) {
	    int ch = getc(stream);
	    if (ch == EOF)
		break;
	    putc(ch, stdout);
	}
    }
}
