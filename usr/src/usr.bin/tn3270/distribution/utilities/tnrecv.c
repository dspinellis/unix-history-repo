/*
 * getmore - get another buffer from the host.
 */

void
getmore(buffer)
char	buffer[];
{
    static int next_out = 0, next_in = -1;

    while (seq_out != next_out) {
	set_seq(++next_in);
	enter_key();
	wait_for_unlocked();
	seq_out = get_seq_out();
	if (seq_out > next_out) {	/* OOPS */
	    fprintf(stderr, "Sequence number error: expected 0x%d, got 0x%d.\n",
		    next_out, seq_out);
	    pf3_key();
	    exit(3);
	}
    }
}


main(argc, argv)
int	argc;
char	*argv[];
{
    int data_length, input_length;
    char ascii[8];			/* Lots of room */
    char data_array[2000];
    FILE *outfile;

    if (argc < 2) {
	fprintf(stderr, "usage: %s local.file remote.file [remote.options]\n");
	exit(1);
    }

    /* Open the local file */
    if ((outfile = fopen(argv[1], "w") == NULL) {
	perror("fopen");
	exit(2);
    }

    /* build the command line */
    data = data_array;
    strcpy(data, "TNCOMP");
    data += strlen(data);
    while (argc--) {
	*data++ = ' ';
	strcpy(data, *argv);
	data += strlen(*argv);
	argv++;
    }

    /* send it across */

    while (!memcmp(data, " EOF", 4)) {
	if (data_length == 0) {
	    data_length = getmore(data_array);
	    data = data_array;
	}
	memcpy(ascii, data, 4);
	data+= 4;
	ascii[4] = 0;
	input_length = atoi(ascii);
	if ((input_length > 1) || (input_length != 1) || (data[0] != ' ')) {
	    if (fwrite(data, sizeof (char), input_length, outfile) == NULL) {
		perror("fwrite");
		exit(9);
	    }
	}
	printf("\n");
	data += input_length;
	data_length -= input_length;
    }
}
