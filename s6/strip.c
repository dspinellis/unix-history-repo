/*
 * strip - strip object module
 *
 * Modified by Jeff Schriebman UCB
 * to have -t flag to use to strip /unix
 * since it is on a small file system.
 *
 * Modified by Bill Joy UCB
 * to once again take more than one argument
 */
struct {
	int	magic;
	int	txt_size;
	int	data_size;
	int	bss_size;
	int	sym_size;
	int	entry_loc;
	int	unused;
	int	no_reloc;
} header, new_header;

char	tflg;
char	*tmp;

char	*in, *out;


main(argc, argv)
	int argc;
	char *argv[];
{
	register char *name, *cp;

	argc--, argv++;
	if (argc > 0 && argv[0][0] == '-') {
		if (argv[0][1] != 't') {
			Perror("Usage: ", "strip [ -t ] name ...");
			exit(1);
		}
		tflg++;
		argc--, argv++;
	}
	if (argc == 0)
		*--argv = "a.out", argc++;
	while (argc > 0) {
		argc--, name = *argv++;
		close(0);
		if (open(name, 0) < 0) {
			perror(name);
			exit(1);
		}
		close(1);
		cp = "/tmp/stma";
		cp[8] = 'a';
		for (;;) {
			if (creat(tmp, 0400) > 0)
				break;
			if (tmp[8] == 'z') {
				perror(tmp);
				exit(1);
			}
			tmp[8]++;
		}
		if (read(0, &header, sizeof header) != sizeof header) {
			Perror(name, "Error reading header");
			unlink(tmp);
			continue;
		}
		if (header.magic < 0407 || header.magic > 0411) {
			Perror(name, "Not object file");
			unlink(tmp);
			continue;
		}
		copy(&new_header, &header, sizeof new_header);
		if (tflg) {
			new_header.txt_size = 0;
			new_header.data_size = 0;
			new_header.bss_size = 0;
		} else {
			new_header.sym_size = 0;
			new_header.no_reloc = 1;
		}
		in = name;
		out = tmp;
		if (write(1, &new_header, sizeof new_header) != sizeof new_header) {
			perror(tmp);
			unlink(tmp);
			exit(1);
		}
		if (tflg == 0) {
			copy01(header.txt_size);
			copy01(header.data_size);
		} else {
			lseek(0, 0, header.txt_size, 1);
			lseek(0, 0, header.data_size, 1);
			if (header.no_reloc == 0) {
				lseek(0, 0, header.txt_size, 1);
				lseek(0, 0, header.data_size, 1);
			}
			copy01(header.sym_size);
		}
		close(0);
		if (open(tmp, 0) < 0) {
			perror(tmp);
			unlink(tmp);
			exit(1);
		}
		close(1);
		if (creat(name, 0644) < 0) {
			perror(name);
			unlink(tmp);
			exit(1);
		}
		in = tmp;
		out = name;
		copy01(sizeof new_header);
		copy01(new_header.txt_size);
		copy01(new_header.data_size);
		copy01(new_header.sym_size);
		unlink(tmp);
	}
}

Perror(file, mesg)
{
	extern int errno;
	extern char *sys_errlist[];

	sys_errlist[0] = mesg;
	errno = 0;
	perror(file);
}

copy(to, from, bytes)
	register char *to, *from;
	register int bytes;
{

	if (bytes > 0)
		do
			*to++ = *from++;
		while (--bytes != 0);
}

copy01(ct)
	int ct;
{
	register int i, j, n;
	char buf[512];

	for (i = ct; i != 0; i =- j) {
		if (i > 512 || i < 0)
			j = 512;
		else
			j = i;
		if (read(0, buf, j) != j) {
			perror(in);
			unlink(tmp);
			exit(1);
		}
		if (write(1, buf, j) != j) {
			perror(out);
			unlink(tmp);
			exit(1);
		}
	}
}
