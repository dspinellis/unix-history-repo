#
/*
 * size - determine object size
 *
 * Modified by Bill Joy UCB August 23, 1977
 * to work correctly on "large" objects by using longs.
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
} header;

long	txt_size;
long	data_size;
long	bss_size;

long	total;
long	longof();

main(Argc, argv)
	int Argc;
	char *argv[];
{
	register char *name;
	extern int fout;
	register int argc;

	fout = dup(1);
	Argc--, argv++;
	argc = Argc;
	do {
		close(0);
		if (argc > 0)
			argc--, name = *argv++;
		else
			name = "a.out";
		if (open(name, 0) < 0) {
			perror(name);
			continue;
		}
		if (read(0, &header, sizeof header) != sizeof header) {
			Perror(name, "Can't read header");
			continue;
		}
		if (header.magic < 0407 || header.magic > 0411) {
			Perror(name, "Not object file");
			continue;
		}
		if (Argc > 1)
			printf("%s: ", name);
		txt_size = longof(0, header.txt_size);
		data_size = longof(0, header.data_size);
		bss_size = longof(0, header.bss_size);
		printf("%ld+%ld+%ld=", txt_size, data_size, bss_size);
		total = txt_size + data_size + bss_size;
		printf("%ld (%lo)\n", total, total);
		flush();
	} while (argc > 0);
}

Perror(name, mesg)
	char *name, *mesg;
{
	extern char *sys_errlist[];
	extern int errno;

	sys_errlist[0] = mesg;
	errno = 0;
	perror(name);
}

long
longof(l)
	long l;
{

	return (l);
}
