/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)fsdb.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 *  fsdb - file system debugger
 *
 *  usage: fsdb [options] special
 *  options:
 *	-?		display usage
 *	-o		override some error conditions
 *	-p"string"	set prompt to string
 *	-w		open for write
 */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <ufs/fs.h>
#include <ufs/dinode.h>
#include <stdio.h>
#include <setjmp.h>
#include <paths.h>

/*
 * Defines from the 4.3-tahoe file system, for systems with the 4.2 or 4.3
 * file system.
 */
#ifndef FS_42POSTBLFMT
#define cg_blktot(cgp) (((cgp))->cg_btot)
#define cg_blks(fs, cgp, cylno) (((cgp))->cg_b[cylno])
#define cg_inosused(cgp) (((cgp))->cg_iused)
#define cg_blksfree(cgp) (((cgp))->cg_free)
#define cg_chkmagic(cgp) ((cgp)->cg_magic == CG_MAGIC)
#endif

/*
 * Never changing defines.
 */
#define	OCTAL		8		/* octal base */
#define	DECIMAL		10		/* decimal base */
#define	HEX		16		/* hexadecimal base */

/*
 * Adjustable defines.
 */
#define	NBUF		10		/* number of cache buffers */
#define PROMPTSIZE	80		/* size of user definable prompt */
#define	MAXFILES	40000		/* max number of files ls can handle */
#define FIRST_DEPTH	10		/* default depth for find and ls */
#define SECOND_DEPTH	100		/* second try at depth (maximum) */
#define INPUTBUFFER	1040		/* size of input buffer */
#define BYTESPERLINE	16		/* bytes per line of /dxo output */
#define	NREG		36		/* number of save registers */

/*
 * Values dependent on sizes of structs and such.
 */
#define NUMB		3			/* these three are arbitrary, */
#define	BLOCK		5			/* but must be different from */
#define	FRAGMENT	7			/* the rest (hence odd).      */
#define	BITSPERCHAR	8			/* couldn't find it anywhere  */
#define	CHAR		(sizeof (char))
#define	SHORT		(sizeof (short))
#define	LONG		(sizeof (long))
#define	INODE		(sizeof (struct dinode))
#define	DIRECTORY	(sizeof (struct direct))
#define	CGRP		(sizeof (struct cg))
#define SB		(sizeof (struct fs))
#define	BLKSIZE		(fs->fs_bsize)		/* for clarity */
#define	FRGSIZE		(fs->fs_fsize)
#define	BLKSHIFT	(fs->fs_bshift)
#define	FRGSHIFT	(fs->fs_fshift)

/*
 * Messy macros that would otherwise clutter up such glamorous code.
 */
#define itob(i)		((itod(fs, (i)) << FRGSHIFT) + itoo(fs, (i)) * INODE)
#define min(x, y)	((x) < (y) ? (x) : (y))
#define	STRINGSIZE(d)	((long)d->d_reclen - \
				((long)&d->d_name[0] - (long)&d->d_ino))
#define	letter(c)	((((c) >= 'a')&&((c) <= 'z')) ||\
				(((c) >= 'A')&&((c) <= 'Z')))
#define	digit(c)	(((c) >= '0') && ((c) <= '9'))
#define HEXLETTER(c)	(((c) >= 'A') && ((c) <= 'F'))
#define hexletter(c)	(((c) >= 'a') && ((c) <= 'f'))
#define octaldigit(c)	(((c) >= '0') && ((c) <= '7'))
#define uppertolower(c)	((c) - 'A' + 'a')
#define hextodigit(c)	((c) - 'a' + 10)
#define	numtodigit(c)	((c) - '0')
#define loword(X)	(((ushort *)&X)[1])
#define lobyte(X)	(((unsigned char *)&X)[1])

/*
 * buffer cache structure.
 */
struct buf {
	struct	buf  *fwd;
	struct	buf  *back;
	char	*blkaddr;
	short	valid;
	long	blkno;
} buf[NBUF], bhdr;

/*
 * used to hold save registers (see '<' and '>').
 */
struct	save_registers {
	long	sv_addr;
	long	sv_value;
	long	sv_objsz;
} regs[NREG];

/*
 * cd, find, and ls use this to hold filenames.  Each filename is broken
 * up by a slash.  In other words, /usr/src/adm would have a len field
 * of 2 (starting from 0), and filenames->fname[0-2] would hold usr,
 * src, and adm components of the pathname.
 */
struct filenames {
	long	ino;		/* inode */
	long	len;		/* number of components */
	char	flag;		/* flag if using SECOND_DEPTH allocator */
	char	find;		/* flag if found by find */
	char	**fname;	/* hold components of pathname */
} *filenames, *top;

struct fs filesystem, *fs;	/* super block */

/*
 * Global data.
 */
char		*input_path[MAXPATHLEN];
char		*stack_path[MAXPATHLEN];
char		*current_path[MAXPATHLEN];
char		input_buffer[INPUTBUFFER];
char		*prompt;
char		*buffers;
char		scratch[64];
char		BASE[] = "o u     x";
char		PROMPT[PROMPTSIZE] = "> ";
char		laststyle = '/';
char		lastpo = 'x';
short		input_pointer;
short		current_pathp;
short		stack_pathp;
short		input_pathp;
short		cmp_level;
short		nfiles;
short		type = NUMB;
short		dirslot;
short		fd;
short		c_count;
short		error;
short		paren;
short		trapped;
short		doing_cd;
short		doing_find;
short		find_by_name;
short		find_by_inode;
short		long_list;
short		recursive;
short		objsz = SHORT;
short		override = 0;
short		wrtflag;
short		base = HEX;
short		acting_on_inode;
short		acting_on_directory;
short		should_print = 1;
short		clear;
short		star;
long		addr;
long		bod_addr;
long		value;
long		erraddr;
long		errcur_bytes;
long		errino;
long		errinum;
long		cur_cgrp;
long		cur_ino;
long		cur_inum;
long		cur_dir;
long		cur_block;
long		cur_bytes;
long		find_ino;
long		filesize;
long		blocksize;
long		stringsize;
long		count = 1;
long		commands;
long		read_requests;
long		actual_disk_reads;
jmp_buf		env;

extern char	*malloc(), *calloc();
char		getachar();
char		*getblk(), *fmtentry();
int		err();
long		get(), bmap(), expr(), term(), getnumb();
unsigned long	*print_check();

/*
 * main - lines are read up to the unprotected ('\') newline and
 *	held in an input buffer.  Characters may be read from the
 *	input buffer using getachar() and unread using ungetachar().
 *	Reading the whole line ahead allows the use of debuggers
 *	which would otherwise be impossible since the debugger
 *	and fsdb could not share stdin.
 */

main(argc,argv)
	short			argc;
	char			**argv;
{

	register char		c, *cptr;
	register short		i, j, *iptr;
	register struct direct	*dirp;
	register struct buf	*bp;
	struct filenames	*fn;
	char			*progname;
	short			colon, mode;
	long			temp;
	unsigned		block;
	int			ffcmp();

	setbuf(stdin, NULL);

	progname = argv[0];
	prompt = &PROMPT[0];
	/*
	 * Parse options.
	 */
	while (argc>1 && argv[1][0] == '-') {
		if (strcmp("-?", argv[1]) == 0)
			goto usage;
		if (strcmp("-o", argv[1]) == 0) {
			printf("error checking off\n");
			override = 1;
			argc--; argv++;
			continue;
		}
		if (strncmp("-p", argv[1],2) == 0) {
			prompt = &argv[1][2];
			argc--; argv++;
			continue;
		}
		if (strcmp("-w", argv[1]) == 0) {
			wrtflag = 2;		/* suitable for open */
			argc--; argv++;
			continue;
		}
	}
	if (argc!=2) {
usage:
		printf("usage:   %s [options] special\n", progname);
		printf("options:\n");
		printf("\t-?		display usage\n");
		printf("\t-o		override some error conditions\n");
		printf("\t-p\"string\"	set prompt to string\n");
		printf("\t-w		open for write\n");
		exit(1);
	}
	/*
	 * Attempt to open the special file.
	 */
	if ((fd = open(argv[1],wrtflag)) < 0) {
		perror(argv[1]);
		exit(1);
	}
	/*
	 * Read in the super block and validate (not too picky).
	 */
	if (lseek(fd, SBLOCK * DEV_BSIZE, 0) == -1) {
		perror(argv[1]);
		exit(1);
	}
	if (read(fd, &filesystem, sizeof filesystem) != sizeof filesystem) {
		printf("%s: cannot read superblock\n", argv[1]);
		exit(1);
	}
	fs = &filesystem;
	if (fs->fs_magic != FS_MAGIC) {
		printf("%s: Bad magic number in file system\n", argv[1]);
		exit(1);
	}
#ifdef FS_42POSTBLFMT
	if (fs->fs_postblformat == FS_42POSTBLFMT)
		fs->fs_nrpos = 8;
#endif
	printf("fsdb of %s %s -- last mounted on %s\n",
		argv[1], wrtflag ? "(Opened for write)" : "(Read only)", 
		&fs->fs_fsmnt[0]);
	/*
	 * Malloc buffers and set up cache.
	 */
	buffers = malloc(NBUF * BLKSIZE);
	bhdr.fwd = bhdr.back = &bhdr;
	for (i=0; i<NBUF; i++) {
		bp = &buf[i];
		bp->blkaddr = buffers + (i * BLKSIZE);
		bp->valid = 0;
		insert(bp);
	}
	/*
	 * Malloc filenames structure.  The space for the actual filenames
	 * is allocated as it needs it.
	 */
	filenames = (struct filenames *)calloc(MAXFILES,
						sizeof (struct filenames));
	if (filenames == NULL) {
		printf("out of memory\n");
		exit(1);
	}

	fn = filenames;

	restore_inode(2);
	/*
	 * Malloc a few filenames (needed by pwd for example).
	 */
	for (i = 0; i < MAXPATHLEN; i++) {
		input_path[i] = calloc(1, MAXNAMLEN);
		stack_path[i] = calloc(1, MAXNAMLEN);
		current_path[i] = calloc(1, MAXNAMLEN);
		if (current_path[i] == NULL) {
			printf("out of memory\n");
			exit(1);
		}
	}
	current_pathp = -1;

	signal(2,err);
	setjmp(env);

	getnextinput();
	/*
	 * Main loop and case statement.  If an error condition occurs
	 * initialization and recovery is attempted.
	 */
	for (;;) {
		if (error) {
			freemem(filenames, nfiles);
			nfiles = 0;
			c_count = 0;
			count = 1;
			star = 0;
			error = 0;
			paren = 0;
			acting_on_inode = 0;
			acting_on_directory = 0;
			should_print = 1;
			addr = erraddr;
			cur_ino = errino;
			cur_inum = errinum;
			cur_bytes = errcur_bytes;
			printf("?\n");
			getnextinput();
			if (error)
				continue;
		}
		c_count++;

		switch (c = getachar()) {

		case '\n': /* command end */
			freemem(filenames, nfiles);
			nfiles = 0;
			if (should_print && laststyle == '=') {
				ungetachar(c);
				goto calc;
			}
			if (c_count == 1) {
				clear = 0;
				should_print = 1;
				erraddr = addr;
				errino = cur_ino;
				errinum = cur_inum;
				errcur_bytes = cur_bytes;
				switch (objsz) {
				case DIRECTORY:
					if ((addr =
					     getdirslot(dirslot+1)) == 0)
						should_print = 0;
					if (error) {
						ungetachar(c);
						continue;
					}
					break;
				case INODE:
					cur_inum++;
					addr = itob(cur_inum);
					if (!icheck(addr)) {
						cur_inum--;
						should_print = 0;
					}
					break;
				case CGRP:
				case SB:
					cur_cgrp++;
					if ((addr=cgrp_check(cur_cgrp)) == 0) {
					     cur_cgrp--;
					     continue;
					}
					break;
				default:
					addr += objsz;
					cur_bytes += objsz;
					if (valid_addr() == 0)
						continue;
				}
			}
			if (type == NUMB)
				trapped = 0;
			if (should_print)
				switch (objsz) {
				case DIRECTORY:
					fprnt('?', 'd');
					break;
				case INODE:
					fprnt('?', 'i');
					if (!error)
						cur_ino = addr;
					break;
				case CGRP:
					fprnt('?', 'c');
					break;
				case SB:
					fprnt('?', 's');
					break;
				case CHAR:
				case SHORT:
				case LONG:
					fprnt(laststyle, lastpo);
				}
			if (error) {
				ungetachar(c);
				continue;
			}
			c_count = colon = acting_on_inode = 0;
			acting_on_directory = 0;
			should_print = 1;
			getnextinput();
			if (error)
				continue;
			erraddr = addr;
			errino = cur_ino;
			errinum = cur_inum;
			errcur_bytes = cur_bytes;
			continue;

		case '(': /* numeric expression or unknown command */
		default:
			colon = 0;
			if (digit(c) || c == '(') {
				ungetachar(c);
				addr = expr();
				type = NUMB;
				value = addr;
				continue;
			}
			printf("unknown command or bad syntax\n");
			error++;
			continue;

		case '?': /* general print facilities */
		case '/':
			fprnt(c, getachar());
			continue;

		case ';': /* command separator and . */
		case '\t':
		case ' ':
		case '.':
			continue;

		case ':': /* command indicator */
			colon++;
			commands++;
			should_print = 0;
			stringsize = 0;
			trapped = 0;
			continue;

		case ',': /* count indicator */
			colon = star = 0;
			if ((c = getachar()) == '*') {
				star = 1;
				count = BLKSIZE;
			} else {
				ungetachar(c);
				count = expr();
				if (error)
					continue;
				if (!count)
					count = 1;
			}
			clear = 0;
			continue;

		case '+': /* address addition */
			colon = 0;
			c = getachar();
			ungetachar(c);
			if (c == '\n')
				temp = 1;
			else {
				temp = expr();
				if (error)
					continue;
			}
			erraddr = addr;
			errcur_bytes = cur_bytes;
			switch (objsz) {
			case DIRECTORY:
				addr = getdirslot(dirslot + temp);
				if (error)
					continue;
				break;
			case INODE:
				cur_inum += temp;
				addr = itob(cur_inum);
				if (!icheck(addr)) {
					cur_inum -= temp;
					continue;
				}
				break;
			case CGRP:
			case SB:
				cur_cgrp += temp;
				if ((addr = cgrp_check(cur_cgrp)) == 0) {
					cur_cgrp -= temp;
					continue;
				}
				break;
			default:
				laststyle = '/';
				addr += temp * objsz;
				cur_bytes += temp * objsz;
				if (valid_addr() == 0)
					continue;
			}
			value = get(objsz);
			continue;

		case '-': /* address subtraction */
			colon = 0;
			c = getachar();
			ungetachar(c);
			if (c == '\n')
				temp = 1;
			else {
				temp = expr();
				if (error)
					continue;
			}
			erraddr = addr;
			errcur_bytes = cur_bytes;
			switch (objsz) {
			case DIRECTORY:
				addr = getdirslot(dirslot - temp);
				if (error)
					continue;
				break;
			case INODE:
				cur_inum -= temp;
				addr = itob(cur_inum);
				if (!icheck(addr)) {
					cur_inum += temp;
					continue;
				}
				break;
			case CGRP:
			case SB:
				cur_cgrp -= temp;
				if ((addr = cgrp_check(cur_cgrp)) == 0) {
					cur_cgrp += temp;
					continue;
				}
				break;
			default:
				laststyle = '/';
				addr -= temp * objsz;
				cur_bytes -= temp * objsz;
				if (valid_addr() == 0)
					continue;
			}
			value = get(objsz);
			continue;

		case '*': /* address multiplication */
			colon = 0;
			temp = expr();
			if (error)
				continue;
			if (objsz != INODE && objsz != DIRECTORY)
				laststyle = '/';
			addr *= temp;
			value = get(objsz);
			continue;

		case '%': /* address division */
			colon = 0;
			temp = expr();
			if (error)
				continue;
			if (!temp) {
				printf("divide by zero\n");
				error++;
				continue;
			}
			if (objsz != INODE && objsz != DIRECTORY)
				laststyle = '/';
			addr /= temp;
			value = get(objsz);
			continue;

		case '=': { /* assignment operation */
			short tbase = base;

calc:
			c = getachar();
			if (c == '\n') {
				ungetachar(c);
				c = lastpo;
				if (acting_on_inode == 1) {
					if (c != 'o' && c != 'd' && c != 'x' &&
					    c != 'O' && c != 'D' && c != 'X') {
						switch (objsz) {
						case LONG:
							c = lastpo = 'X';
							break;
						case SHORT:
							c = lastpo = 'x';
							break;
						case CHAR:
							c = lastpo = 'c';
						}
					}
				} else {
					if (acting_on_inode == 2)
						c = lastpo = 't';
				}
			} else if (acting_on_inode)
				lastpo = c;
			should_print = star = 0;
			count = 1;
			erraddr = addr;
			errcur_bytes = cur_bytes;
			switch (c) {
			case '"': /* character string */
				if (type == NUMB) {
					blocksize = BLKSIZE;
					filesize = BLKSIZE * 2;
					cur_bytes = blkoff(fs, addr);
					if (objsz==DIRECTORY || objsz==INODE)
						lastpo = 'X';
				}
				puta();
				continue;
			case '+': /* =+ operator */
				temp = expr();
				value = get(objsz);
				if (!error)
					put(value+temp,objsz);
				continue;
			case '-': /* =- operator */
				temp = expr();
				value = get(objsz);
				if (!error)
					put(value-temp,objsz);
				continue;
			case 'b':
			case 'c':
				if (objsz == CGRP)
					fprnt('?', c);
				else
					fprnt('/', c);
				continue;
			case 'i':
				addr = cur_ino;
				fprnt('?', 'i');
				continue; 
			case 's':
				fprnt('?', 's');
				continue;
			case 't':
			case 'T':
				laststyle = '=';
				printf("\t\t");
				printf("%s", ctime(&value));
				continue;
			case 'o':
				base = OCTAL;
				goto otx;
			case 'd':
				if (objsz == DIRECTORY) {
					addr = cur_dir;
					fprnt('?', 'd');
					continue;
				}
				base = DECIMAL;
				goto otx;
			case 'x':
				base = HEX;
otx:
				laststyle = '=';
				printf("\t\t");
				if (acting_on_inode)
					print(value & 0177777L, 12, -8, 0);
				else
					print(addr & 0177777L, 12, -8, 0);
				printf("\n");
				base = tbase;
				continue;
			case 'O':
				base = OCTAL;
				goto OTX;
			case 'D':
				base = DECIMAL;
				goto OTX;
			case 'X':
				base = HEX;
OTX:
				laststyle = '=';
			 	printf("\t\t");
				if (acting_on_inode)
					print(value, 12, -8, 0);
				else
					print(addr, 12, -8, 0);
				printf("\n");
				base = tbase;
				continue;
			default: /* regular assignment */
				ungetachar(c);
				value = expr();
				if (error)
					printf("syntax error\n");
				else
					put(value,objsz);
				continue;
			}
		}

		case '>': /* save current address */
			colon = 0;
			should_print = 0;
			c = getachar();
			if (!letter(c) && !digit(c)) {
				printf("invalid register specification, ");
				printf("must be letter or digit\n");
				error++;
				continue;
			}
			if (letter(c)) {
				if (c < 'a')
					c = uppertolower(c);
				c = hextodigit(c);
			} else
				c = numtodigit(c);
			regs[c].sv_addr = addr;
			regs[c].sv_value = value;
			regs[c].sv_objsz = objsz;
			continue;

		case '<': /* restore saved address */
			colon = 0;
			should_print = 0;
			c = getachar();
			if (!letter(c) && !digit(c)) {
				printf("invalid register specification, ");
				printf("must be letter or digit\n");
				error++;
				continue;
			}
			if (letter(c)) {
				if (c < 'a')
					c = uppertolower(c);
				c = hextodigit(c);
			} else
				c = numtodigit(c);
			addr = regs[c].sv_addr;
			value = regs[c].sv_value;
			objsz = regs[c].sv_objsz;
			continue;

		case 'a':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("at", 2)) { 		/* access time */
				acting_on_inode = 2;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_atime;
				value = get(LONG);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'b':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("block", 2)) { 	/* block conversion */
				if (type == NUMB) {
					value = addr;
					cur_bytes = 0;
					blocksize = BLKSIZE;
					filesize = BLKSIZE * 2;
				}
				addr = value << FRGSHIFT;
				bod_addr = addr;
				value = get(LONG);
				type = BLOCK;
				dirslot = 0;
				trapped++;
				continue;
			}
			if (match("bs", 2)) {		/* block size */
				acting_on_inode = 1;
				should_print = 1;
				if (icheck(cur_ino) == 0)
					continue;
				addr = (long)
					&((struct dinode *)cur_ino)->di_blocks;
				value = get(LONG);
				type = NULL;
				continue;
			}
			if (match("base", 2)) {		/* change/show base */
showbase:
				if ((c = getachar()) == '\n') {
					ungetachar(c);
					printf("base =\t\t");
					switch (base) {
					case OCTAL:
						printf("OCTAL\n");
						continue;
					case DECIMAL:
						printf("DECIMAL\n");
						continue;
					case HEX:
						printf("HEX\n");
						continue;
					}
				}
				if (c != '=') {
					printf("missing '='\n");
					error++;
					continue;
				}
				value = expr();
				switch (value) {
				default:
					printf("invalid base\n");
					error++;
					break;
				case OCTAL:
				case DECIMAL:
				case HEX:
					base = value;
				}
				goto showbase;
			}
			goto bad_syntax;

		case 'c':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("cd", 2)) {		/* change directory */
				top = filenames - 1;
				eat_spaces();
				if ((c = getachar()) == '\n') {
					ungetachar(c);
					current_pathp = -1;
					restore_inode(2);
					continue;
				}
				ungetachar(c);
				temp = cur_inum;
				doing_cd = 1;
				parse();
				doing_cd = 0;
				if (nfiles != 1) {
					restore_inode(temp);
					if (!error) {
						print_path(input_path,
								input_pathp);
						if (nfiles == 0)
							printf(" not found\n");
						else
							printf(" ambiguous\n");
						error++;
					}
					continue;
				}
				restore_inode(filenames->ino);
				if ((mode = icheck(addr)) == 0)
					continue;
				if ((mode & IFMT) != IFDIR) {
					restore_inode(temp);
					print_path(input_path, input_pathp);
					printf(" not a directory\n");
					error++;
					continue;
				}
				for (i = 0; i <= top->len; i++)
					strcpy(current_path[i],
						top->fname[i]);
				current_pathp = top->len;
				continue;
			}
			if (match("cg", 2)) {		/* cylinder group */
				if (type == NUMB)
					value = addr;
				if (value > fs->fs_ncg - 1) {
					printf("maximum cylinder group is ");
					print(fs->fs_ncg - 1, 8, -8, 0);
					printf("\n");
					error++;
					continue;
				}
				type = objsz = CGRP;
				cur_cgrp = value;
				addr = cgtod(fs, cur_cgrp) << FRGSHIFT;
				continue;
			}
			if (match("ct", 2)) {		/* creation time */
				acting_on_inode = 2;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_ctime;
				value = get(LONG);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'd':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("directory", 2)) { 	/* directory offsets */
				if (type == NUMB)
					value = addr;
				objsz = DIRECTORY;
				type = DIRECTORY;
				addr = getdirslot(value);
				continue;
			}
			if (match("db", 2)) {		/* direct block */
				acting_on_inode = 1;
				should_print = 1;
				if (type == NUMB)
					value = addr;
				if (value >= NDADDR) {
					printf("direct blocks are 0 to ");
					print(NDADDR - 1, 0, 0, 0);
					printf("\n");
					error++;
					continue;
				}
				addr = cur_ino;
				if (!icheck(addr))
					continue;
				addr = (long)
				      &((struct dinode *)cur_ino)->di_db[value];
				bod_addr = addr;
				cur_bytes = (value) * BLKSIZE;
				cur_block = value;
				type = BLOCK;
				dirslot = 0;
				value = get(LONG);
				if (!value && !override) {
					printf("non existent block\n");
					error++;
				}
				continue;
			}
			goto bad_syntax;

		case 'f':
			if (colon) 
				colon = 0;
			else
				goto no_colon;
			if (match("find", 3)) {		/* find command */
				find();
				continue;
			}
			if (match("fragment", 2)) {	/* fragment conv. */
				if (type == NUMB) {
					value = addr;
					cur_bytes = 0;
					blocksize = FRGSIZE;
					filesize = FRGSIZE * 2;
				}
				if (min(blocksize, filesize) - cur_bytes >
							FRGSIZE) {
					blocksize = cur_bytes + FRGSIZE;
					filesize = blocksize * 2;
				}
				addr = value << FRGSHIFT;
				bod_addr = addr;
				value = get(LONG);
				type = FRAGMENT;
				dirslot = 0;
				trapped++;
				continue;
			}
			if (match("file", 4)) {		/* access as file */
				acting_on_inode = 1;
				should_print = 1;
				if (type == NUMB)
					value = addr;
				addr = cur_ino;
				if ((mode = icheck(addr)) == 0)
					continue;
				if ((mode & IFCHR) && !override) {
					printf("special device\n");
					error++;
					continue;
				}
				if ((addr = (bmap(value) << FRGSHIFT)) == 0)
					continue;
				cur_block = value;
				bod_addr = addr;
				type = BLOCK;
				dirslot = 0;
				continue;
			}
			if (match("fill", 4)) {		/* fill */
				if (getachar() != '=') {
					printf("missing '='\n");
					error++;
					continue;
				}
				if (objsz == INODE || objsz == DIRECTORY) {
				      printf("can't fill inode or directory\n");
				      error++;
				      continue;
				}
				fill();
				continue;
			}
			goto bad_syntax;

		case 'g':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("gid", 1)) {		/* group id */
				acting_on_inode = 1;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_gid;
				value = get(SHORT);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'i':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("inode", 2)) { /* i# to inode conversion */
				if (c_count == 2) {
					addr = cur_ino;
					value = get(INODE);
					type = NULL;
					laststyle = '=';
					lastpo = 'i';
					should_print = 1;
					continue;
				}
				if (type == NUMB)
					value = addr;
				addr = itob(value);
				if (!icheck(addr))
					continue;
				cur_ino = addr;
				cur_inum = value;
				value = get(INODE);
				type = NULL;
				continue;
			}
			if (match("ib", 2)) {	/* indirect block */
				acting_on_inode = 1;
				should_print = 1;
				if (type == NUMB)
					value = addr;
				if (value >= NIADDR) {
					printf("indirect blocks are 0 to ");
					print(NIADDR - 1, 0, 0, 0);
					printf("\n");
					error++;
					continue;
				}
				addr = (long)
				      &((struct dinode *)cur_ino)->di_ib[value];
				cur_bytes = (NDADDR - 1) * BLKSIZE;
				temp = 1;
				for (i = 0; i < value; i++) {
					temp *= NINDIR(fs) * BLKSIZE;
					cur_bytes += temp;
				}
				type = BLOCK;
				dirslot = 0;
				value = get(LONG);
				if (!value && !override) {
					printf("non existent block\n");
					error++;
				}
				continue;
			}
			goto bad_syntax;

		case 'l':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("ls", 2)) {		/* ls command */
				temp = cur_inum;
				recursive = long_list = 0;
				top = filenames - 1;
				for (;;) {
					eat_spaces();
					if ((c = getachar()) == '-') {
						if ((c = getachar()) == 'R') {
						  recursive = 1;
						  continue;
						} else if (c == 'l') {
						  long_list = 1;
						} else {
						  printf("unknown option ");
						  printf("'%c'\n", c);
						  error++;
						  break;
						}
					} else
						ungetachar(c);
					if ((c = getachar()) == '\n') {
						if (c_count != 2) {
							ungetachar(c);
							break;
						}
					}
					c_count++;
					ungetachar(c);
					parse();
					restore_inode(temp);
					if (error)
						break;
				}
				recursive = 0;
				if (error || nfiles == 0) {
					if (!error) {
						print_path(input_path,
								input_pathp);
						printf(" not found\n");
					}
					continue;
				}
				if (nfiles) {
				    cmp_level = 0;
				    qsort((char *)filenames, nfiles,
					sizeof (struct filenames), ffcmp);
				    ls(filenames, filenames + (nfiles - 1), 0);
				} else {
				    printf("no match\n");
				    error++;
				}
				restore_inode(temp);
				continue;
			}
			if (match("ln", 2)) {		/* link count */
				acting_on_inode = 1;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_nlink;
				value = get(SHORT);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'm':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			addr = cur_ino;
			if ((mode = icheck(addr)) == 0)
				continue;
			if (match("mt", 2)) { 		/* modification time */
				acting_on_inode = 2;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_mtime;
				value = get(LONG);
				type = NULL;
				continue;
			}
			if (match("md", 2)) {		/* mode */
				acting_on_inode = 1;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_mode;
				value = get(SHORT);
				type = NULL;
				continue;
			}
			if (match("maj", 2)) {	/* major device number */
				acting_on_inode = 1;
				should_print = 1;
				if (devcheck(mode))
					continue;
				addr = (long)
					&((struct dinode *)cur_ino)->di_db[1];
				value = get(LONG);
				type = NULL;
				continue;
			}
			if (match("min", 2)) {	/* minor device number */
				acting_on_inode = 1;
				should_print = 1;
				if (devcheck(mode))
					continue;
				addr = (long)
					&((struct dinode *)cur_ino)->di_db[0];
				value = get(LONG);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'n':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("nm", 1)) {		/* directory name */
				objsz = DIRECTORY;
				acting_on_directory = 1;
				cur_dir = addr;
				if ((cptr = getblk(addr)) == 0)
					continue;
				dirp = (struct direct *)(cptr+blkoff(fs, addr));
				stringsize = (long)dirp->d_reclen -
				  ((long)&dirp->d_name[0] - (long)&dirp->d_ino);
				addr = (long)
					&((struct direct *)addr)->d_name[0];
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'o':
			if (colon) 
				colon = 0;
			else
				goto no_colon;
			if (match("override", 1)) {	/* override flip flop */
				if (override = !override)
					printf("error checking off\n");
				else
					printf("error checking on\n");
				continue;
			}
			goto bad_syntax;

		case 'p':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("pwd", 2)) {		/* print working dir */
				print_path(current_path, current_pathp);
				printf("\n");
				continue;
			}
			if (match("prompt", 2)) {	/* change prompt */
				if ((c = getachar()) != '=') {
					printf("missing '='\n");
					error++;
					continue;
				}
				if ((c = getachar()) != '"') {
					printf("missing '\"'\n");
					error++;
					continue;
				}
				i = 0;
				prompt = &prompt[0];
				while ((c = getachar()) != '"' &&
				       c != '\n') {
					prompt[i++] = c;
					if (i >= PROMPTSIZE) {
						printf("string too long\n");
						error++;
						break;
					}
				}
				prompt[i] = '\0';
				continue;
			}
			goto bad_syntax;
				
		case 'q':
			if (!colon)
				goto no_colon;
			if (match("quit", 1)) {		/* quit */
				if ((c = getachar()) != '\n') {
					error++;
					continue;
				}
				exit(0);
			}
			goto bad_syntax;

		case 's':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("sb", 2)) {		/* super block */
				if (c_count == 2) {
					cur_cgrp = -1;
					type = objsz = SB;
					laststyle = '=';
					lastpo = 's';
					should_print = 1;
					continue;
				}
				if (type == NUMB)
					value = addr;
				if (value > fs->fs_ncg - 1) {
					printf("maximum super block is ");
					print(fs->fs_ncg - 1, 8, -8, 0);
					printf("\n");
					error++;
					continue;
				}
				type = objsz = SB;
				cur_cgrp = value;
				addr = cgsblock(fs, cur_cgrp) << FRGSHIFT;
				continue;
			}
			if (match("sz", 2)) {		/* file size */
				acting_on_inode = 1;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_size;
				value = get(LONG);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'u':
			if (colon)
				colon = 0;
			else
				goto no_colon;
			if (match("uid", 1)) {		/* user id */
				acting_on_inode = 1;
				should_print = 1;
				addr = (long)
					&((struct dinode *)cur_ino)->di_uid;
				value = get(SHORT);
				type = NULL;
				continue;
			}
			goto bad_syntax;

		case 'F': /* buffer status (internal use only) */
			if (colon) 
				colon = 0;
			else
				goto no_colon;
			for (bp = bhdr.fwd; bp != &bhdr; bp = bp->fwd)
				printf("%8x %d\n",bp->blkno,bp->valid);
			printf("\n");
			printf("# commands\t\t%d\n", commands);
			printf("# read requests\t\t%d\n", read_requests);
			printf("# actual disk reads\t%d\n", actual_disk_reads);
			continue;
no_colon:
		printf("a colon should precede a command\n");
		error++;
		continue;
bad_syntax:
		printf("more letters needed to distinguish command\n");
		error++;
		continue;
		}
	}
}

/*
 * getachar - get next character from input buffer.
 */
char
getachar()
{
	return(input_buffer[input_pointer++]);
}

/*
 * ungetachar - return character to input buffer.
 */
ungetachar(c)
	register char	c;
{
	if (input_pointer == 0) {
		printf("internal problem maintaining input buffer\n");
		error++;
		return;
	}
	input_buffer[--input_pointer] = c;
}

/*
 * getnextinput - display the prompt and read an input line.
 *	An input line is up to 128 characters terminated by the newline
 *	character.  Handle overflow, shell escape, and eof.
 */
getnextinput()
{
	register int	i;
	register char	c;
	register short	pid, rpid;
	int		retcode;

newline:
	i = 0;
	printf("%s", prompt);
ignore_eol:
	while ((c = getc(stdin)) != '\n' && !(c == '!' && i == 0) &&
	       !feof(stdin) && i <= INPUTBUFFER - 2)
		input_buffer[i++] = c;
	if (input_buffer[i - 1] == '\\') {
		input_buffer[i++] = c;
		goto ignore_eol;
	}
	if (feof(stdin)) {
		printf("\n");
		exit(0);
	}
	if (c == '!') {
		if ((pid = fork()) == 0) {
			execl(_PATH_BSHELL, "sh", "-t", 0);
			error++;
			return;
		}
		while ((rpid = wait(&retcode)) != pid && rpid != -1)
			;
		printf("!\n");
		goto newline;
	}
	if (c != '\n')
		printf("input truncated to 128 characters\n");
	input_buffer[i] = '\n';
	input_pointer = 0;
}

/*
 * eat_spaces - read extraneous spaces.
 */
eat_spaces()
{
	register char	c;

	while ((c = getachar()) == ' ')
		;
	ungetachar(c);
}

/*
 * restore_inode - set up all inode indicators so inum is now
 *	the current inode.
 */
restore_inode(inum)
	long		inum;
{
	errinum = cur_inum = inum;
	addr = errino = cur_ino = itob(inum);
}

/*
 * match - return false if the input does not match string up to
 *	upto letters.   Then proceed to chew up extraneous letters.
 */
match(string, upto)
	register char	*string;
	register int	upto;
{
	register int	i, length = strlen(string) - 1;
	register char	c;
	int		save_upto = upto;

	while (--upto) {
		string++;
		if ((c = getachar()) != *string) {
			for (i = save_upto - upto; i; i--) {
				ungetachar(c);
				c = *--string;
			}
			return(0);
		}
		length--;
	}
	while (length--) {
		string++;
		if ((c = getachar()) != *string) {
			ungetachar(c);
			return(1);
		}
	}
	return(1);
}

/*
 * expr - expression evaluator.  Will evaluate expressions from
 *	left to right with no operator precedence.  Parentheses may
 *	be used.
 */
long
expr()
{
	register long	numb = 0, temp;
	register char	c;

	numb = term();
	for (;;) {
		if (error)
			return;
		c = getachar();
		switch (c) {

		case '+':
			numb += term();
			continue;

		case '-':
			numb -= term();
			continue;

		case '*':
			numb *= term();
			continue;

		case '%':
			temp = term();
			if (!temp) {
				printf("divide by zero\n");
				error++;
				return;
			}
			numb /= temp;
			continue;

		case ')':
			paren--;
			return(numb);

		default:
			ungetachar(c);
			if (paren && !error) {
				printf("missing ')'\n");
				error++;
			}
			return(numb);
		}
	}
}

/*
 * term - used by expression evaluator to get an operand.
 */
long
term()
{
	register char	c;

	switch (c = getachar()) {

	default:
		ungetachar(c);

	case '+':
		return(getnumb());

	case '-':
		return(-getnumb());

	case '(':
		paren++;
		return(expr());
	}
}

/*
 * getnumb - read a number from the input stream.  A leading
 *	zero signifies octal interpretation, a leading '0x'
 *	signifies hexadecimal, and a leading '0t' signifies
 *	decimal.  If the first character is a character,
 *	return an error.
 */
long
getnumb()
{

	register char	c, savec;
	long		number = 0, tbase, num;
	extern short	error;

	c = getachar();
	if (!digit(c)) {
		error++;
		ungetachar(c);
		return(-1);
	}
	if (c == '0') {
		tbase = OCTAL;
		if ((c = getachar()) == 'x')
			tbase = HEX;
		else if (c == 't')
			tbase = DECIMAL;
		else ungetachar(c);
	} else {
		tbase = base;
		ungetachar(c);
	}
	for (;;) {
		num = tbase;
		c = savec = getachar();
		if (HEXLETTER(c))
			c = uppertolower(c);
		switch (tbase) {
		case HEX:
			if (hexletter(c)) {
				num = hextodigit(c);
				break;
			}
		case DECIMAL:
			if (digit(c))
				num = numtodigit(c);
			break;
		case OCTAL:
			if (octaldigit(c))
				num = numtodigit(c);
		}
		if (num == tbase)
			break;
		number = number * tbase + num;
	}
	ungetachar(savec);
	return(number);
}

/*
 * find - the syntax is almost identical to the unix command.
 *		find dir [-name pattern] [-inum number]
 *	Note:  only one of -name or -inum may be used at a time.
 *	       Also, the -print is not needed (implied).
 */
find()
{
	register struct filenames	*fn;
	register char			c;
	long				temp;
	short				mode;

	eat_spaces();
	temp = cur_inum;
	top = filenames - 1;
	doing_cd = 1;
	parse();
	doing_cd = 0;
	if (nfiles != 1) {
		restore_inode(temp);
		if (!error) {
			print_path(input_path, input_pathp);
			if (nfiles == 0)
				printf(" not found\n");
			else
				printf(" ambiguous\n");
			error++;
			return;
		}
	}
	restore_inode(filenames->ino);
	freemem(filenames, nfiles);
	nfiles = 0;
	top = filenames - 1;
	if ((mode = icheck(addr)) == 0)
		return;
	if ((mode & IFMT) != IFDIR) {
		print_path(input_path, input_pathp);
		printf(" not a directory\n");
		error++;
		return;
	}
	eat_spaces();
	if ((c = getachar()) != '-') {
		printf("missing '-'\n");
		error++;
		return;
	}
	find_by_name = find_by_inode = 0;
	c = getachar();
	if (match("name", 4)) {
		eat_spaces();
		find_by_name = 1;
	} else if (match("inum", 4)) {
		eat_spaces();
		find_ino = expr();
		if (error)
			return;
		while ((c = getachar()) != '\n')
			;
		ungetachar(c);
		find_by_inode = 1;
	} else {
		printf("use -name or -inum with find\n");
		error++;
		return;
	}
	doing_find = 1;
	parse();
	doing_find = 0;
	if (error) {
		restore_inode(temp);
		return;
	}
	for (fn = filenames; fn <= top; fn++) {
		if (fn->find == 0)
			continue;
		printf("i#: ");
		print(fn->ino, 12, -8, 0);
		print_path(fn->fname, fn->len);
		printf("\n");
	}
	restore_inode(temp);
}

/*
 * ls - do an ls.  Should behave exactly as ls(1).
 *	Only -R and -l is supported and -l gives different results.
 */
ls(fn0, fnlast, level)
	struct filenames		*fn0, *fnlast;
	short				level;
{
	register struct filenames	*fn, *fnn;
	register int			i;
	int				fcmp();

	fn = fn0;
	for (;;) {
		fn0 = fn;
		if (fn0->len) {
			cmp_level = level;
			qsort((char *)fn0, fnlast - fn0 + 1,
				sizeof (struct filenames), fcmp);
		}
		for (fnn = fn, fn++; fn <= fnlast; fnn = fn, fn++) {
			if (fnn->len != fn->len && level == fnn->len - 1)
				break;
			if (fnn->len == 0)
				continue;
			if (strcmp(fn->fname[level], fnn->fname[level]))
				break;
		}
		if (fn0->len && level != fn0->len - 1)
			ls(fn0, fnn, level + 1);
		else {
			if (fn0 != filenames)
				printf("\n");
			print_path(fn0->fname, fn0->len - 1);
			printf(":\n");
			if (fn0->len == 0)
				cmp_level = level;
			else
				cmp_level = level + 1;
			qsort((char *)fn0, fnn - fn0 + 1,
				sizeof (struct filenames), fcmp);
			formatf(fn0, fnn);
			nfiles -= fnn - fn0 + 1;
		}
		if (fn > fnlast)
			return;
	}
}

/*
 * formatf - code lifted from ls.
 */
formatf(fn0, fnlast)
	register struct filenames	*fn0, *fnlast;
{
	register struct filenames	*fn;
	int				width = 0, w, nentry = fnlast - fn0 + 1;
	int				i, j, columns, lines;
	char				*cp;

	if (long_list) {
		columns = 1;
	} else {
		for (fn = fn0; fn <= fnlast; fn++) {
			int len = strlen(fn->fname[cmp_level]) + 2;

			if (len > width)
				width = len;
		}
		width = (width + 8) &~ 7;
		columns = 80 / width;
		if (columns == 0)
			columns = 1;
	}
	lines = (nentry + columns - 1) / columns;
	for (i = 0; i < lines; i++) {
		for (j = 0; j < columns; j++) {
			fn = fn0 + j * lines + i;
			if (long_list) {
				printf("i#: ");
				print(fn->ino, 12, -8, 0);
			}
			cp = fmtentry(fn);
			printf("%s", cp);
			if (fn + lines > fnlast) {
				printf("\n");
				break;
			}
			w = strlen(cp);
			while (w < width) {
				w = (w + 8) &~ 7;
				putchar('\t');
			}
		}
	}
}

/*
 * fmtentry - code lifted from ls.
 */
char *
fmtentry(fn)
	register struct filenames	*fn;
{
	static char			fmtres[BUFSIZ];
	register struct dinode		*ip;
	register char			*cptr, *cp, *dp;

	dp = &fmtres[0];
	for (cp = fn->fname[cmp_level]; *cp; cp++) {
		if (*cp < ' ' || *cp >= 0177)
			*dp++ = '?';
		else
			*dp++ = *cp;
	}
	addr = itob(fn->ino);
	if ((cptr = getblk(addr)) == 0)
		return(NULL);
	cptr += blkoff(fs, addr);
	ip = (struct dinode *)cptr;
	switch (ip->di_mode & IFMT) {
	case IFDIR:
		*dp++ = '/';
		break;
	case IFLNK:
		*dp++ = '@';
		break;
	case IFSOCK:
		*dp++ = '=';
		break;
#ifdef IFIFO
	case IFIFO:
		*dp++ = 'p';
		break;
#endif
	case IFCHR:
	case IFBLK:
	case IFREG:
		if (ip->di_mode & 0111)
			*dp++ = '*';
		else
			*dp++ = ' ';
		break;
	default:
		*dp++ = '?';

	}
	*dp++ = 0;
	return (fmtres);
}

/*
 * fcmp - routine used by qsort.  Will sort first by name, then
 *	then by pathname length if names are equal.  Uses global
 *	cmp_level to tell what component of the path name we are comparing.
 */
fcmp(f1, f2)
	register struct filenames	*f1, *f2;
{
	int 				value;

	if ((value = strcmp(f1->fname[cmp_level], f2->fname[cmp_level])))
		return(value);
	return (f1->len - f2->len);
}

/*
 * ffcmp - routine used by qsort.  Sort only by pathname length.
 */
ffcmp(f1, f2)
	register struct filenames	*f1, *f2;
{
	return (f1->len - f2->len);
}

/*
 * parse - set up the call to follow_path.
 */
parse()
{
	register int	i, j;
	char		c;

	stack_pathp = input_pathp = -1;
	if ((c = getachar()) == '/') {
		while ((c = getachar()) == '/')
			;
		ungetachar(c);
		cur_inum = 2;
		if ((c = getachar()) == '\n') {
			ungetachar('\n');
			if (doing_cd) {
				top++;
				top->ino = 2;
				top->len = -1;
				nfiles = 1;
				return;
			}
		} else
			ungetachar(c);
	} else {
		ungetachar(c);
		stack_pathp = current_pathp;
		if (!doing_find)
			input_pathp = current_pathp;
		for (i = 0; i <= current_pathp; i++) {
			if (!doing_find)
				strcpy(input_path[i], current_path[i]);
			strcpy(stack_path[i], current_path[i]);
		}
	}
	getname();
	follow_path(stack_pathp + 1, cur_inum);
}

/*
 * follow_path - called by cd, find, and ls.
 *	input_path holds the name typed by the user.
 *	stack_path holds the name at the current depth.
 */
follow_path(level, inum)
	long			level, inum;
{
	register struct direct	*dirp;
	register char		**ccptr, *cptr, c;
	register int		i;
	struct filenames	*tos, *bos, *fn, *fnn, *fnnn;
	long			block;
	short			mode;

	tos = top + 1;
	restore_inode(inum);
	if ((mode = icheck(addr)) == 0)
		return;
	if ((mode & IFMT) != IFDIR)
	    return;
	block = cur_bytes = 0;
	while (cur_bytes < filesize) {
	    if (block == 0 || bcomp(addr)) {
		error = 0;
		if ((addr = (bmap(block++) << FRGSHIFT)) == 0)
		    break;
		if ((cptr = getblk(addr)) == 0)
		    break;
		cptr += blkoff(fs, addr);
	    }
	    dirp = (struct direct *)cptr;
	    if (dirp->d_ino) {
		if (level > input_pathp || doing_find ||
			compare(input_path[level], &dirp->d_name[0], 1)) {
		    if (++top - filenames >= MAXFILES) {
			printf("too many files\n");
			error++;
			return;
		    }
		    top->fname = (char **)calloc(FIRST_DEPTH, sizeof (char **));
		    top->flag = 0;
		    if (top->fname == 0) {
			printf("out of memory\n");
			error++;
			return;
		    }
		    nfiles++;
		    top->ino = dirp->d_ino;
		    top->len = stack_pathp;
		    top->find = 0;
		    if (doing_find) {
			if (find_by_name) {
			    if (compare(input_path[0], &dirp->d_name[0], 1))
				top->find = 1;
			} else if (find_by_inode)
			    if (find_ino == dirp->d_ino)
				top->find = 1;
		    }
		    if (top->len + 1 >= FIRST_DEPTH && top->flag == 0) {
			ccptr = (char **)calloc(SECOND_DEPTH, sizeof (char **));
			if (ccptr == 0) {
			    printf("out of memory\n");
			    error++;
			    return;
			}
			for (i = 0; i < FIRST_DEPTH; i++)
				ccptr[i] = top->fname[i];
			free((char *)top->fname);
			top->fname = ccptr;
			top->flag = 1;
		    }
		    if (top->len >= SECOND_DEPTH) {
			printf("maximum depth exceeded, try to cd lower\n");
			error++;
			return;
		    }
		    /*
		     * Copy current depth.
		     */
		    for (i = 0; i <= stack_pathp; i++) {
			top->fname[i]=calloc(1, strlen(stack_path[i])+1);
			if (top->fname[i] == 0) {
			    printf("out of memory\n");
			    error++;
			    return;
			}
			strcpy(top->fname[i], stack_path[i]);
		    }
		    /*
		     * Check for '.' or '..' typed.
		     */
		    if ((level <= input_pathp) &&
				       (strcmp(input_path[level], ".") == 0 ||
					strcmp(input_path[level], "..") == 0)) {
			if (strcmp(input_path[level],"..") == 0 &&
							 top->len >= 0) {
			    free(top->fname[top->len]);
			    top->len -= 1;
			}
		    } else {
			/*
			 * Check for duplicates.
			 */
			if (!doing_cd && !doing_find) {
			    for (fn = filenames; fn < top; fn++) {
				if (fn->ino == dirp->d_ino &&
					    fn->len == stack_pathp + 1) {
				    for (i = 0; i < fn->len; i++)
					if (strcmp(fn->fname[i], stack_path[i]))
					    break;
				    if (i != fn->len ||
					    strcmp(fn->fname[i], dirp->d_name))
					continue;
				    freemem(top, 1);
				    if (top == filenames)
					top = NULL;
				    else
					top--;
					nfiles--;
					goto duplicate;
				}
			    }
			}
			top->len += 1;
			top->fname[top->len] = calloc(1,
						strlen(&dirp->d_name[0])+1);
			if (top->fname[top->len] == 0) {
			    printf("out of memory\n");
			    error++;
			    return;
			}
			strcpy(top->fname[top->len], &dirp->d_name[0]);
		    }
		}
	    }
duplicate:
	    addr += dirp->d_reclen;
	    cptr += dirp->d_reclen;
	    cur_bytes += dirp->d_reclen;
	}
	if (top < filenames)
	    return;
	if ((doing_cd && level == input_pathp) ||
		(!recursive && !doing_find && level > input_pathp))
	    return;
	bos = top;
	/*
	 * Check newly added entries to determine if further expansion
	 * is required.
	 */
	for (fn = tos; fn <= bos; fn++) {
	    /*
	     * Avoid '.' and '..' if beyond input.
	     */
	    if ((recursive || doing_find) && (level > input_pathp) &&
		(strcmp(fn->fname[fn->len], ".") == 0 ||
		 strcmp(fn->fname[fn->len], "..") == 0))
		 continue;
	    restore_inode(fn->ino);
	    if ((mode = icheck(cur_ino)) == 0)
		return;
	    if ((mode & IFMT) == IFDIR || level < input_pathp) {
		/*
		 * Set up current depth, remove current entry and
		 * continue recursion.
		 */
		for (i = 0; i <= fn->len; i++)
		    strcpy(stack_path[i], fn->fname[i]);
		stack_pathp = fn->len;
		if (!doing_find &&
			(!recursive || (recursive && level <= input_pathp))) {
		    /*
		     * Remove current entry by moving others up.
		     */
		    freemem(fn, 1);
		    fnn = fn;
		    for (fnnn = fnn, fnn++; fnn <= top; fnnn = fnn, fnn++) {
			fnnn->ino = fnn->ino;
			fnnn->len = fnn->len;
			if (fnnn->len + 1 < FIRST_DEPTH) {
			    fnnn->fname = (char **)calloc(FIRST_DEPTH,
							sizeof (char **));
			    fnnn->flag = 0;
			} else if (fnnn->len < SECOND_DEPTH) {
			    fnnn->fname = (char **)calloc(SECOND_DEPTH,
							sizeof (char **));
			    fnnn->flag = 1;
			} else {
			    printf("maximum depth exceeded, ");
			    printf("try to cd lower\n");
			    error++;
			    return;
			}
			for (i = 0; i <= fnn->len; i++)
			    fnnn->fname[i] = fnn->fname[i];
		    }
		    if (fn == tos)
			fn--;
		    top--;
		    bos--;
		    nfiles--;
		}
		follow_path(level + 1, cur_inum);
		if (error)
			return;
	    }
	}
}

/*
 * getname - break up the pathname entered by the user into components.
 */
getname()
{
	register int	i;
	char		c;

	if ((c = getachar()) == '\n') {
	    ungetachar(c);
	    return;
	}
	ungetachar(c);
	input_pathp++;
clear:
	for (i = 0; i < MAXNAMLEN; i++)
	    input_path[input_pathp][i] = '\0';
	for (;;) {
	    c = getachar();
	    if (c == '\\') {
		if (strlen(input_path[input_pathp]) + 1 >= MAXNAMLEN) {
		    printf("maximum name length exceeded, ");
		    printf("truncating\n");
		    return;
		}
		input_path[input_pathp][strlen(input_path[input_pathp])] = c;
		input_path[input_pathp][strlen(input_path[input_pathp])] =
						getachar();
		continue;
	    }
	    if (c == ' ' || c == '\n') {
		ungetachar(c);
		return;
	    }
	    if (!doing_find && c == '/') {
		if (++input_pathp >= MAXPATHLEN) {
		    printf("maximum path length exceeded, ");
		    printf("truncating\n");
		    input_pathp--;
		    return;
		}
		goto clear;
	    }
	    if (strlen(input_path[input_pathp]) >= MAXNAMLEN) {
		printf("maximum name length exceeded, truncating\n");
		return;
	    }
	    input_path[input_pathp][strlen(input_path[input_pathp])] = c;
	}
}

/*
 * compare - check if a filename matches the pattern entered by the user.
 *	Handles '*', '?', and '[]'.
 */
compare(s1, s2, at_start)
	char		*s1, *s2;
	short		at_start;
{
	register char	c, *s;

	s = s2;
	while (c = *s1) {
		if (c == '*') {
			if (at_start && s == s2 && !letter(*s2) && !digit(*s2))
				return(0);
			if (*++s1 == 0)
				return(1);
			while (*s2) {
				if (compare(s1, s2, 0))
					return(1);
				if (error)
					return(0);
				s2++;
			}
		}
		if (*s2 == 0)
			return(0);
		if (c == '\\') {
			s1++;
			goto compare_chars;
		}
		if (c == '?') {
			if (at_start && s == s2 && !letter(*s2) && !digit(*s2))
				return(0);
			s1++;
			s2++;
			continue;
		}
		if (c == '[') {
			s1++;
			if (*s2 >= *s1++) {
				if (*s1++ != '-') {
					printf("missing '-'\n");
					error++;
					return(0);
				}
				if (*s2 <= *s1++) {
					if (*s1++ != ']') {
						printf("missing ']'");
						error++;
						return(0);
					}
					s2++;
					continue;
				}
			}
		}
compare_chars:
		if (*s1++ == *s2++)
			continue;
		else
			return(0);
	}
	if (*s1 == *s2)
		return(1);
	return(0);
}

/*
 * freemem - free the memory allocated to the filenames structure.
 */
freemem(p, numb)
	struct filenames	*p;
	int			numb;
{
	register int		i, j;

	if (numb == 0)
		return;
	for (i = 0; i < numb; i++, p++) {
		for (j = 0; j <= p->len; j++)
			free(p->fname[j]);
		free((char *)p->fname);
	}
}

/*
 * print_path - print the pathname held in p.
 */
print_path(p, pntr)
	char		*p[];
	short		pntr;
{
	register int	i;

	printf("/");
	if (pntr >= 0) {
		for (i = 0; i < pntr; i++)
			printf("%s/", p[i]);
		printf("%s", p[pntr]);
	}
}

/*
 * fill - fill a section with a value or string.
 *	addr,count:fill=[value, "string"].
 */
fill()
{
	register char	*cptr;
	register int	i;
	short		eof_flag, end = 0, eof = 0;
	long		temp, tcount, taddr;

	if (!wrtflag) {
		printf("not opened for write '-w'\n");
		error++;
		return;
	}
	temp = expr();
	if (error)
		return;
	if ((cptr = getblk(addr)) == 0)
		return;
	if (type == NUMB)
		eof_flag = 0;
	else
		eof_flag = 1;
	taddr = addr;
	switch (objsz) {
	case LONG:
		addr &= ~(LONG - 1);
		break;
	case SHORT:
		addr &= ~(SHORT - 1);
		temp &= 0177777L;
		break;
	case CHAR:
		temp &= 0377;
	}
	cur_bytes -= taddr - addr;
	cptr += blkoff(fs, addr);
	tcount = check_addr(eof_flag, &end, &eof, 0);
	for (i = 0; i < tcount; i++) {
		switch (objsz) {
		case LONG:
			*(long *)cptr = temp;
			break;
		case SHORT:
			*(short *)cptr = temp;
			break;
		case CHAR:
			*cptr = temp;
		}
		cptr += objsz;
	}
	addr += (tcount - 1) * objsz;
	cur_bytes += (tcount - 1) * objsz;
	put(temp, objsz);
	if (eof) {
		printf("end of file\n");
		error++;
	} else if (end) {
		printf("end of block\n");
		error++;
	}
}
	
/*
 * get - read a byte, short or long from the file system.
 *	The entire block containing the desired item is read
 *	and the appropriate data is extracted and returned. 
 */
long
get(lngth)
	short		lngth;
{

	register char	*bptr;
	long		temp = addr;

	objsz = lngth;
	if (objsz == INODE || objsz == SHORT)
		temp &= ~(SHORT - 1);
	else if (objsz == DIRECTORY || objsz == LONG)
		temp &= ~(LONG - 1);
	if ((bptr = getblk(temp)) == 0)
		return(-1);
	bptr += blkoff(fs, temp);
	switch (objsz) {
	case CHAR:
		return((long)*bptr);
	case SHORT:
	case INODE:
		return((long)(*(short *)bptr));
	case LONG:
	case DIRECTORY:
		return(*(long *)bptr);
	}
	return(0);
}

/*
 * cgrp_check - make sure that we don't bump the cylinder group
 *	beyond the total number of cylinder groups or before the start.
 */
cgrp_check(cgrp)
	long		cgrp;
{
	if (cgrp < 0) {
		if (objsz == CGRP)
			printf("beginning of cylinder groups\n");
		else
			printf("beginning of super blocks\n");
		error++;
		return(0);
	}
	if (cgrp >= fs->fs_ncg) {
		if (objsz == CGRP)
			printf("end of cylinder groups\n");
		else
			printf("end of super blocks\n");
		error++;
		return(0);
	}
	if (objsz == CGRP)
		return(cgtod(fs, cgrp) << FRGSHIFT);
	else
		return(cgsblock(fs, cgrp) << FRGSHIFT);
}

/*
 * icheck -  make sure we can read the block containing the inode
 *	and determine the filesize (0 if inode not allocated).  Return
 *	0 if error otherwise return the mode.
 */
icheck(address)
	long			address;
{
	register char		*cptr;
	register struct dinode	*ip;

	if ((cptr = getblk(address)) == 0)
		return(0);
	cptr += blkoff(fs, address);
	ip = (struct dinode *)cptr;
	if ((ip->di_mode & IFMT) == 0) {
		if (!override) {
			printf("inode not allocated\n");
			error++;
			return(0);
		}
		blocksize = filesize = 0;
	} else {
		trapped++;
		filesize = ip->di_size;
		blocksize = filesize * 2;
	}
	return(ip->di_mode);
}

/*
 * getdirslot - get the address of the directory slot desired.
 */
getdirslot(slot)
	short			slot;
{
	register char		*cptr;
	register struct direct	*dirp;
	register short		i;
	char			*string = &scratch[0];
	short			bod = 0, mode, temp;

	if (slot < 0) {
		slot = 0;
		bod++;
	}
	if (type != DIRECTORY) {
		if (type == BLOCK)
			string = "block";
		else
			string = "fragment";
		addr = bod_addr;
		if ((cptr = getblk(addr)) == 0)
			return(0);
		cptr += blkoff(fs, addr);
		cur_bytes = 0;
		dirp = (struct direct *)cptr;
		for (dirslot = 0; dirslot < slot; dirslot++) {
			dirp = (struct direct *)cptr;
			if (blocksize > filesize) {
				if (cur_bytes + dirp->d_reclen >= filesize) {
					printf("end of file\n");
					erraddr = addr;
					errcur_bytes = cur_bytes;
					stringsize = STRINGSIZE(dirp);
					error++;
					return(addr);
				}
			} else {
				if (cur_bytes + dirp->d_reclen >= blocksize) {
					printf("end of %s\n", string);
					erraddr = addr;
					errcur_bytes = cur_bytes;
					stringsize = STRINGSIZE(dirp);
					error++;
					return(addr);
				}
			}
			cptr += dirp->d_reclen;
			addr += dirp->d_reclen;
			cur_bytes += dirp->d_reclen;
		}
		if (bod) {
			if (blocksize > filesize)
				printf("beginning of file\n");
			else
				printf("beginning of %s\n", string);
			erraddr = addr;
			errcur_bytes = cur_bytes;
			error++;
		}
		stringsize = STRINGSIZE(dirp);
		return(addr);
	} else {
		addr = cur_ino;
		if ((mode = icheck(addr)) == 0)
			return(0);
		if (!override && (mode & IFDIR) == 0) {
			printf("inode is not a directory\n");
			error++;
			return(0);
		}
		temp = slot;
		i = cur_bytes = 0;
		for (;;) {
			if (i == 0 || bcomp(addr)) {
				error = 0;
				if ((addr=(bmap(i++) << FRGSHIFT)) == 0)
					break;
				if ((cptr = getblk(addr)) == 0)
					break;
				cptr += blkoff(fs, addr);
			}
			dirp = (struct direct *)cptr;
			value = dirp->d_ino;
			if (!temp--)
				break;
			if (cur_bytes + dirp->d_reclen >= filesize) {
				printf("end of file\n");
				dirslot = slot - temp - 1;
				objsz = DIRECTORY;
				erraddr = addr;
				errcur_bytes = cur_bytes;
				stringsize = STRINGSIZE(dirp);
				error++;
				return(addr);
			}
			addr += dirp->d_reclen;
			cptr += dirp->d_reclen;
			cur_bytes += dirp->d_reclen;
		}
		dirslot = slot;
		objsz = DIRECTORY;
		if (bod) {
			printf("beginning of file\n");
			erraddr = addr;
			errcur_bytes = cur_bytes;
			error++;
		}
		stringsize = STRINGSIZE(dirp);
		return(addr);
	}
}

/*
 * putf - print a byte as an ascii character if possible.
 *	The exceptions are tabs, newlines, backslashes
 *	and nulls which are printed as the standard C
 *	language escapes. Characters which are not
 *	recognized are printed as \?.
 */
putf(c)
	register	char  c;
{

	if (c<=037 || c>=0177 || c=='\\') {
		printf("\\");
		switch (c) {
		case '\\':
			printf("\\");
			break;
		case '\t':
			printf("t");
			break;
		case '\n':
			printf("n");
			break;
		case '\0':
			printf("0");
			break;
		default:
			printf("?");
		}
	}
	else {
		printf("%c", c);
		printf(" ");
	}
}

/*
 * put - write an item into the buffer for the current address
 *	block.  The value is checked to make sure that it will
 *	fit in the size given without truncation.  If successful,
 *	the entire block is written back to the file system.
 */
put(item,lngth)
	long		item;
	short		lngth;
{

	register char	*bptr, *sbptr;
	register long	*vptr;
	long		s_err,nbytes;
	long		olditem;

	if (!wrtflag) {
		printf("not opened for write '-w'\n");
		error++;
		return;
	}
	objsz = lngth;
	if ((sbptr = getblk(addr)) == 0)
		return;
	bptr = sbptr + blkoff(fs, addr);
	switch (objsz) {
	case LONG:
	case DIRECTORY:
		olditem = *(long *)bptr;
		*(long *)bptr = item;
		break;
	case SHORT:
	case INODE:
		olditem = (long)*(short *)bptr;
		item &= 0177777L;
		*(short *)bptr = item;
		break;
	case CHAR:
		olditem = (long)*bptr;
		item &= 0377;
		*bptr = lobyte(loword(item));
		break;
	default:
		error++;
		return;
	}
	if ((s_err = lseek(fd, addr & fs->fs_bmask, 0)) == -1) {
		error++;
		printf("seek error : %x\n",addr);
		return(0);
	}
	if ((nbytes = write(fd, sbptr, BLKSIZE)) != BLKSIZE) {
		error++;
		printf("write error : addr   = %x\n",addr);
		printf("            : s_err  = %x\n",s_err);
		printf("            : nbytes = %x\n",nbytes);
		return(0);
	}
	if (!acting_on_inode && objsz != INODE && objsz != DIRECTORY) {
		index(base);
		print(olditem, 8, -8, 0);
		printf("\t=\t");
		print(item, 8, -8, 0);
		printf("\n");
	} else {
		if (objsz == DIRECTORY) {
			addr = cur_dir;
			fprnt('?', 'd');
		} else {
			addr = cur_ino;
			objsz = INODE;
			fprnt('?', 'i');
		}
	}
	return;
}

/*
 * getblk - check if the desired block is in the file system.
 *	Search the incore buffers to see if the block is already
 *	available. If successful, unlink the buffer control block
 *	from its position in the buffer list and re-insert it at
 *	the head of the list.  If failure, use the last buffer
 *	in the list for the desired block. Again, this control
 *	block is placed at the head of the list. This process
 *	will leave commonly requested blocks in the in-core buffers.
 *	Finally, a pointer to the buffer is returned.
 */
char *
getblk(address)
	long			address;
{

	register struct buf	*bp;
	long			s_err, nbytes;
	unsigned long		block;

	read_requests++;
	block = lblkno(fs, address);
	if (block >= fragstoblks(fs, fs->fs_size)) {
		printf("block exceeds maximum block in file system\n");
		error++;
		return(0);
	}
	for (bp=bhdr.fwd; bp!= &bhdr; bp=bp->fwd)
		if (bp->valid && bp->blkno==block)
			goto xit;
	actual_disk_reads++;
	bp = bhdr.back;
	bp->blkno = block;
	bp->valid = 0;
	if ((s_err = lseek(fd, address & fs->fs_bmask, 0)) == -1) {
		error++;
		printf("seek error : %x\n",address);
		return(0);
	}
	if ((nbytes = read(fd, bp->blkaddr, BLKSIZE)) != BLKSIZE) {
		error++;
		printf("read error : addr   = %x\n",address);
		printf("           : s_err  = %x\n",s_err);
		printf("           : nbytes = %x\n",nbytes);
		return(0);
	}
	bp->valid++;
xit:	bp->back->fwd = bp->fwd;
	bp->fwd->back = bp->back;
	insert(bp);
	return(bp->blkaddr);
}

/*
 * insert - place the designated buffer control block
 *	at the head of the linked list of buffers.
 */
insert(bp)
	register struct buf	*bp;
{

	bp->back = &bhdr;
	bp->fwd = bhdr.fwd;
	bhdr.fwd->back = bp;
	bhdr.fwd = bp;
}

/*
 * err - called on interrupts.  Set the current address
 *	back to the last address stored in erraddr. Reset all
 *	appropriate flags.  A reset call is made to return
 *	to the main loop;
 */
err()
{
	freemem(filenames, nfiles);
	nfiles = 0;
	signal(2,err);
	addr = erraddr;
	cur_ino = errino;
	cur_inum = errinum;
	cur_bytes = errcur_bytes;
	error = 0;
	c_count = 0;
	printf("\n?\n");
	fseek(stdin, 0L, 2);
	longjmp(env,0);
}

/*
 * devcheck - check that the given mode represents a 
 *	special device. The IFCHR bit is on for both
 *	character and block devices.
 */
devcheck(md)
	register	short md;
{
	if (override)
		return(0);
	if (md & IFCHR)
		return(0);
	printf("not character or block device\n");
	error++;
	return(1);
}

/*
 * nullblk - return error if address is zero.  This is done
 *	to prevent block 0 from being used as an indirect block
 *	for a large file or as a data block for a small file.
 */
nullblk(bn)
	long		bn;
{
	if (bn != 0)
		return(0);
	printf("non existent block\n");
	error++;
	return(1);
}

/*
 * puta - put ascii characters into a buffer.  The string
 *	terminates with a quote or newline.  The leading quote,
 *	which is optional for directory names, was stripped off
 *	by the assignment case in the main loop.
 */
puta()
{
	register char		*cptr, c;
	register int		i;
	char			*sbptr;
	short			terror = 0;
	long			maxchars, s_err, nbytes, temp;
	long			taddr = addr, tcount = 0, item, olditem = 0;

	if (!wrtflag) {
		printf("not opened for write '-w'\n");
		error++;
		return;
	}
	if ((sbptr = getblk(addr)) == 0)
		return;
	cptr = sbptr + blkoff(fs, addr);
	if (objsz == DIRECTORY) {
		if (acting_on_directory)
			maxchars = stringsize - 1;
		else
			maxchars = LONG;
	} else if (objsz == INODE)
		maxchars = objsz - (addr - cur_ino);
	else
		maxchars = min(blocksize - cur_bytes, filesize - cur_bytes);
	while ((c = getachar()) != '"') {
		if (tcount >= maxchars) {
			printf("string too long\n");
			if (objsz == DIRECTORY)
				addr = cur_dir;
			else if (acting_on_inode || objsz == INODE)
				addr = cur_ino;
			else
				addr = taddr;
			erraddr = addr;
			errcur_bytes = cur_bytes;
			terror++;
			break;
		}
		tcount++;
		if (c == '\n') {
			ungetachar(c);
			break;
		}
		temp = (long)*cptr;
		olditem <<= BITSPERCHAR;
		olditem += temp & 0xff;
		if (c == '\\') {
			switch (c = getachar()) {
			case 't':
				*cptr++ = '\t';
				break;
			case 'n':
				*cptr++ = '\n';
				break;
			case '0':
				*cptr++ = '\0';
				break;
			default:
				*cptr++ = c;
				break;
			}
		}
		else
			*cptr++ = c;
	}
	if (objsz == DIRECTORY && acting_on_directory)
		for (i = tcount; i <= maxchars; i++)
			*cptr++ = '\0';
	if ((s_err = lseek(fd, addr & fs->fs_bmask, 0)) == -1) {
		error++;
		printf("seek error : %x\n",addr);
		return(0);
	}
	if ((nbytes = write(fd, sbptr, BLKSIZE)) != BLKSIZE) {
		error++;
		printf("write error : addr   = %x\n",addr);
		printf("            : s_err  = %x\n",s_err);
		printf("            : nbytes = %x\n",nbytes);
		return(0);
	}
	if (!acting_on_inode && objsz != INODE && objsz != DIRECTORY) {
		addr += tcount;
		cur_bytes += tcount;
		taddr = addr;
		if (objsz != CHAR) {
			addr &= ~(objsz - 1);
			cur_bytes -= taddr - addr;
		}
		if (addr == taddr) {
			addr -= objsz;
			taddr = addr;
		}
		tcount = LONG - (taddr - addr);
		index(base);
		if ((cptr = getblk(addr)) == 0)
			return;
		cptr += blkoff(fs, addr);
		switch (objsz) {
		case LONG:
			item = *(long *)cptr;
			if (tcount < LONG) {
				olditem <<= tcount * BITSPERCHAR;
				temp = 1;
				for (i = 0; i < (tcount*BITSPERCHAR); i++)
					temp <<= 1;
				olditem += item & (temp - 1);
			}
			break;
		case SHORT:
			item = (long)*(short *)cptr;
			if (tcount < SHORT) {
				olditem <<= tcount * BITSPERCHAR;
				temp = 1;
				for (i = 0; i < (tcount * BITSPERCHAR); i++)
					temp <<= 1;
				olditem += item & (temp - 1);
			}
			olditem &= 0177777L;
			break;
		case CHAR:
			item = (long)*cptr;
			olditem &= 0377;
		}
		print(olditem, 8, -8, 0);
		printf("\t=\t");
		print(item, 8, -8, 0);
		printf("\n");
	} else {
		if (objsz == DIRECTORY) {
			addr = cur_dir;
			fprnt('?', 'd');
		} else {
			addr = cur_ino;
			objsz = INODE;
			fprnt('?', 'i');
		}
	}
	if (terror)
		error++;
}

/*
 * fprnt - print data.  'count' elements are printed where '*' will
 *	print an entire blocks worth or up to the eof, whichever
 *	occurs first.  An error will occur if crossing a block boundary
 *	is attempted since consecutive blocks don't usually have
 *	meaning.  Current print types:
 *		/		b   - print as bytes (base sensitive)
 *				c   - print as characters
 *				o O - print as octal shorts (longs)
 *				d D - print as decimal shorts (longs)
 *				x X - print as hexadecimal shorts (longs)
 *		?		c   - print as cylinder groups
 *				d   - print as directories
 *				i   - print as inodes
 *				s   - print as super blocks
 */
fprnt(style, po)
	register char		style, po;
{
	register int		i;
	register struct fs	*sb;
	register struct cg	*cg;
	register struct direct	*dirp;
	register struct dinode	*ip;
	int			tbase;
	char			c, *cptr, *p;
	long			tinode, tcount, temp, taddr;
	short			offset, mode, end = 0, eof = 0, eof_flag;
	unsigned short		*sptr;
	unsigned long		*lptr;

	laststyle = style;
	lastpo = po;
	should_print = 0;
	if (count != 1) {
		if (clear) {
			count = 1;
			star = 0;
			clear = 0;
		} else
			clear = 1;
	}
	tcount = count;
	offset = blkoff(fs, addr);

	if (style == '/') {
		if (type == NUMB)
			eof_flag = 0;
		else
			eof_flag = 1;
		switch (po) {

		case 'c': /* print as characters */
		case 'b': /* or bytes */
			if ((cptr = getblk(addr)) == 0)
				return;
			cptr += offset;
			objsz = CHAR;
			tcount = check_addr(eof_flag, &end, &eof, 0);
			if (tcount) {
				for (i=0; tcount--; i++) {
					if (i % 16 == 0) {
						if (i)
							printf("\n");
						index(base);
					}
					if (po == 'c') {
						putf(*cptr++);
						if ((i + 1) % 16)
							printf("  ");
					} else {
						if ((i + 1) % 16 == 0)
							print(*cptr++ & 0377,
								2,-2,0);
						else
							print(*cptr++ & 0377,
								4,-2,0);
					}
					addr += CHAR;
					cur_bytes += CHAR;
				}
				printf("\n");
			}
			addr -= CHAR;
			erraddr = addr;
			cur_bytes -= CHAR;
			errcur_bytes = cur_bytes;
			if (eof) {
				printf("end of file\n");
				error++;
			} else if (end) {
				if (type == BLOCK)
					printf("end of block\n");
				else
					printf("end of fragment\n");
				error++;
			}
			return;

		case 'o': /* print as octal shorts */
			tbase = OCTAL;
			goto otx;
		case 'd': /* print as decimal shorts */
			tbase = DECIMAL;
			goto otx;
		case 'x': /* print as hex shorts */
			tbase = HEX;
otx:
			if ((cptr = getblk(addr)) == 0)
				return;
			taddr = addr;
			addr &= ~(SHORT - 1);
			cur_bytes -= taddr - addr;
			cptr += blkoff(fs, addr);
			sptr = (unsigned short *)cptr;
			objsz = SHORT;
			tcount = check_addr(eof_flag, &end, &eof, 0);
			if (tcount) {
				for (i=0; tcount--; i++) {
					sptr = (unsigned short *)
					   print_check(sptr, &tcount, tbase, i);
					switch (po) {
					case 'o':
						printf("%06o ",*sptr++);
						break;
					case 'd':
						printf("%05d  ",*sptr++);
						break;
					case 'x':
						printf("%04x   ",*sptr++);
					}
					addr += SHORT;
					cur_bytes += SHORT;
				}
				printf("\n");
			}
			addr -= SHORT;
			erraddr = addr;
			cur_bytes -= SHORT;
			errcur_bytes = cur_bytes;
			if (eof) {
				printf("end of file\n");
				error++;
			} else if (end) {
				if (type == BLOCK)
					printf("end of block\n");
				else
					printf("end of fragment\n");
				error++;
			}
			return;

		case 'O': /* print as octal longs */
			tbase = OCTAL;
			goto OTX;
		case 'D': /* print as decimal longs */
			tbase = DECIMAL;
			goto OTX;
		case 'X': /* print as hex longs */
			tbase = HEX;
OTX:
			if ((cptr = getblk(addr)) == 0)
				return;
			taddr = addr;
			addr &= ~(LONG - 1);
			cur_bytes -= taddr - addr;
			cptr += blkoff(fs, addr);
			lptr = (unsigned long *)cptr;
			objsz = LONG;
			tcount = check_addr(eof_flag, &end, &eof, 0);
			if (tcount) {
				for (i=0; tcount--; i++) {
					lptr =
					   print_check(lptr, &tcount, tbase, i);
					switch (po) {
					case 'O':
						printf("%011o    ",*lptr++);
						break;
					case 'D':
						printf("%010u     ",*lptr++);
						break;
					case 'X':
						printf("%08x       ",*lptr++);
					}
					addr += LONG;
					cur_bytes += LONG;
				}
				printf("\n");
			}
			addr -= LONG;
			erraddr = addr;
			cur_bytes -= LONG;
			errcur_bytes = cur_bytes;
			if (eof) {
				printf("end of file\n");
				error++;
			} else if (end) {
				if (type == BLOCK)
					printf("end of block\n");
				else
					printf("end of fragment\n");
				error++;
			}
			return;

		default:
			error++;
			printf("no such print option\n");
			return;
		}
	} else
		switch (po) {

		case 'c': /* print as cylinder group */
			if (type != NUMB)
				if (cur_cgrp + count > fs->fs_ncg) {
					tcount = fs->fs_ncg - cur_cgrp;
					if (!star)
						end++;
				}
			addr &= ~(LONG - 1);
			for (; tcount--;) {
				erraddr = addr;
				errcur_bytes = cur_bytes;
				if (type != NUMB) {
					addr = cgtod(fs, cur_cgrp)
						<< FRGSHIFT;
					cur_cgrp++;
				}
				if ((cptr = getblk(addr)) == 0) {
					if (cur_cgrp)
						cur_cgrp--;
					return;
				}
				cptr += blkoff(fs, addr);
				cg = (struct cg *)cptr;
				if (type == NUMB) {
					cur_cgrp = cg->cg_cgx + 1;
					type = objsz = CGRP;
					if (cur_cgrp + count - 1 > fs->fs_ncg) {
						tcount = fs->fs_ncg - cur_cgrp;
						if (!star)
							end++;
					}
				}
				if (!override && !cg_chkmagic(cg)) {
					printf("invalid cylinder group ");
					printf("magic word\n");
					if (cur_cgrp)
						cur_cgrp--;
					error++;
					return;
				}
				printcg(cg);
				if (tcount)
					printf("\n");
			}
			cur_cgrp--;
			if (end) {
				printf("end of cylinder groups\n");
				error++;
			}
			return;

		case 'd': /* print as directories */
			if ((cptr = getblk(addr)) == 0)
				return;
			if (type == NUMB) {
				if (fragoff(fs, addr)) {
					printf("address must be at the ");
					printf("beginning of a fragment\n");
					error++;
					return;
				}
				bod_addr = addr;
				type = FRAGMENT;
				dirslot = 0;
				cur_bytes = 0;
				blocksize = FRGSIZE;
				filesize = FRGSIZE * 2;
			}
			cptr += offset;
			objsz = DIRECTORY;
			while (tcount-- && cur_bytes < filesize &&
			       cur_bytes < blocksize && !bcomp(addr)) {
				dirp = (struct direct *)cptr;
				tinode = dirp->d_ino;
				printf("i#: ");
				if (tinode == 0)
					printf("free\t");
				else
					print(tinode, 12, -8, 0);
				printf("%s\n",&dirp->d_name[0]);
				erraddr = addr;
				errcur_bytes = cur_bytes;
				addr += dirp->d_reclen;
				cptr += dirp->d_reclen;
				cur_bytes += dirp->d_reclen;
				dirslot++;
			}
			addr = erraddr;
			cur_dir = addr;
			cur_bytes = errcur_bytes;
			stringsize = STRINGSIZE(dirp);
			dirslot--;
			if (tcount >= 0 && !star) {
				switch (type) {
				case FRAGMENT:
					printf("end of fragment\n");
					break;
				case BLOCK:
					printf("end of block\n");
					break;
				default:
					printf("end of directory\n");
				}
				error++;
			} else
				error = 0;
			return;

		case 'i': /* print as inodes */
			if ((ip = (struct dinode *)getblk(addr)) == 0)
				return;
			for (i=1; i < fs->fs_ncg; i++)
				if (addr < (cgimin(fs,i) << FRGSHIFT))
					break;
			i--;
			offset /= INODE;
			temp = (addr - (cgimin(fs,i) << FRGSHIFT)) >> FRGSHIFT;
			temp = (i * fs->fs_ipg) + fragstoblks(fs,temp) *
							INOPB(fs) + offset;
			if (count + offset > INOPB(fs)) {
				tcount = INOPB(fs) - offset;
				if (!star)
					end++;
			}
			objsz = INODE;
			ip += offset;
			for (i=0; tcount--; ip++, temp++) {
				if ((mode = icheck(addr)) == 0)
					if (!override)
						continue;
				p = " ugtrwxrwxrwx";

				switch (mode & IFMT) {
				case IFDIR:
					c = 'd';
					break;
				case IFCHR:
					c = 'c';
					break;
				case IFBLK:
					c = 'b';
					break;
				case IFREG:
					c = '-';
					break;
				case IFLNK:
					c = 'l';
					break;
				case IFSOCK:
					c = 's';
					break;
				default:
					c = '?';
					if (!override)
						goto empty;

				}
				printf("i#: ");
				print(temp,12,-8,0);
				printf("   md: ");
				printf("%c", c);
				for (mode = mode << 4; *++p; mode = mode << 1) {
					if (mode & IFREG)
						printf("%c", *p);
					else
						printf("-");
				} 
				printf("  uid: ");
				print(ip->di_uid,8,-4,0);
				printf("      gid: ");
				print(ip->di_gid,8,-4,0);
				printf("\n");
				printf("ln: ");
				print(ip->di_nlink,8,-4,0);
				printf("       bs: ");
				print(ip->di_blocks,12,-8,0);
				printf("   sz : ");
				print(ip->di_size,12,-8,0);
				printf("\n");
				if (ip->di_mode & IFCHR) {
					printf("maj: ");
					print(ip->di_db[1] & 0377,4,-2,0);
					printf("  min: ");
					print(ip->di_db[0] & 0377,4,-2,0);
					printf("\n");
				} else {
					for (i = 0; i < NDADDR; ) {
						if (ip->di_db[i] == 0)
							break;
						printf("db#%x: ",i);
						print(ip->di_db[i],11,-8,0);
						if (++i % 4 == 0)
							printf("\n");
						else
							printf("  ");
					}
					if (i % 4)
						printf("\n");
					for (i = 0; i < NIADDR; i++) {
						if (ip->di_ib[i] == 0)
							break;
						printf("ib#%x: ",i);
						print(ip->di_ib[i],11,-8,0);
						printf("  ");
					}
					if (i)
						printf("\n");
				}
				if (count == 1) {
					printf("\taccessed: %s",
						ctime(&ip->di_atime));
					printf("\tmodified: %s",
						ctime(&ip->di_mtime));
					printf("\tcreated : %s",
						ctime(&ip->di_ctime));
				}
				if (tcount)
					printf("\n");
empty:
				if (c == '?' && !override) {
					printf("i#: ");
					print(temp, 12, -8, 0);
					printf("  is unallocated\n");
					if (count != 1)
						printf("\n");
				}
				cur_ino = erraddr = addr;
				errcur_bytes = cur_bytes;
				cur_inum++;
				addr = addr + INODE;
			}
			addr = erraddr;
			cur_bytes = errcur_bytes;
			cur_inum--;
			if (end) {
				printf("end of block\n");
				error++;
			}
			return;

		case 's': /* print as super block */
			if (cur_cgrp == -1) {
				addr = SBLOCK * DEV_BSIZE;
				type = NUMB;
			}
			addr &= ~(LONG - 1);
			if (type != NUMB)
				if (cur_cgrp + count > fs->fs_ncg) {
					tcount = fs->fs_ncg - cur_cgrp;
					if (!star)
						end++;
				}
			for (; tcount--;) {
				erraddr = addr;
				cur_bytes = errcur_bytes;
				if (type != NUMB) {
					addr = cgsblock(fs, cur_cgrp)
							<< FRGSHIFT;
					cur_cgrp++;
				}
				if ((cptr = getblk(addr)) == 0) {
					if (cur_cgrp)
						cur_cgrp--;
					return;
				}
				cptr += blkoff(fs, addr);
				sb = (struct fs *)cptr;
				if (type == NUMB) {
					for (i = 0; i < fs->fs_ncg; i++)
						if (addr == cgsblock(fs, i) <<
								FRGSHIFT)
							break;
					if (i == fs->fs_ncg)
						cur_cgrp = 0;
					else
						cur_cgrp = i + 1;
					type = objsz = SB;
					if (cur_cgrp + count - 1 > fs->fs_ncg) {
						tcount = fs->fs_ncg - cur_cgrp;
						if (!star)
							end++;
					}
				}
				if (sb->fs_magic != FS_MAGIC) {
					cur_cgrp = 0;
					if (!override) {
						printf("invalid super block ");
						printf("magic word\n");
						cur_cgrp--;
						error++;
						return;
					}
				}
				if (cur_cgrp == 0)
					printf("\tsuper block:\n");
				else {
					printf("\tsuper block in cylinder ");
					printf("group ");
					print(cur_cgrp - 1, 0, 0, 0);
					printf(":\n");
				}
				printsb(sb);
				if (tcount)
					printf("\n");
			}
			cur_cgrp--;
			if (end) {
				printf("end of super blocks\n");
				error++;
			}
			return;
		default:
			error++;
			printf("no such print option\n");
			return;
		}
}

/*
 * valid_addr - call check_addr to validate the current address.
 */
valid_addr()
{
	short	eof_flag, end = 0, eof = 0;
	long	tcount = count;

	if (!trapped)
		return(1);
	if (cur_bytes < 0) {
		cur_bytes = 0;
		if (blocksize > filesize) {
			printf("beginning of file\n");
		} else {
			if (type == BLOCK)
				printf("beginning of block\n");
			else
				printf("beginning of fragment\n");
		}
		error++;
		return(0);
	}
	count = 1;
	check_addr(1, &end, &eof, (filesize < blocksize));
	count = tcount;
	if (eof) {
		printf("end of file\n");
		error++;
		return(0);
	}
	if (end == 2) {
		if (erraddr > addr) {
			if (type == BLOCK)
				printf("beginning of block\n");
			else
				printf("beginning of fragment\n");
			error++;
			return(0);
		}
	}
	if (end) {
		if (type == BLOCK)
			printf("end of block\n");
		else
			printf("end of fragment\n");
		error++;
		return(0);
	}
	return(1);
}

/*
 * check_addr - check if the address crosses the end of block or
 *	end of file.  Return the proper count.
 */
check_addr(eof_flag, end, eof, keep_on)
	short	eof_flag, *end, *eof, keep_on;
{
	long	temp, tcount = count, taddr = addr, tcur_bytes = cur_bytes;

	if (bcomp(addr + count * objsz - 1) ||
	    (keep_on && taddr < (bmap(cur_block) << FRGSHIFT))) {
		error = 0;
		addr = taddr;
		cur_bytes = tcur_bytes;
		if (keep_on) {
			if (addr < erraddr) {
				if (cur_bytes < 0) {
					(*end) = 2;
					return;
				}
				temp = cur_block - lblkno(fs, cur_bytes);
				cur_block -= temp;
				if ((addr = bmap(cur_block) << FRGSHIFT) == 0) {
					cur_block += temp;
					return;
				}
				temp = tcur_bytes - cur_bytes;
				addr += temp;
				cur_bytes += temp;
				return;
			} else {
				if (cur_bytes >= filesize) {
					(*eof)++;
					return;
				}
				temp = lblkno(fs, cur_bytes) - cur_block;
				cur_block += temp;
				if ((addr = bmap(cur_block) << FRGSHIFT) == 0) {
					cur_block -= temp;
					return;
				}
				temp = tcur_bytes - cur_bytes;
				addr += temp;
				cur_bytes += temp;
				return;
			}
		}
		tcount = (blkroundup(fs, addr+1)-addr) / objsz;
		if (!star)
			(*end) = 2;
	}
	addr = taddr;
	cur_bytes = tcur_bytes;
	if (eof_flag) {
		if (blocksize > filesize) {
			if (cur_bytes >= filesize) {
				tcount = 0;
				(*eof)++;
			} else if (tcount > (filesize - cur_bytes) / objsz) {
				tcount = (filesize - cur_bytes) / objsz;
				if (!star || tcount == 0)
					(*eof)++;
			}
		} else {
			if (cur_bytes >= blocksize) {
				tcount = 0;
				(*end)++;
			} else if (tcount > (blocksize - cur_bytes) / objsz) {
				tcount = (blocksize - cur_bytes) / objsz;
				if (!star || tcount == 0)
					(*end)++;
			}
		}
	}
	return(tcount);
}

/*
 * print_check - check if the index needs to be printed and delete
 *	rows of zeros from the output.
 */
unsigned long *
print_check(lptr, tcount, tbase, i)
	unsigned long	*lptr;
	long		*tcount;
	short		tbase;
	register int	i;
{
	register int	j, k, temp = BYTESPERLINE / objsz;
	short		first_time = 0;
	unsigned long	*tlptr;
	unsigned short	*tsptr, *sptr;

	sptr = (unsigned short *)lptr;
	if (i == 0)
		first_time = 1;
	if (i % temp == 0) {
		if (*tcount >= temp - 1) {
			if (objsz == SHORT)
				tsptr = sptr;
			else
				tlptr = lptr;
			k = *tcount - 1;
			for (j = i; k--; j++)
				if (objsz == SHORT) {
					if (*tsptr++ != 0)
						break;
				} else {
					if (*tlptr++ != 0)
						break;
				}
			if (j > (i + temp - 1)) {
				j = (j - i) / temp;
				while (j-- > 0) {
					if (objsz == SHORT)
						sptr += temp;
					else
						lptr += temp;
					*tcount -= temp;
					i += temp;
					addr += BYTESPERLINE; 
					cur_bytes += BYTESPERLINE;
				}
				if (first_time)
					printf("*");
				else
					printf("\n*");
			}
			if (i)
				printf("\n");
			index(tbase);
		} else {
			if (i)
				printf("\n");
			index(tbase);
		}
	}
	if(objsz == SHORT)
		return((unsigned long *)sptr);
	else
		return(lptr);
}

/*
 * index - print a byte index for the printout in base b
 *	with leading zeros.
 */
index(b)
	int	b;
{
	int	tbase = base;

	base = b;
	print(addr, 8, 8, 1);
	printf(":\t");
	base = tbase;
}

/*
 * print - print out the value to digits places with/without
 *	leading zeros and right/left justified in the current base.
 */
print(value, fieldsz, digits, lead)
	int		value, fieldsz, digits, lead;
{
	register int	i, left = 0;
	char		mode = BASE[base - OCTAL];
	char		*string = &scratch[0];

	if (digits < 0) {
		left = 1;
		digits *= -1;
	}
	if (base != HEX)
		if (digits)
			digits = digits + (digits - 1)/((base >> 1) - 1) + 1;
		else
			digits = 1;
	if (lead) {
		if (left)
			sprintf(string, "%%%c%d%d.%d%c",
				'-', 0, digits, lead, mode);
		else
			sprintf(string, "%%%d%d.%d%c", 0, digits, lead, mode);
	} else {
		if (left)
			sprintf(string, "%%%c%d%c", '-', digits, mode);
		else
			sprintf(string, "%%%d%c", digits, mode);
	}
	printf(string, value);
	for (i = 0; i < fieldsz - digits; i++)
		printf(" ");
}

/*
 * Print out the contents of a superblock.
 */
printsb(fs)
	struct fs *fs;
{
	int c, i, j, k, size;

#ifdef FS_42POSTBLFMT
	if (fs->fs_postblformat == FS_42POSTBLFMT)
		fs->fs_nrpos = 8;
	printf("magic\t%x\tformat\t%s\ttime\t%s", fs->fs_magic,
	    fs->fs_postblformat == FS_42POSTBLFMT ? "static" : "dynamic",
	    ctime(&fs->fs_time));
#else
	printf("magic\t%x\ttime\t%s",
	    fs->fs_magic, ctime(&fs->fs_time));
#endif
	printf("nbfree\t%d\tndir\t%d\tnifree\t%d\tnffree\t%d\n",
	    fs->fs_cstotal.cs_nbfree, fs->fs_cstotal.cs_ndir,
	    fs->fs_cstotal.cs_nifree, fs->fs_cstotal.cs_nffree);
	printf("ncg\t%d\tncyl\t%d\tsize\t%d\tblocks\t%d\n",
	    fs->fs_ncg, fs->fs_ncyl, fs->fs_size, fs->fs_dsize);
	printf("bsize\t%d\tshift\t%d\tmask\t0x%08x\n",
	    fs->fs_bsize, fs->fs_bshift, fs->fs_bmask);
	printf("fsize\t%d\tshift\t%d\tmask\t0x%08x\n",
	    fs->fs_fsize, fs->fs_fshift, fs->fs_fmask);
	printf("frag\t%d\tshift\t%d\tfsbtodb\t%d\n",
	    fs->fs_frag, fs->fs_fragshift, fs->fs_fsbtodb);
	printf("cpg\t%d\tbpg\t%d\tfpg\t%d\tipg\t%d\n",
	    fs->fs_cpg, fs->fs_fpg / fs->fs_frag, fs->fs_fpg, fs->fs_ipg);
	printf("minfree\t%d%%\toptim\t%s\tmaxcontig %d\tmaxbpg\t%d\n",
	    fs->fs_minfree, fs->fs_optim == FS_OPTSPACE ? "space" : "time",
	    fs->fs_maxcontig, fs->fs_maxbpg);
#ifdef FS_42POSTBLFMT
	printf("rotdelay %dms\theadswitch %dus\ttrackseek %dus\trps\t%d\n",
	    fs->fs_rotdelay, fs->fs_headswitch, fs->fs_trkseek, fs->fs_rps);
	printf("ntrak\t%d\tnsect\t%d\tnpsect\t%d\tspc\t%d\n",
	    fs->fs_ntrak, fs->fs_nsect, fs->fs_npsect, fs->fs_spc);
	printf("trackskew %d\tinterleave %d\n",
	    fs->fs_trackskew, fs->fs_interleave);
#else
	printf("rotdelay %dms\trps\t%d\n",
	    fs->fs_rotdelay, fs->fs_rps);
	printf("ntrak\t%d\tnsect\t%d\tspc\t%d\n",
	    fs->fs_ntrak, fs->fs_nsect, fs->fs_spc);
#endif
	printf("nindir\t%d\tinopb\t%d\tnspf\t%d\n",
	    fs->fs_nindir, fs->fs_inopb, fs->fs_nspf);
	printf("sblkno\t%d\tcblkno\t%d\tiblkno\t%d\tdblkno\t%d\n",
	    fs->fs_sblkno, fs->fs_cblkno, fs->fs_iblkno, fs->fs_dblkno);
	printf("sbsize\t%d\tcgsize\t%d\tcgoffset %d\tcgmask\t0x%08x\n",
	    fs->fs_sbsize, fs->fs_cgsize, fs->fs_cgoffset, fs->fs_cgmask);
	printf("csaddr\t%d\tcssize\t%d\tshift\t%d\tmask\t0x%08x\n",
	    fs->fs_csaddr, fs->fs_cssize, fs->fs_csshift, fs->fs_csmask);
	printf("cgrotor\t%d\tfmod\t%d\tronly\t%d\n",
	    fs->fs_cgrotor, fs->fs_fmod, fs->fs_ronly);
#ifdef FS_42POSTBLFMT
	if (fs->fs_cpc != 0)
		printf("blocks available in each of %d rotational positions",
		     fs->fs_nrpos);
	else
		printf("insufficient space to maintain rotational tables\n");
#endif
	for (c = 0; c < fs->fs_cpc; c++) {
		printf("\ncylinder number %d:", c);
#ifdef FS_42POSTBLFMT
		for (i = 0; i < fs->fs_nrpos; i++) {
			if (fs_postbl(fs, c)[i] == -1)
				continue;
			printf("\n   position %d:\t", i);
			for (j = fs_postbl(fs, c)[i], k = 1; ;
			     j += fs_rotbl(fs)[j], k++) {
				printf("%5d", j);
				if (k % 12 == 0)
					printf("\n\t\t");
				if (fs_rotbl(fs)[j] == 0)
					break;
			}
		}
#else
		for (i = 0; i < NRPOS; i++) {
			if (fs->fs_postbl[c][i] == -1)
				continue;
			printf("\n   position %d:\t", i);
			for (j = fs->fs_postbl[c][i], k = 1; ;
			     j += fs->fs_rotbl[j], k++) {
				printf("%5d", j);
				if (k % 12 == 0)
					printf("\n\t\t");
				if (fs->fs_rotbl[j] == 0)
					break;
			}
		}
#endif
	}
	printf("\ncs[].cs_(nbfree,ndir,nifree,nffree):\n\t");
	for (i = 0, j = 0; i < fs->fs_cssize; i += fs->fs_bsize, j++) {
		size = fs->fs_cssize - i < fs->fs_bsize ?
		    fs->fs_cssize - i : fs->fs_bsize;
		fs->fs_csp[j] = (struct csum *)calloc(1, size);
		lseek(fd, fsbtodb(fs, (fs->fs_csaddr + j * fs->fs_frag)) *
		    fs->fs_fsize / fsbtodb(fs, 1), 0);
		if (read(fd, fs->fs_csp[j], size) != size) {
			for (j--; j >= 0; j--)
				free(fs->fs_csp[j]);
			return;
		}
	}
	for (i = 0; i < fs->fs_ncg; i++) {
		struct csum *cs = &fs->fs_cs(fs, i);
		if (i && i % 4 == 0)
			printf("\n\t");
		printf("(%d,%d,%d,%d) ",
		    cs->cs_nbfree, cs->cs_ndir, cs->cs_nifree, cs->cs_nffree);
	}
	for (j--; j >= 0; j--)
		free(fs->fs_csp[j]);
	printf("\n");
	if (fs->fs_ncyl % fs->fs_cpg) {
		printf("cylinders in last group %d\n",
		    i = fs->fs_ncyl % fs->fs_cpg);
		printf("blocks in last group %d\n",
		    i * fs->fs_spc / NSPB(fs));
	}
}

/*
 * Print out the contents of a cylinder group.
 */
printcg(cg)
	struct cg *cg;
{
	int i,j;

	printf("\ncg %d:\n", cg->cg_cgx);
#ifdef FS_42POSTBLFMT
	printf("magic\t%x\ttell\t%x\ttime\t%s",
	    fs->fs_postblformat == FS_42POSTBLFMT ?
	    ((struct ocg *)cg)->cg_magic : cg->cg_magic,
	    fsbtodb(fs, cgtod(fs, cg->cg_cgx)) * fs->fs_fsize / fsbtodb(fs, 1),
	    ctime(&cg->cg_time));
#else
	printf("magic\t%x\ttell\t%x\ttime\t%s",
	    cg->cg_magic,
	    fsbtodb(fs, cgtod(fs, cg->cg_cgx)) * fs->fs_fsize / fsbtodb(fs, 1),
	    ctime(&cg->cg_time));
#endif
	printf("cgx\t%d\tncyl\t%d\tniblk\t%d\tndblk\t%d\n",
	    cg->cg_cgx, cg->cg_ncyl, cg->cg_niblk, cg->cg_ndblk);
	printf("nbfree\t%d\tndir\t%d\tnifree\t%d\tnffree\t%d\n",
	    cg->cg_cs.cs_nbfree, cg->cg_cs.cs_ndir,
	    cg->cg_cs.cs_nifree, cg->cg_cs.cs_nffree);
	printf("rotor\t%d\tirotor\t%d\tfrotor\t%d\nfrsum",
	    cg->cg_rotor, cg->cg_irotor, cg->cg_frotor);
	for (i = 1, j = 0; i < fs->fs_frag; i++) {
		printf("\t%d", cg->cg_frsum[i]);
		j += i * cg->cg_frsum[i];
	}
	printf("\nsum of frsum: %d\niused:\t", j);
	pbits(cg_inosused(cg), fs->fs_ipg);
	printf("free:\t");
	pbits(cg_blksfree(cg), fs->fs_fpg);
	printf("b:\n");
	for (i = 0; i < fs->fs_cpg; i++) {
		if (cg_blktot(cg)[i] == 0)
			continue;
		printf("   c%d:\t(%d)\t", i, cg_blktot(cg)[i]);
#ifdef FS_42POSTBLFMT
		for (j = 0; j < fs->fs_nrpos; j++) {
			if (fs->fs_cpc == 0 ||
			    fs_postbl(fs, i % fs->fs_cpc)[j] == -1)
				continue;
			printf(" %d", cg_blks(fs, cg, i)[j]);
		}
#else
		for (j = 0; j < NRPOS; j++) {
			if (fs->fs_cpc == 0 ||
			    fs->fs_postbl[i % fs->fs_cpc][j] == -1)
				continue;
			printf(" %d", cg->cg_b[i][j]);
		}
#endif
		printf("\n");
	}
}

/*
 * Print out the contents of a bit array.
 */
pbits(cp, max)
	register char *cp;
	int max;
{
	register int i;
	int count = 0, j;

	for (i = 0; i < max; i++)
		if (isset(cp, i)) {
			if (count)
				printf(",%s", count % 6 ? " " : "\n\t");
			count++;
			printf("%d", i);
			j = i;
			while ((i+1)<max && isset(cp, i+1))
				i++;
			if (i != j)
				printf("-%d", i);
		}
	printf("\n");
}

/*
 * bcomp - used to check for block over/under flows when stepping through
 *	a file system.
 */
bcomp(addr)
	long	addr;
{
	if (override)
		return(0);
	if (lblkno(fs, addr) == (bhdr.fwd)->blkno)
		return(0);
	error++;
	return(1);
}

/*
 * bmap - maps the logical block number of a file into
 *	the corresponding physical block on the file
 *	system.
 */
long
bmap(bn)
	long			bn;
{
	register int		i, j;
	register struct dinode	*ip;
	int			sh;
	long			nb;

	ip = (struct dinode *)cur_ino;
	if (bn < NDADDR) {
		addr = (long)&ip->di_db[bn];
		cur_bytes = bn * BLKSIZE;
		return(nullblk(nb=get(LONG)) ? 0L : nb);
	}

	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		printf("file too big\n");
		error++;
		return(0L);
	}
	addr = (long)&ip->di_ib[NIADDR - j];
	nb = get(LONG);
	if (nb == 0)
		return(0L);
	for (; j <= NIADDR; j++) {
		sh /= NINDIR(fs);
		addr = (nb << FRGSHIFT) + ((bn / sh) % NINDIR(fs)) * LONG;
		if (nullblk(nb = get(LONG)))
			return(0L);
	}
	return(nb);
}
