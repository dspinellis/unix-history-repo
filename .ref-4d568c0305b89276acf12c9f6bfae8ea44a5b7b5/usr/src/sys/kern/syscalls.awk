awk < syscalls '\
	BEGIN { \
		sysent = "sysent";
		sysnames = "syscall.c";
		syshdr = "syscall.h";
		printf "/*\n * System call names.\n */\n" > sysnames; \
		printf "char *syscallnames[] = {\n" > sysnames; \
		printf "/*\n * System call numbers.\n */\n" > syshdr; \
	} \
	$1 ~ /^#[ 	]*if/ { \
		print > sysent; \
		savesyscall = syscall; \
		next;
	} \
	$1 ~ /^#[ 	]*el/ { \
		print > sysent; \
		syscall = savesyscall; \
		next; \
	} \
	$1 ~ /^#/ { \
		print > sysent; \
		next; \
	} \
	syscall != $1 { \
		printf "syscall number out of sync at %d; line is:\n", syscall; \
		print; \
		exit 1; \
	} \
	{	comment = $4; \
		for (i = 5; i <= NF; i++) \
			comment = comment " " $i; \
		if (NF < 5) \
			$5 = $4; \
	} \
	$2 == "STD" { \
		printf("\t%d, %s,\t\t\t/* %d = %s */\n", \
		    $3, $4, syscall, $5) > sysent; \
		printf("\t\"%s\",\t\t\t/* %d = %s */\n", \
		    $5, syscall, $5) > sysnames; \
		printf("#define\tSYS_%s\t%d\n", \
		    $5, syscall) > syshdr; \
	} \
	$2 == "COMPAT" { \
		printf("\tcompat(%d, %s),\t\t/* %d = old %s */\n", \
		    $3, $4, syscall, $5) > sysent; \
		printf("\t\"old %s\",\t\t/* %d = old %s */\n", \
		    $5, syscall, $5) > sysnames; \
		printf("\t\t\t\t/* %d is old %s */\n", \
		    syscall, comment) > syshdr; \
	} \
	$2 == "OBSOL" { \
		printf("\t0, nosys,\t\t\t/* %d = old %s */\n", \
		    syscall, comment) > sysent; \
		printf("\t\"#%d\",\t\t\t/* %d = old %s */\n", \
		    syscall, syscall, comment) > sysnames; \
		printf("\t\t\t\t/* %d is old %s */\n", \
		    syscall, comment) > syshdr; \
	} \
	$2 == "UNIMPL" { \
		printf("\t0, nosys,\t\t\t/* %d = %s */\n", \
		    syscall, comment) > sysent; \
		printf("\t\"#%d\",\t\t\t/* %d = %s */\n", \
		    syscall, syscall, comment) > sysnames; \
	} \
	{ syscall++ }\
	END { \
		printf("};\n") > sysnames; \
	}
	'
