BEGIN {
	i = 0;
	eflag = 0;
	User = Inmake = Indefs = 0;
	version = "v7";
	Makefile = install = postnews = "";
}

/^#/	{next;}
/^[ 	]*$/	{next;}

/^@@BeginMakefile$/	{Inmake = 1; next;}
/^@@EndMakefile$/	{Inmake = 0; next;}
/^@@BeginDefs$/	{Indefs = 1; next;}
/^@@EndDefs$/	{Indefs = 0; next;}
/^@@BeginUser$/	{User = 1; next;}

$1 == version {version = $1; next;}
$1 == Makefile {Makefile = $1; next;}
$1 == postnews {postnews = $1; next;}
$1 == install {install = $1; next;}

{
	if ((Inmake + Indefs + User) != 1) {
		printf "sequence error\n";
		eflag = 1;
		exit;
	}
	if ($1 ~ /^\+/) {omit = 0; nm = substr($1, 2, length($1)-1);}
	else if ($1 ~ /^\-/) {omit = 1; nm = substr($1, 2, length($1)-1);}
	else {omit = 0; nm = $1;}

	if (!User) {
		Mkparm[i] = Inmake;
		opt[i++] = nm;
		valid[nm] = "ok";
	}
	else if (valid[nm] == "") {
		printf "%s: invalid option\n", nm;
		eflag = 1;
		next;
	}
	if (omit) {
		Omit[nm] = 1;
		next;
	}
	else Omit[nm] = 0;

	if (NF >= 2) {
		val =$2;
		for (j = 3; j <= NF; j++)
			val = sprintf("%s=%s", val, $(j));
		gotit[nm] = val;
	}
}

END {
	if (eflag) {
		printf "echo No defs.h file created\n";
		exit;
	}
	if (Makefile == "") Makefile = version;
	if (postnews == "") postnews = version;
	if (install == "") install = version;
	printf "cp defs.templ defs.h; chmod +w defs.h\n";
	printf "ed - defs.h <<'EOF'\n";
	for (j = 0; j < i; j++) {
		if (Mkparm[j]) continue;
		vl=opt[j];
		do=gotit[vl];
		if (Omit[vl])
			printf "g/^#define[ 	]*%s[ 	]/s~^~/* ~\n", vl;
		else if (do != "")
			printf "g/^#define[ 	]*%s[ 	]/s~@@~%s~\n", vl, do;
	}
	printf "w\nq\nEOF\n";
	printf "cat >Makefile <<'EOF'\n";
	for (j = 0; j < i; j++) {
		if (Mkparm[j]) {
			vl=opt[j];
			printf "%s=", vl;
			if (!Omit[vl])
				printf "%s", gotit[vl];
			printf "\n";
		}
	}
	printf "EOF\ncat Makefile.%s >>Makefile\n", Makefile;
	printf "cp postnews.%s postnews; chmod +x postnews\n", postnews;
	printf "cp install.%s install; chmod +x install\n", install;
}
