BEGIN { FS = ":";
  errfile = "txt2named.err";
  if (origin == "") { origin = "msu.edu"}
}
/^MX/ {
  split($0,f," *: *")
  host = f[2]
  pref = f[3]
  mailx = f[4]
  if (length(host) > length(origin) && substr(host,length(host)-length(origin),length(origin)+1) == ("." origin)) {
    host = substr(host,1,length(host)-length(origin)-1);
  } else if (host == origin) {
    host = "@";
  } else {
    host = host ".";
  }
  printf "%s\tIN MX\t%s %s\n", host, pref, mailx "."
}
/^HOST/ {
  nf = split($0,f,FS)
  for (i=2; i<= 6; i++) {
    sub("^  *","",f[i])
    sub("  *$","",f[i])
  }
  ip = f[2]
  names = f[3]
  cpu = f[4]
  opsys = f[5]
  proto = f[6]
  opts = f[7]
  for (i=8; i <= nf; i++)  opts = opts FS f[i]
  global = 0
  comment = ""
  shuffle = 0
  dupok = 0
  nocname1 = 0
  noptr = 0
  for (;;) {
    sub("^  *","",opts)
    if (substr(opts,1,8) == "comment=") {
      comment = substr(opts,9,length(opts));
      break
    }
    c = index(opts,",")
    if (c) opt = substr(opts,1,c-1)
    else opt = opts
    sub("  *$","",opt)
    if (opt == "global") global = 1
    if (opt == "shuffle") shuffle = 1
    if (opt == "dupok") dupok = 1
    if (opt == "nocname1") nocname1 = 1
    if (opt == "nonamed") next
    if (opt == "noptr") noptr = 1
    if (c) opts = substr(opts,c+1,length(opts))
    else break
  }
  aflag = ""
  if (shuffle) dupok = 1
  if (dupok) aflag = aflag " DUPOK"
  if (noptr) aflag = aflag " NOPTR"
  if (aflag != "") aflag = "	;" aflag
  ns = split(proto,plist," *, *")
  tcp = "";
  udp = "";
  for (i=1; i <= ns; i++) {
    prot = plist[i];
    if (substr(prot,1,4) == "tcp/") tcp = tcp " " substr(prot,5,length(prot));
    if (substr(prot,1,4) == "udp/") udp = udp " " substr(prot,5,length(prot));
  }
  nn = split(names,nlist," *, *")
  for (n=1; n <= nn; n++) {
    name = nlist[n];
    np = split(name,part,".")
    sname = part[1];
    if (n == 1) {
      fullname = name ".";
      if (np > 1 && part[np-1] "." part[np] == origin) {
	if (np == 2) sname = "@";
	dept = "";
	if (np == 4) dept = "." part[2];
	if (shuffle) printf "%s%s\t60\tIN SA\t%s%s\n", sname, dept, ip, aflag
	else printf "%s%s\tIN A\t%s%s\n", sname, dept, ip, aflag
	if (cpu != "" || opsys != "") {
	  if (cpu == "") cpu = "-";
	  if (opsys == "") opsys = "-";
	  printf "%s%s\tIN HINFO\t\"%s\" \"%s\"\n", sname, dept, cpu, opsys;
	}
	if (tcp != "") {
	  printf "%s%s\tIN WKS\t%s\tTCP %s\n", sname, dept, ip, tcp;
	}
	if (udp != "") {
	  printf "%s%s\tIN WKS\t%s\tUDP %s\n", sname, dept, ip, udp;
	}
	if (np > 3 && global) {
	  if (nocname1) printf "%s\tIN A\t%s%s\n", sname, ip, aflag
	  else printf "%s\tIN CNAME\t%s%s\n", sname, fullname, aflag
	}
	if (comment != "") printf "%s%s\tIN TXT\t\"%s\"\n", sname, dept, comment
      } else {
	next;
      }
    } else {
      if (np == 1) {
	printf "%s%s\tIN CNAME\t%s%s\n", sname, dept, fullname, aflag
      } else if (np > 1 && part[np-1] "." part[np] == origin) {
	if (np == 2) sname = "@";
	dept = "";
	if (np == 4) dept = "." part[2];
	printf "%s%s\tIN CNAME\t%s%s\n", sname, dept, fullname, aflag
	if (np > 3 && global) printf "%s\tIN CNAME\t%s%s\n", sname, fullname, aflag
      } else {
	next;
      }
    }
  }
  next;
}
/^;/ { print $0 }
{ next }
