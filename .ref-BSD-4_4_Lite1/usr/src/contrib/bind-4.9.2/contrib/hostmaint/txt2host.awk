BEGIN { origin = "msu.edu"; FS = ":" }
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
  noabbr = 0;
  comment = "";
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
    if (opt == "global") {
      global = 1;
    }
    if (opt == "noabbr") {
      noabbr = 1;
    }
    if (c) opts = substr(opts,c+1,length(opts))
    else break
  }
  if (cpu != "" || opsys != "") comment = opsys ": " cpu ": " comment
  nn = split(names,nlist,",")
  printf "%s\t", ip
  for (n=1; n <= nn; n++) {
    name = nlist[n];
    sub("^  *","",name)
    sub("  *$","",name)
    np = split(name,part,".")
    printf "%s ", name;
    if (np > 3 && global) printf "%s.%s.%s ",part[1], part[np-1], part[np];
    if (np > 1 && ! noabbr) printf "%s ", part[1];
  }
  if (comment != "") printf "\t# %s", comment
  printf "\n";
  next;
}
/^#/ { print }
/^[0-9]/ { print }
/^;/ { print "#" substr($0,2,length($0)) }
/^MX:/ { print "#" $0 }
{ next }
