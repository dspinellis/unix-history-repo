BEGIN { origin = "msu.edu"; nredef = 0
  }
/^[0-9]/ {
  global = 0
  nn = 0
  c = index($0,"#")
  if (c == 0) {
    nl = split($0,nlist)
    comment = ""
  } else {
    nl = split(substr($0,1,c-1),nlist)
    comment = substr($0,c+1,length($0))
  }
  ip = nlist[1]
  printf "HOST: %s: ", ip
  for (n=2; n <= nl; n++) {
    name = nlist[n]
    if (name != "") {
      np = split(name,part,".")
      found = 0
      for (i=0; i < nn; i++) {
	if (part[1] == pname[i]) {
	  if (np == 3) global = 1
	  found = 1
	  break
	}
      }
      if (!found) {
	if (nn) printf ", "
	printf "%s", name
	pname[nn++] = part[1]
      }
    }
  }
  sub("^  *","",comment)
  sub("  *$","",comment)
  cpu = ""
  opsys = ""
  if (comment ~ /:.*:/) {
    i = index(comment,":")
    opsys = substr(comment,1,i-1)
    comment = substr(comment,i+1,length(comment))
    i = index(comment,":")
    cpu = substr(comment,1,i-1)
    comment = substr(comment,i+1,length(comment))
    sub("^ *"," ",cpu)
    sub("^  *","",comment)
  }
  printf ":%s:%s:: ", cpu, opsys
  if (global) printf "global"
  if (comment != "") {
    if (global) printf ","
    printf "comment=%s", comment
  }
  printf "\n"
  next
}
/^#MX:/ { print substr($0,2,length($0)); next }
/^#/ { print ";" substr($0,2,length($0)) }
/^HOST:/ { print }
/^MX:/ { print }
/^;/ { print }
{ next }
