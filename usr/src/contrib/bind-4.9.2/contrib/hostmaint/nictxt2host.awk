BEGIN { FS = ":" }
/^HOST/ {
  ips = $2;
  while (substr(ips,1,1) == " ") ips = substr(ips,2,length(ips)-1);
  while (substr(ips,length(ips),1) == " ") ips = substr(ips,1,length(ips)-1);
  names = $3;
  while (substr(names,1,1) == " ") names = substr(names,2,length(names)-1);
  while (substr(names,length(names),1) == " ") names = substr(names,1,length(names)-1);
  ni = 1;
  j = 1;
  for (i=1; i < length(ips); i++) {
    if (substr(ips,i,1) == ",") {
      iplist[ni++] = substr(ips,j,i-j);
      j = i+1;
    }
  }
  iplist[ni] = substr(ips,j,i);
  nn = 1;
  j = 1;
  for (i=1; i < length(names); i++) {
    if (substr(names,i,1) == ",") {
      nlist[nn++] = substr(names,j,i-j);
      j = i+1;
    }
  }
  nlist[nn] = substr(names,j,i);
  for (ii=1; ii <= ni; ii++) {
    ip = iplist[ii]
    while (substr(ip,1,1) == " ") ip = substr(ip,2,length(ip)-1);
    while (substr(ip,length(ip),1) == " ") ip = substr(ip,1,length(ip)-1);
    printf "%s\t", ip
    for (n=1; n <= nn; n++) {
      name = nlist[n];
      while (substr(name,1,1) == " ") name = substr(name,2,length(name)-1);
      while (substr(name,length(name),1) == " ") name = substr(name,1,length(name)-1);
      printf "%s ", name;
    }
    printf "\n";
  }
  next;
}
/^;/ { printf "#%s\n", substr($0,2,length($0)) }
{ next }
