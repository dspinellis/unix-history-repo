BEGIN { FS = ":" }
/^NET/ {
  ip = $2;
  while (substr(ip,1,1) == " ") ip = substr(ip,2,length(ip)-1);
  while (substr(ip,length(ip),1) == " ") ip = substr(ip,1,length(ip)-1);
  name = $3;
  while (substr(name,1,1) == " ") name = substr(name,2,length(name)-1);
  while (substr(name,length(name),1) == " ") name = substr(name,1,length(name)-1);
  ni = 1;
  j = 1;
  for (i=1; i < length(ip); i++) {
    if (substr(ip,i,1) == ".") {
      ippart[ni++] = substr(ip,j,i-j);
      j = i+1;
    }
  }
  ippart[ni] = substr(ip,j,i);
  ip = ippart[1]
  if (ippart[1]+0 >= 128) ip = ip "." ippart[2]
  if (ippart[1]+0 >= 192) ip = ip "." ippart[3]
  printf "%s\t", name;
  if (length(name) < 8) printf "\t"
  printf "%s", ip
  printf "\n";
  next;
}
/^#/ { printf "#%s\n", substr($0,2,length($0)) }
{ next }
