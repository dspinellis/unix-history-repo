{ name = $1
  if (substr(name,length(name),1) == ".") name = substr(name,1,length(name)-1)
}
$4 == "A" {
  hostip[name] = $5
}
$4 == "HINFO" {
  hinfo[name] = "	# " $6 ": " $5 ":"
}
$4 == "CNAME" {
  cname[$5] = cname[$5] " " name
}
END {
  for (name in hostip) {
    print hostip[name] "	" name cname[name] hinfo[name]
  }
}
