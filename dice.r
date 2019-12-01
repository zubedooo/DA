#dice <- strsplit(intToUtf8(0x2680L:0x2685), split = "")[[1]]

roll  <- function() {
  die = 1:6
  d = sample(x = die, size = 2, replace = F)
  
  print(d[1])
  print(d[2])
 print(d[1]+d[2])
  #glue::glue("{d[1]} + {d[2]} = {sum(d)}")
 glue::glue("{d[1]}+{d[2]}={sum(d)}")
  return(d)
}

d1=roll()

print(d1[1])
print(d1[2])
print(d1[1]+d1[2])