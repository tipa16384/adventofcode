let d$ = "e:\Documents\adventofcode\2021\puzzle1a.dat"

dim depths(2000)
open #1: name d$, access "input"
for i = 1 to 2000
    input #1: depths(i)
next i
close #1

let total = 0

for i = 2 to 2000
    if depths(i) > depths(i-1) then
        let total = total + 1
    end if
next i

print "Part 1:";total

let total = 0
let prev = depths(1) + depths(2) + depths(3)
for i = 2 to 1998
    let current = depths(i) + depths(i+1) + depths(i+2)
    if current > prev then
        let total = total + 1
    end if
    let prev = current
next i

print "Part 2:";total

end
