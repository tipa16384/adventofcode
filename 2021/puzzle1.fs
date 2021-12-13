256 Constant max-line
Create line-buffer max-line 2 + allot
0 Value fd-in

s" puzzle1a.dat" r/o open-file throw Value fd-in

." File open, maybe?"

: list-ints
begin
    line-buffer max-line fd-in read-line throw
while
    line-buffer