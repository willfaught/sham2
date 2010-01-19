files = Check.hs Context.hs Main.hs Parse.hs Reduce.hs Substitute.hs Syntax.hs
cflags = -optc-m32 -opta-m32 -optl-m32

main: $(files)
	ghc --make -o sham $(cflags) $(files)

clean:
	rm -f sham *.hi *.o
