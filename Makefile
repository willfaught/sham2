main: Check.hs Context.hs Main.hs Parse.hs Reduce.hs Substitute.hs Syntax.hs
	ghc --make Check.hs Context.hs Main.hs Parse.hs Reduce.hs Substitute.hs Syntax.hs -o sham

clean:
	rm *.hi
