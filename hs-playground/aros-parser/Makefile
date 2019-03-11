Calc : Tokens.hs Grammar.hs AROS.hs
	ghc --make AROS

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

clean:
	rm -f AROS Grammar.hs Tokens.hs *.o *.hi
