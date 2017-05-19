all: Lexer.hs Grammar.hs

Lexer.hs: ./Tokens.x
	alex ./Tokens.x -o ./Lexer.hs

Grammar.hs: ./Grammar.y
	happy ./Grammar.y -o ./Grammar.hs

winclean:
	del *.o
	del *.hi

clean:
	rm *.o
	rm *.hi
	rm ./Fc/*.hi
	rm ./Fc/*.hi
