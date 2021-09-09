CC = ghc -o

SRC =	myalgo.hs

NAME =	pushswap_checker

all:	$(NAME)

$(NAME):
		$(CC) $(NAME) $(SRC)

clean:
		rm -rf $(NAME)
		rm -rf *.hi
		rm -rf *.o

fclean: clean
		rm -f $(NAME)

re:	fclean all