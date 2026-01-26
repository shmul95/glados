##
## EPITECH PROJECT, 2026
## Template Makefile
## File description:
## Makefile
##

BIN_PATH	:= ./bin
LIB_PATH	:= ./lib

INC_PATH	:= ./inc
SRC_PATH 	:= ./src

RUNE		:= $(BIN_PATH)/.rune-raw

NAME 		:= my_project

SRC			:= $(SRC_PATH)/main.ru

FLAGS 		:= -I$(INC_PATH) -L$(LIB_PATH) -lstd


all: $(NAME)

$(NAME):
	$(RUNE) build $(SRC) -o $(NAME) $(FLAGS)
	@echo "Build complete: ./$(NAME)"

clean:
	rm -rf $(shell find . -type f -name "*.o")

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
