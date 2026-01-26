##
## EPITECH PROJECT, 2026
## Template Makefile
## File description:
## Makefile
##

# name of your project
NAME 		:= my_project


BIN_PATH	:= ./bin
LIB_PATH	:= ./lib

INC_PATH	:= ./inc
SRC_PATH 	:= ./src

REPO_PATH	:= github:rune-prog-lang/glados
RESULT_PATH := ./result

RUNE		:= $(BIN_PATH)/.rune-raw

SRC			:= $(shell find $(SRC_PATH) -name "*.ru")

FLAGS 		:= -I$(INC_PATH) -L$(LIB_PATH) -lstd

all: $(NAME)

$(NAME):
	@$(RUNE) build $(SRC) -o $(NAME) $(FLAGS)

init:
	nix build $(REPO_PATH) --refresh
	@cp -rf $(RESULT_PATH)/* .
	@rm $(RESULT_PATH)

run: all
	@./$(NAME)

clean:
	@rm -rf $(shell find . -type f -name "*.o")

fclean: clean
	@rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
