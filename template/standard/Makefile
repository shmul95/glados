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

REPO_PATH	:= github:rune-prog-lang/glados/344-nix-build
RESULT_PATH := ./result

RUNE		:= $(BIN_PATH)/.rune-raw

NAME 		:= my_project

SRC			:= $(SRC_PATH)/main.ru

FLAGS 		:= -I$(INC_PATH) -L$(LIB_PATH) -lstd

all: $(NAME)

$(NAME):
	$(RUNE) build $(SRC) -o $(NAME) $(FLAGS)
	@echo "Build complete: ./$(NAME)"

init:
	@echo "Building Rune compiler via Nix..."
	nix build $(REPO_PATH) --refresh
	@cp -rf $(RESULT_PATH)/* .
	@rm $(RESULT_PATH)
	@echo "Init complete. Compiler and StdLib copied to ./bin and ./lib"

clean:
	rm -rf $(shell find . -type f -name "*.o")

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
