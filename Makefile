LISP_DIRECTORY   := Lisp
RUNE_DIRECTORY   := Rune

LISP_BINARY_NAME := glados-exe
RUNE_BINARY_NAME := rune-exe


all: lisp rune move_binaries

clean:
	@$(MAKE) -C $(LISP_DIRECTORY) clean
	@$(MAKE) -C $(RUNE_DIRECTORY) clean

fclean:
	@$(MAKE) -C $(LISP_DIRECTORY) fclean
	@$(MAKE) -C $(RUNE_DIRECTORY) fclean
	rm -f glados rune

re: fclean all

tests:
	@$(MAKE) -C $(LISP_DIRECTORY) tests
	@$(MAKE) -C $(RUNE_DIRECTORY) tests

coverage:
	@$(MAKE) -C $(LISP_DIRECTORY) coverage
	@$(MAKE) -C $(RUNE_DIRECTORY) coverage

lisp:
	@$(MAKE) -C $(LISP_DIRECTORY)

rune:
	@$(MAKE) -C $(RUNE_DIRECTORY)

move_binaries:
	@mv $(LISP_DIRECTORY)/glados .
	@mv $(RUNE_DIRECTORY)/rune .

.PHONY: all clean fclean re tests coverage lisp rune move_binaries
