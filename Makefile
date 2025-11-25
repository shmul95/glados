LISP_DIRECTORY   := Lisp
RUNE_DIRECTORY   := Rune

LISP_BINARY_NAME := glados-exe
RUNE_BINARY_NAME := rune-exe


all: lisp rune symlinks

clean:
	@$(MAKE) -C $(LISP_DIRECTORY) clean
	@$(MAKE) -C $(RUNE_DIRECTORY) clean

fclean:
	@$(MAKE) -C $(LISP_DIRECTORY) fclean
	@$(MAKE) -C $(RUNE_DIRECTORY) fclean
 	rm -f glados # rune

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

symlinks:
	@ln -sf "$$(find $(LISP_DIRECTORY)/.stack-work -type f -name $(LISP_BINARY_NAME) | head -n 1)" glados
	@ln -sf "$$(find $(RUNE_DIRECTORY)/.stack-work -type f -name $(RUNE_BINARY_NAME) | head -n 1)" rune

.PHONY: all clean fclean re tests coverage lisp rune
