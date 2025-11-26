LISP_DIRECTORY   := Lisp
RUNE_DIRECTORY   := Rune

LISP_BINARY_NAME := glados
RUNE_BINARY_NAME := rune


all: lisp rune move_binaries

clean:
	@$(MAKE) -C $(LISP_DIRECTORY) clean
	@$(MAKE) -C $(RUNE_DIRECTORY) clean

fclean:
	@$(MAKE) -C $(LISP_DIRECTORY) fclean
	@$(MAKE) -C $(RUNE_DIRECTORY) fclean
	@find . -maxdepth 1 -type l \( -name $(LISP_BINARY_NAME) -o -name $(RUNE_BINARY_NAME) \) -delete

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
	@if [ -L $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) ]; then \
        mv $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) .; \
    else \
        echo "Error: $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) not found"; \
        exit 1; \
    fi
	@if [ -L $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) ]; then \
		mv $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) .; \
	else \
		echo "Error: $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) not found"; \
		exit 1; \
	fi

.PHONY: all clean fclean re tests coverage lisp rune move_binaries
