LISP_DIRECTORY   := LispLang
RUNE_DIRECTORY   := RuneLang

LISP_BINARY_NAME := glados
RUNE_BINARY_NAME := rune


all: move_binaries

clean:
	@$(MAKE) -C $(LISP_DIRECTORY) clean
	@$(MAKE) -C $(RUNE_DIRECTORY) clean

fclean:
	@$(MAKE) -C $(LISP_DIRECTORY) fclean
	@$(MAKE) -C $(RUNE_DIRECTORY) fclean
	@find . -maxdepth 1 -type f \( -name $(LISP_BINARY_NAME) -o -name $(RUNE_BINARY_NAME) \) -delete

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

move_binaries: lisp rune
	@echo "Move binaries to root directory..."
	@if [ -f $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) ]; then \
        cp $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) .; \
    else \
        echo "Error: $(LISP_DIRECTORY)/$(LISP_BINARY_NAME) not found"; \
        exit 1; \
    fi
	@if [ -f $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) ]; then \
		cp $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) .; \
	else \
		echo "Error: $(RUNE_DIRECTORY)/$(RUNE_BINARY_NAME) not found"; \
		exit 1; \
	fi

lib:
	@$(MAKE) -C $(RUNE_DIRECTORY) lib

.PHONY: all clean fclean re tests coverage lisp rune move_binaries lib
