STACK             := stack
STACK_WORK_DIR    := .stack-work
NPROCS			  := $$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
STACK_BUILD_FLAGS := --jobs $(NPROCS)

STACK_BINARY_NAME := glados-exe
NAME 			  := glados

all:
	$(STACK) build $(STACK_BUILD_FLAGS)
	@find $(STACK_WORK_DIR)/install -type f -name $(STACK_BINARY_NAME) -exec cp {} ./${NAME} \;

tests:
	$(STACK) test $(STACK_BUILD_FLAGS)

clean:
	$(STACK) clean

fclean: clean
	rm -rf $(STACK_WORK_DIR) $(NAME)

re: fclean all

.PHONY: all clean fclean re tests
