STACK             := stack
STACK_WORK_DIR    := .stack-work
NPROCS            := $$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
STACK_BUILD_FLAGS := --jobs $(NPROCS)
STACK_NIX_FLAGS   := --nix --system-ghc

STACK_BINARY_NAME := glados-exe
NAME              := glados

all:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) build $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS); \
	else \
		$(STACK) build $(STACK_BUILD_FLAGS); \
	fi
	@$(MAKE) copy_binary

tests:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) test $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS); \
	else \
		$(STACK) test $(STACK_BUILD_FLAGS); \
	fi

coverage:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) test $(STACK_BUILD_FLAGS) $(STACK_NIX_FLAGS) --coverage; \
	else \
		$(STACK) test $(STACK_BUILD_FLAGS) --coverage; \
	fi

clean:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) clean $(STACK_NIX_FLAGS); \
	else \
		$(STACK) clean; \
	fi

fclean:
	@if grep -q 'ID=nixos' /etc/os-release; then \
		$(STACK) clean $(STACK_NIX_FLAGS); \
	else \
		$(STACK) clean; \
	fi
	rm -rf $(STACK_WORK_DIR) $(NAME)

re: fclean all

copy_binary:
	@cp "$$(find $(shell pwd)/.stack-work -type f -name $(STACK_BINARY_NAME) | head -n 1)" $(NAME)

.PHONY: all clean fclean re test coverage copy_binary