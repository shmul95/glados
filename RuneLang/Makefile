STACK             := stack
STACK_WORK_DIR    := .stack-work
NPROCS            := $$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
STACK_BUILD_FLAGS := --jobs $(NPROCS)
STACK_NIX_FLAGS   := --nix --system-ghc

STACK_BINARY_NAME := rune-exe
NAME              := rune
REPO_DIR          := $(shell pwd)

STD_LIB_DIR       := $(REPO_DIR)/lib/std
STD_LIB_FILES     := $(wildcard $(STD_LIB_DIR)/*.ru)

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

lib:
	./rune build $(STD_LIB_FILES) -shared -o $(STD_LIB_DIR)/libstd.so
	@mkdir -p $(HOME)/.local/lib
	@cp $(STD_LIB_DIR)/libstd.so $(HOME)/.local/lib/

.PHONY: all clean fclean re test coverage copy_binary lib
