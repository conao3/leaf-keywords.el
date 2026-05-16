## Makefile

all:

REPO_USER    := conao3
PACKAGE_NAME := leaf-keywords
REPO_NAME    := leaf-keywords.el

EMACS        ?= emacs
ELS          := leaf-keywords.el

GIT_HOOKS    := pre-commit

LEAF_REPO    := https://github.com/conao3/leaf.el.git
LEAF_REF     := 1e363fe114310a6af5f3f770be795657307ad3d1
DEPS_DIR     := .deps
LEAF_DIR     := $(DEPS_DIR)/leaf

BATCH        := $(EMACS) -Q --batch -L . -L $(LEAF_DIR)

##################################################

.PHONY: all test clean

all: git-hook help

git-hook: $(GIT_HOOKS:%=.git/hooks/%)

.git/hooks/%: git-hooks/%
	cp -a $< $@

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Install git-hook to your local .git folder)
	$(info   - make test     # Test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files and fetched dependencies)
	$(info )

##############################

$(LEAF_DIR):
	git clone $(LEAF_REPO) $(LEAF_DIR)
	git -C $(LEAF_DIR) checkout --quiet $(LEAF_REF)

%.elc: %.el $(LEAF_DIR)
	$(BATCH) -f batch-byte-compile $<

##############################

test: $(LEAF_DIR) $(ELS:%.el=%.elc)
	$(BATCH) -l leaf-keywords-tests.el -f cort-test-run

clean:
	rm -rf $(ELS:%.el=%.elc) $(DEPS_DIR)
