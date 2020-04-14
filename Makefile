## Makefile

all:

REPO_USER    := conao3
PACKAGE_NAME := leaf-keywords
REPO_NAME    := leaf-keywords.el

EMACS        ?= emacs
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

##################################################

.PHONY: all

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
	$(info   - make clean    # Clean compiled files, docker conf files)
	$(info )
	$(info This Makefile required `cask`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

%.elc: %.el .cask
	cask exec $(EMACS) -Q --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

##############################

test: $(ELS:%.el=%.elc)
	cask exec $(EMACS) -Q --batch -L . -l leaf-keywords-tests.el -f cort-test-run
#	cask exec buttercup -L .

clean:
	rm -rf $(ELS:%.el=%.elc) .cask
