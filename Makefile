all:

include Makefunc.mk

TOP          := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW    := $(sort $(shell compgen -c emacs- | xargs))
EXPECT_EMACS  += 24.4 24.5
EXPECT_EMACS  += 25.1 25.2 25.3
EXPECT_EMACS  += 26.1 26.2

ALL_EMACS    := $(filter $(EMACS_RAW),$(EXPECT_EMACS:%=emacs-%))

DEPENDS      := leaf

TESTFILE     := leaf-keywords-tests.el
ELS          := leaf-keywords.el

CORTELS      := $(TESTFILE) cort-test.el

EMACS        ?= emacs
BATCH        := $(EMACS) -Q --batch -L $(TOP) $(DEPENDS:%=-L ./%/)

##################################################

.PHONY: all git-hook build check allcheck test clean clean-v

all: git-hook build

##############################

git-hook:
	cp -a git-hooks/* .git/hooks/

build: $(ELS:%.el=%.elc)

%.elc: %.el $(DEPENDS)
	$(BATCH) $(DEPENDS:%=-L %/) -f batch-byte-compile $<

##############################
#
#  one-time test (on top level)
#

check: build
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################
#
#  multi Emacs version test (on independent environment)
#

allcheck: $(ALL_EMACS:%=.make/verbose-%)
	@echo ""
	@cat $(^:%=%/.make-test-log) | grep =====
	@rm -rf $^

.make/verbose-%: $(DEPENDS)
	mkdir -p $@
	cp -rf $(ELS) $(CORTELS) $(DEPENDS) $@/
	cd $@; echo $(ELS) | xargs -n1 -t $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -f batch-byte-compile
	cd $@; $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -l $(TESTFILE) -f cort-test-run | tee .make-test-log

##############################
#
#  silent `allcheck' job
#

test: $(ALL_EMACS:%=.make/silent-%)
	@echo ""
	@cat $(^:%=%/.make-test-log) | grep =====
	@rm -rf $^

.make/silent-%: $(DEPENDS)
	@mkdir -p $@
	@cp -rf $(ELS) $(CORTELS) $(DEPENDS) $@/
	@cd $@; echo $(ELS) | xargs -n1 $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -f batch-byte-compile
	@cd $@; $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -l $(TESTFILE) -f cort-test-run > .make-test-log 2>&1

##############################

clean:
	rm -rf $(ELC) $(DEPENDS) .make

##############################
#
#  depend files
#

leaf:
	curl -L https://github.com/conao3/leaf.el/archive/master.tar.gz > $@.tar.gz
	mkdir $@ && tar xf $@.tar.gz -C $@ --strip-components 1
	rm -rf $@.tar.gz
