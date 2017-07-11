# Directory where this Makefile exists (the dotfiles directory)
EMACS_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

el-modules = init-core.el \
init-theme.el \
init-ivy.el \
init-eshell.el \
init.el

all: init $(el-modules)

clean:
	rm -fv *.el
	rm -fv *.elc
	rm -fv sh/*.sh

init: initialize.sh
initialize.sh: init.org
	bin/tangle init.org
install.sh: init.org
	bin/tangle init.org
run-init: init
	bash initialize.sh

%.el: %.org
	bin/tangle $<

byte-compile-all: all
	for f in *.el; do \
		bin/byte-compile $$f; \
	done

run: all
	for f in sh/*.sh; do \
		echo "Running: $$f"; \
		bash -l $$f; \
	done

install: run-init run install.sh
	bash -l install.sh
