EMACS ?= emacs
PACKAGE_USER_DIR ?= $(HOME)/.emacs.d/elpa/30.2/develop

.PHONY: test

test:
	$(EMACS) --batch -L . -L test \
		--eval '(progn (require (quote package)) (setq package-user-dir "$(PACKAGE_USER_DIR)") (package-initialize))' \
		-l test/ytr-org-test.el \
		-f ert-run-tests-batch-and-exit
