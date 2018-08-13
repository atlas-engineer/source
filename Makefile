LISP?=sbcl
SSH_HOST=source.atlas.engineer
SSH_PORT=22
SSH_USER=root
SSH_TARGET_DIR=/root
CURRENT_DIR = $(PWD)
ASD = "/source.asd"
ASD_PATH = $(CURRENT_DIR)$(ASD)

help:
	@echo 'Makefile for Source. Please run this Makefile from             '
	@echo 'the directory it is located in.                                '
	@echo '                                                               '
	@echo 'Usage:                                                         '
	@echo '   make publish       create a binary for release              '
	@echo '   make rsync_push    push to a remote server                  '
	@echo '   make run           start the server                         '
	@echo '                                                               '


publish:
	$(LISP) --non-interactive --eval "(asdf:make :source)"

rsync_push:
	rsync -ar $(CURRENT_DIR) $(SSH_USER)@$(SSH_HOST):$(SSH_TARGET_DIR) --filter=':- .gitignore'

run:
	$(LISP) --noprint --eval "(asdf:load-asd \""$(ASD_PATH)"\")" --eval "(ql:quickload :athena)" --eval "(source:start :port 80)"

