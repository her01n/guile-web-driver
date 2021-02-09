default: .tested

GUILE ?= guile

.tested: web/*.scm test/*.scm
	hdt
	touch $@

clean:
	rm -rf .tested

GUILE_SITE_DIR ?= $(shell $(GUILE) -c "(display (%site-dir)) (newline)")

install:
	install -D --target-directory=$(GUILE_SITE_DIR)/web web/*.scm
	install -D --target-directory=$(GUILE_SITE_DIR)/web/driver web/driver/*.scm

uninstall:
	rm -rf $(GUILE_SITE_DIR)/web/driver
	rm -rf $(GUILE_SITE_DIR)/web/driver.scm
