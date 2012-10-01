OBJDIR = obj

VERSION_SPEC = src/version.ads
VERSION      = $(GIT_REV)
GIT_REV      = `git describe --always`

BUILD_OPTS = -p

all: xfrm_proxy

git-rev: FORCE
	@if [ -d .git ]; then \
		if [ -r $@ ]; then \
			if [ "$$(cat $@)" != "$(GIT_REV)" ]; then \
				echo $(GIT_REV) > $@; \
			fi; \
		else \
			echo $(GIT_REV) > $@; \
		fi \
	fi

$(VERSION_SPEC): git-rev
	@echo "package Version is"                      > $@
	@echo "   Version_String : constant String :=" >> $@
	@echo "     \"$(VERSION)\";"                   >> $@
	@echo "end Version;"                           >> $@

xfrm_proxy: $(VERSION_SPEC)
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf $(OBJDIR)
	@rm -f git-rev

FORCE:
