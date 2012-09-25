OBJDIR = obj

BUILD_OPTS = -p

all: xfrm_proxy

xfrm_proxy:
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf $(OBJDIR)
