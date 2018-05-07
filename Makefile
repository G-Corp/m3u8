HAS_ELIXIR=1

include bu.mk

release: dist lint tag ## Tag and release to hex.pm
	$(verbose) $(REBAR) hex publish
