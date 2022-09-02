.PHONY: orderbot-maestro orderbot-blockfrost format lint

EXTENSIONS := -o -XTypeApplications -o -XPatternSynonyms -o -XBangPatterns -o -XUnboxedTuples

orderbot-maestro:
	cabal run geniusyield-orderbot-exe -- config-maestro.json

orderbot-blockfrost:
	cabal run geniusyield-orderbot-exe -- config-blockfrost.json

# Run formatters
format:
	fourmolu -i --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs -E geniusyield-framework)

# Check formatting (without making changes)
format_check:
	fourmolu --mode check --check-idempotence $(EXTENSIONS) $(shell env -C . fd -ehs -E geniusyield-framework)

# Apply hlint suggestions
lint:
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check:
	hlint $(shell fd -ehs)
