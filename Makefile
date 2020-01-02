# Space-separated list of the dependencies of your project (include
# package-lint and/or buttercup if you want makel to use these tools):
ELPA_DEPENDENCIES=evil evil-test-helpers package-lint

# List of package archives to download above dependencies
# from. Available archives are: gnu, melpa, melpa-stable and org:
ELPA_ARCHIVES=gnu melpa

# List of ERT test files:
TEST_ERT_FILES=test/evil-goggles-test.el test/evil-tests-with-evil-goggles.el

# List of files to check for Emacs conventions:
LINT_CHECKDOC_FILES=$(wildcard *.el)

# List of files to check for packaging guidelines:
LINT_PACKAGE_LINT_FILES=$(wildcard *.el)

# List of files to check for compilation errors and warnings:
LINT_COMPILE_FILES=${LINT_CHECKDOC_FILES}

test/evil-tests.el:
	curl -s "https://raw.githubusercontent.com/emacs-evil/evil/40daccf17685ba4e59cf56563a8b0c4a386e109c/evil-tests.el" --output test/evil-tests.el

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.6.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
