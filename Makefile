BENCHMARK_RESULTS_DIR := build/benchmark_results

# Makefile for MultiGen development

.PHONY: help install test test-unit test-integration test-translation \
		test-py2c test-benchmark test-build test-memory-llvm clean lint format type-check \
		build docs docs-clean docs-serve benchmark benchmark-algorithms \
		benchmark-data-structures benchmark-report benchmark-clean check snap \
		check-wheel publish-test publish

# Default target
help:
	@echo "MultiGen Development Commands"
	@echo "========================="
	@echo ""
	@echo "Setup:"
	@echo "  install       Install development dependencies"
	@echo "  install-dev   Install with all development extras"
	@echo ""
	@echo "Testing:"
	@echo "  test          Run all tests with pytest"
	@echo "  test-unit     Run unit tests only"
	@echo "  test-translation  Run translation tests only"
	@echo "  test-build    Run batch build tests (includes translation)"
	@echo "  test-integration  Run integration tests only"
	@echo "  test-py2c     Run Python-to-C conversion tests"
	@echo "  test-benchmark    Run performance benchmarks"
	@echo "  test-memory-llvm  Run LLVM memory leak tests (AddressSanitizer)"
	@echo "  test-coverage Run tests with coverage report"
	@echo "  test-legacy   Display info about legacy unittest conversion"
	@echo ""
	@echo "Benchmarking:"
	@echo "  benchmark              Run all benchmarks across all backends"
	@echo "  benchmark-algorithms   Run algorithm benchmarks only"
	@echo "  benchmark-data-structures  Run data structure benchmarks only"
	@echo "  benchmark-report       Generate Markdown report from results"
	@echo "  benchmark-clean        Clean benchmark results"
	@echo ""
	@echo "Code Quality:"
	@echo "  lint          Run ruff linting"
	@echo "  format        Format code with ruff and isort"
	@echo "  format-check  Check code formatting without changes"
	@echo "  type-check    Run mypy type checking"
	@echo "  pre-commit    Install and run pre-commit hooks"
	@echo ""
	@echo "Build & Publish:"
	@echo "  build         Build package for distribution"
	@echo "  clean         Clean build artifacts"
	@echo "  check-wheel   Verify wheel with twine check"
	@echo "  publish-test  Publish to TestPyPI"
	@echo "  publish       Publish to PyPI"
	@echo ""
	@echo "Documentation:"
	@echo "  docs          Build Sphinx documentation"
	@echo "  docs-clean    Clean documentation build"
	@echo "  docs-serve    Open documentation in browser"

# Installation
install:
	uv run pip install -e .

install-dev:
	uv run pip install -e .

# Testing
test: test-pytest

test-pytest:
	uv run pytest tests/ -v --ignore=tests/test_demos.py --ignore=tests/translation

test-unit:
	uv run pytest -m "unit" tests/ -v

test-translation:
	uv run multigen batch --continue-on-error --source-dir tests/translation

test-build:
	uv run multigen batch --build --continue-on-error --source-dir tests/translation

test-integration:
	uv run pytest -m "integration" tests/ -v

test-py2c:
	uv run pytest -m "py2c" tests/ -v

test-benchmark:
	uv run pytest -m "benchmark" tests/ -v
	uv run python tests/benchmarks.py

test-memory-llvm:
	@echo "Running LLVM memory leak tests with AddressSanitizer..."
	@chmod +x scripts/test_llvm_memory.sh
	@./scripts/test_llvm_memory.sh

test-coverage:
	uv run pytest --cov=src/multigen --cov-branch --cov-report=html --cov-report=term-missing --ignore=tests/test_demos.py --ignore=tests/translation tests/
	@echo ""
	@echo "Coverage report generated in htmlcov/index.html"

# quickcheck

check: test type-check benchmark

snap:
	@git add --all . && git commit -m 'snap' && git push

# Code quality
lint:
	uv run ruff check src tests

format:
	uv run ruff check --fix src tests
	uv run ruff format src tests
	uv run isort --profile=black --line-length=120 src tests

format-check:
	uv run ruff check src tests
	uv run ruff format --check src tests

type-check:
	uv run mypy src/multigen

pre-commit:
	pre-commit install
	pre-commit run --all-files

# Build and distribution
build: clean
	uv build

clean:
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info/
	rm -rf .pytest_cache/
	rm -rf .coverage
	rm -rf htmlcov/
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete

# PyPI publishing
check-wheel: build
	@echo "Checking wheel with twine..."
	uv run twine check dist/*

publish-test: check-wheel
	@echo "Publishing to TestPyPI..."
	uv run twine upload --repository testpypi dist/*
	@echo ""
	@echo "Package published to TestPyPI"
	@echo "Install with: pip install --index-url https://test.pypi.org/simple/ multigen"

publish: check-wheel
	@echo "Publishing to PyPI..."
	uv run twine upload dist/*
	@echo ""
	@echo "Package published to PyPI"
	@echo "Install with: pip install multigen"

# Documentation
docs:
	@echo "Building Sphinx documentation..."
	cd docs/sphinx && $(MAKE) html
	@echo ""
	@echo "Documentation built successfully!"
	@echo "Open docs/sphinx/build/index.html in your browser"

docs-clean:
	cd docs/sphinx && $(MAKE) clean

docs-serve:
	@echo "Opening documentation in browser..."
	@open docs/sphinx/build/index.html || xdg-open docs/sphinx/build/index.html 2>/dev/null || echo "Please open docs/sphinx/build/index.html manually"

# Development utilities
run-examples:
	@echo "Running example scripts..."
	uv run python examples/hello_world.py
	uv run python examples/variables.py

# CI simulation
ci-test: install-dev lint format-check type-check test

# Performance monitoring
perf-monitor:
	uv run python scripts/run_tests.py --category benchmark --verbose

# Package verification
verify-package: build
	uv run python -m twine check dist/*
	uv run pip install dist/*.whl
	uv run python -c "import multigen; print(f'MultiGen version: {multigen.__version__}')"

# Development server (for future web interface)
dev-server:
	@echo "Development server not yet implemented"
	@echo "Planned: Web interface for code generation and testing"

# Benchmarking
benchmark:
	@echo "Running all benchmarks across all backends..."
	@mkdir -p $(BENCHMARK_RESULTS_DIR)
	uv run python scripts/benchmark.py --category all --output $(BENCHMARK_RESULTS_DIR)
	@echo ""
	@echo "Generating report..."
	uv run python scripts/generate_benchmark_report.py $(BENCHMARK_RESULTS_DIR)/benchmark_results.json --output $(BENCHMARK_RESULTS_DIR)/benchmark_report.md
	@echo ""
	@echo "Results saved to: $(BENCHMARK_RESULTS_DIR)/"
	@echo "View report: $(BENCHMARK_RESULTS_DIR)/benchmark_report.md"

benchmark-algorithms:
	@echo "Running algorithm benchmarks..."
	uv run python scripts/benchmark.py --category algorithms --output $(BENCHMARK_RESULTS_DIR)
	uv run python scripts/generate_benchmark_report.py $(BENCHMARK_RESULTS_DIR)/benchmark_results.json --output $(BENCHMARK_RESULTS_DIR)/benchmark_report.md

benchmark-data-structures:
	@echo "Running data structure benchmarks..."
	uv run python scripts/benchmark.py --category data_structures --output $(BENCHMARK_RESULTS_DIR)
	uv run python scripts/generate_benchmark_report.py $(BENCHMARK_RESULTS_DIR)/benchmark_results.json --output $(BENCHMARK_RESULTS_DIR)/benchmark_report.md

benchmark-report:
	@echo "Generating benchmark report..."
	uv run python scripts/generate_benchmark_report.py $(BENCHMARK_RESULTS_DIR)/benchmark_results.json --output $(BENCHMARK_RESULTS_DIR)/benchmark_report.md
	@echo "Report saved to: $(BENCHMARK_RESULTS_DIR)/benchmark_report.md"

benchmark-clean:
	@echo "Cleaning benchmark results..."
	rm -rf $(BENCHMARK_RESULTS_DIR)
	@echo "Benchmark results cleaned"
