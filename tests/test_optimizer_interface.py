"""Tests for AbstractOptimizer interface and implementations."""

import pytest

from multigen.backends.optimizer import AbstractOptimizer, NoOpOptimizer, OptimizationInfo


class TestOptimizationInfo:
    """Tests for OptimizationInfo dataclass."""

    def test_basic_construction(self) -> None:
        """Test basic OptimizationInfo construction."""
        info = OptimizationInfo(level=2, level_name="O2")
        assert info.level == 2
        assert info.level_name == "O2"
        assert info.passes_applied == []
        assert info.transformations == []
        assert info.metrics == {}

    def test_construction_with_all_fields(self) -> None:
        """Test OptimizationInfo with all fields provided."""
        info = OptimizationInfo(
            level=3,
            level_name="O3",
            passes_applied=["dce", "inlining"],
            transformations=["dead_code_elimination"],
            metrics={"target_triple": "x86_64-apple-darwin"},
        )
        assert info.level == 3
        assert info.level_name == "O3"
        assert info.passes_applied == ["dce", "inlining"]
        assert info.transformations == ["dead_code_elimination"]
        assert info.metrics["target_triple"] == "x86_64-apple-darwin"

    def test_default_list_independence(self) -> None:
        """Test that default lists are independent between instances."""
        info1 = OptimizationInfo(level=1, level_name="O1")
        info2 = OptimizationInfo(level=2, level_name="O2")
        info1.passes_applied.append("test")
        assert "test" not in info2.passes_applied


class TestNoOpOptimizer:
    """Tests for NoOpOptimizer."""

    def test_optimize_returns_unchanged_code(self) -> None:
        """Test that optimize returns code unchanged."""
        optimizer = NoOpOptimizer()
        code = "int main() { return 0; }"
        result = optimizer.optimize(code)
        assert result == code

    def test_optimize_with_opt_level(self) -> None:
        """Test that opt_level is ignored."""
        optimizer = NoOpOptimizer()
        code = "test code"
        # Any opt_level should return the same code
        assert optimizer.optimize(code, opt_level=0) == code
        assert optimizer.optimize(code, opt_level=3) == code

    def test_get_optimization_info(self) -> None:
        """Test get_optimization_info returns correct info."""
        optimizer = NoOpOptimizer()
        info = optimizer.get_optimization_info()
        assert isinstance(info, OptimizationInfo)
        assert info.level == 0
        assert info.level_name == "O0"
        assert info.passes_applied == []
        assert info.transformations == ["none"]

    def test_supports_level(self) -> None:
        """Test supports_level returns (0, 0)."""
        optimizer = NoOpOptimizer()
        assert optimizer.supports_level == (0, 0)


class TestAbstractOptimizerInterface:
    """Tests for AbstractOptimizer interface compliance."""

    def test_noop_is_abstract_optimizer(self) -> None:
        """Test that NoOpOptimizer is an AbstractOptimizer."""
        optimizer = NoOpOptimizer()
        assert isinstance(optimizer, AbstractOptimizer)

    def test_abstract_methods_exist(self) -> None:
        """Test that AbstractOptimizer defines required abstract methods."""
        # Check that abstract methods are defined
        assert hasattr(AbstractOptimizer, "optimize")
        assert hasattr(AbstractOptimizer, "get_optimization_info")
        assert hasattr(AbstractOptimizer, "supports_level")


# Skip LLVM tests if llvmlite not available
try:
    from llvmlite import binding as llvm  # type: ignore[import-not-found]

    LLVMLITE_AVAILABLE = True
except ImportError:
    LLVMLITE_AVAILABLE = False


@pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")
class TestLLVMOptimizerInterface:
    """Tests for LLVMOptimizer AbstractOptimizer implementation."""

    def test_llvm_optimizer_is_abstract_optimizer(self) -> None:
        """Test that LLVMOptimizer implements AbstractOptimizer."""
        from multigen.backends.llvm.optimizer import LLVMOptimizer

        optimizer = LLVMOptimizer(opt_level=2)
        assert isinstance(optimizer, AbstractOptimizer)

    def test_llvm_optimizer_get_optimization_info(self) -> None:
        """Test LLVMOptimizer.get_optimization_info returns OptimizationInfo."""
        from multigen.backends.llvm.optimizer import LLVMOptimizer

        optimizer = LLVMOptimizer(opt_level=2)
        info = optimizer.get_optimization_info()
        assert isinstance(info, OptimizationInfo)
        assert info.level == 2
        assert info.level_name == "O2"
        assert len(info.passes_applied) > 0
        assert "target_triple" in info.metrics

    def test_llvm_optimizer_supports_level(self) -> None:
        """Test LLVMOptimizer.supports_level returns (0, 3)."""
        from multigen.backends.llvm.optimizer import LLVMOptimizer

        optimizer = LLVMOptimizer(opt_level=2)
        assert optimizer.supports_level == (0, 3)

    def test_llvm_optimizer_info_varies_by_level(self) -> None:
        """Test that optimization info varies by level."""
        from multigen.backends.llvm.optimizer import LLVMOptimizer

        opt0 = LLVMOptimizer(opt_level=0)
        opt3 = LLVMOptimizer(opt_level=3)

        info0 = opt0.get_optimization_info()
        info3 = opt3.get_optimization_info()

        assert info0.level == 0
        assert info3.level == 3
        assert len(info3.passes_applied) > len(info0.passes_applied)


@pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")
class TestLLVMBackendGetOptimizer:
    """Tests for LLVMBackend.get_optimizer() method."""

    def test_llvm_backend_has_get_optimizer(self) -> None:
        """Test that LLVMBackend has get_optimizer method."""
        from multigen.backends.llvm.backend import LLVMBackend

        backend = LLVMBackend()
        assert hasattr(backend, "get_optimizer")

    def test_llvm_backend_get_optimizer_returns_optimizer(self) -> None:
        """Test that get_optimizer returns an AbstractOptimizer."""
        from multigen.backends.llvm.backend import LLVMBackend

        backend = LLVMBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)

    def test_llvm_backend_get_optimizer_caches(self) -> None:
        """Test that get_optimizer returns the same instance."""
        from multigen.backends.llvm.backend import LLVMBackend

        backend = LLVMBackend()
        opt1 = backend.get_optimizer()
        opt2 = backend.get_optimizer()
        assert opt1 is opt2


class TestOtherBackendsGetOptimizer:
    """Tests for get_optimizer on other backends (all return NoOpOptimizer)."""

    def test_cpp_backend_get_optimizer_returns_noop(self) -> None:
        """Test that C++ backend returns NoOpOptimizer."""
        from multigen.backends.cpp.backend import CppBackend

        backend = CppBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_rust_backend_get_optimizer_returns_noop(self) -> None:
        """Test that Rust backend returns NoOpOptimizer."""
        from multigen.backends.rust.backend import RustBackend

        backend = RustBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_go_backend_get_optimizer_returns_noop(self) -> None:
        """Test that Go backend returns NoOpOptimizer."""
        from multigen.backends.go.backend import GoBackend

        backend = GoBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_c_backend_get_optimizer_returns_noop(self) -> None:
        """Test that C backend returns NoOpOptimizer."""
        from multigen.backends.c.backend import CBackend

        backend = CBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_haskell_backend_get_optimizer_returns_noop(self) -> None:
        """Test that Haskell backend returns NoOpOptimizer."""
        from multigen.backends.haskell.backend import HaskellBackend

        backend = HaskellBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_ocaml_backend_get_optimizer_returns_noop(self) -> None:
        """Test that OCaml backend returns NoOpOptimizer."""
        from multigen.backends.ocaml.backend import OCamlBackend

        backend = OCamlBackend()
        optimizer = backend.get_optimizer()
        assert isinstance(optimizer, AbstractOptimizer)
        assert isinstance(optimizer, NoOpOptimizer)

    def test_all_backends_optimizer_caches(self) -> None:
        """Test that all backends cache their optimizer instance."""
        from multigen.backends.c.backend import CBackend
        from multigen.backends.cpp.backend import CppBackend
        from multigen.backends.go.backend import GoBackend
        from multigen.backends.haskell.backend import HaskellBackend
        from multigen.backends.ocaml.backend import OCamlBackend
        from multigen.backends.rust.backend import RustBackend

        backends = [CBackend(), CppBackend(), RustBackend(), GoBackend(), HaskellBackend(), OCamlBackend()]
        for backend in backends:
            opt1 = backend.get_optimizer()
            opt2 = backend.get_optimizer()
            assert opt1 is opt2, f"{backend.get_name()} should cache optimizer"
