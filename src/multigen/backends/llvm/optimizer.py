"""LLVM optimization pass manager.

This module provides infrastructure for applying LLVM optimization passes
to generated IR before compilation. It uses llvmlite's new pass manager API
to configure and run optimization pipelines.

Optimization Levels:
    - O0: No optimization (debugging)
    - O1: Basic optimizations (fast compile, moderate performance)
    - O2: Standard optimizations (balanced, default for production)
    - O3: Aggressive optimizations (maximum performance)

Example:
    >>> from multigen.backends.llvm.optimizer import LLVMOptimizer
    >>> optimizer = LLVMOptimizer(opt_level=2)
    >>> optimized_ir = optimizer.optimize(original_ir)
"""

from typing import Any

from llvmlite import binding as llvm  # type: ignore[import-untyped]


class LLVMOptimizer:
    """Manages LLVM optimization passes for IR optimization.

    This class configures and applies LLVM optimization passes based on
    the requested optimization level. It uses the new pass manager API
    introduced in LLVM 13+.

    Attributes:
        opt_level: Optimization level (0-3)
        target_machine: LLVM target machine for platform-specific opts
    """

    def __init__(self, opt_level: int = 2) -> None:
        """Initialize the LLVM optimizer.

        Args:
            opt_level: Optimization level (0=none, 1=basic, 2=moderate, 3=aggressive)
        """
        if not 0 <= opt_level <= 3:
            raise ValueError(f"Optimization level must be 0-3, got {opt_level}")

        self.opt_level = opt_level

        # Initialize LLVM native target
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        # Create target machine for native platform
        target = llvm.Target.from_default_triple()
        self.target_machine = target.create_target_machine(opt=opt_level)

    def optimize(self, llvm_ir: str) -> str:
        """Apply optimization passes to LLVM IR.

        This method parses the IR, configures optimization passes based on
        the optimization level, runs the passes, and returns optimized IR.

        Args:
            llvm_ir: LLVM IR as string

        Returns:
            Optimized LLVM IR as string

        Raises:
            ValueError: If IR is invalid or cannot be parsed
            RuntimeError: If optimization passes fail
        """
        # Parse and verify IR
        try:
            llvm_module = llvm.parse_assembly(llvm_ir)
            llvm_module.verify()
        except Exception as e:
            raise ValueError(f"Failed to parse LLVM IR: {e}") from e

        # Skip optimization for O0
        if self.opt_level == 0:
            return str(llvm_module)

        # Create pipeline tuning options
        pto = llvm.create_pipeline_tuning_options()
        self._configure_pipeline_options(pto)

        # Create pass builder with target machine
        pb = llvm.create_pass_builder(self.target_machine, pto)

        # Get module pass manager
        mpm = pb.getModulePassManager()

        # Configure optimization passes
        self._configure_passes(mpm)

        # Run optimization passes
        try:
            mpm.run(llvm_module, pb)
        except Exception as e:
            raise RuntimeError(f"Optimization passes failed: {e}") from e

        # Return optimized IR
        return str(llvm_module)

    def _configure_pipeline_options(self, pto: llvm.PipelineTuningOptions) -> None:
        """Configure pipeline tuning options based on optimization level.

        Args:
            pto: Pipeline tuning options object
        """
        # Set speed level (0=O0, 1=O1, 2=O2, 3=O3)
        pto.speed_level = self.opt_level

        # Set size level (0=default, 1=size, 2=aggressive size)
        pto.size_level = 0

        # Enable vectorization for O2+
        pto.loop_vectorization = self.opt_level >= 2
        pto.slp_vectorization = self.opt_level >= 2  # Superword-level parallelism

        # Enable loop optimizations for O2+
        pto.loop_unrolling = self.opt_level >= 2
        pto.loop_interleaving = self.opt_level >= 2

        # Set inlining threshold based on optimization level
        pto.inlining_threshold = self._get_inlining_threshold()

    def _get_inlining_threshold(self) -> int:
        """Get function inlining threshold for current optimization level.

        Higher thresholds allow more aggressive inlining, which can improve
        performance but may increase code size.

        Returns:
            Inlining threshold (instruction cost limit)
        """
        thresholds = {
            0: 0,     # No inlining
            1: 75,    # Conservative inlining
            2: 225,   # Default LLVM threshold
            3: 275,   # Aggressive inlining
        }
        return thresholds.get(self.opt_level, 225)

    def _configure_passes(self, mpm: llvm.ModulePassManager) -> None:
        """Configure optimization passes for the current optimization level.

        This method adds optimization passes incrementally based on the
        optimization level, following LLVM's standard optimization pipeline.

        Args:
            mpm: Module pass manager to configure
        """
        # O1+: Basic optimizations
        if self.opt_level >= 1:
            self._add_basic_optimizations(mpm)

        # O2+: Standard optimizations
        if self.opt_level >= 2:
            self._add_standard_optimizations(mpm)

        # O3: Aggressive optimizations
        if self.opt_level >= 3:
            self._add_aggressive_optimizations(mpm)

        # Always verify IR correctness at the end
        mpm.add_verifier()

    def _add_basic_optimizations(self, mpm: llvm.ModulePassManager) -> None:
        """Add basic O1-level optimizations.

        These passes provide good performance improvement with minimal
        compilation time overhead. Focus on simple cleanups and obvious wins.

        Args:
            mpm: Module pass manager
        """
        # Dead code elimination
        mpm.add_dead_arg_elimination_pass()      # Remove unused function args
        mpm.add_dead_code_elimination_pass()     # Remove unreachable code

        # Global optimizations
        mpm.add_global_opt_pass()                # Optimize global variables

        # Interprocedural optimizations
        mpm.add_ipsccp_pass()                    # Interprocedural constant propagation

        # Control flow and instruction optimizations
        mpm.add_simplify_cfg_pass()              # Simplify control flow graph
        mpm.add_instruction_combine_pass()       # Combine redundant instructions

    def _add_standard_optimizations(self, mpm: llvm.ModulePassManager) -> None:
        """Add standard O2-level optimizations.

        These passes provide the best balance of compile time and runtime
        performance. This is the recommended level for production builds.

        Args:
            mpm: Module pass manager
        """
        # Function inlining
        mpm.add_always_inliner_pass()            # Inline functions marked "alwaysinline"

        # Global optimizations
        mpm.add_global_dead_code_eliminate_pass()  # Global dead code elimination

        # Algebraic optimizations
        mpm.add_reassociate_pass()               # Reassociate expressions for optimization

        # Constant propagation
        mpm.add_sccp_pass()                      # Sparse conditional constant propagation

        # Memory optimizations
        mpm.add_sroa_pass()                      # Scalar replacement of aggregates
        mpm.add_mem_copy_opt_pass()              # Optimize memcpy/memmove
        mpm.add_dead_store_elimination_pass()    # Remove dead stores

        # Control flow optimizations
        mpm.add_tail_call_elimination_pass()     # Eliminate tail calls

        # Loop optimizations
        mpm.add_loop_rotate_pass()               # Rotate loops into do-while form
        mpm.add_loop_simplify_pass()             # Canonicalize loop structure

    def _add_aggressive_optimizations(self, mpm: llvm.ModulePassManager) -> None:
        """Add aggressive O3-level optimizations.

        These passes maximize runtime performance at the cost of longer
        compilation times and potentially larger code size.

        Args:
            mpm: Module pass manager
        """
        # Aggressive dead code elimination
        mpm.add_aggressive_dce_pass()            # More aggressive DCE

        # Aggressive instruction combining
        mpm.add_aggressive_instcombine_pass()    # More aggressive instruction combining

        # Loop optimizations
        mpm.add_loop_unroll_pass()               # Unroll loops
        mpm.add_loop_unroll_and_jam_pass()       # Unroll and jam nested loops
        mpm.add_loop_strength_reduce_pass()      # Loop strength reduction

        # Interprocedural optimizations
        mpm.add_argument_promotion_pass()        # Promote pointer args to values

        # Function merging
        mpm.add_merge_functions_pass()           # Merge identical functions

    def get_optimization_info(self) -> dict[str, Any]:
        """Get information about the current optimization configuration.

        Returns:
            Dictionary with optimization settings and metadata
        """
        return {
            "opt_level": self.opt_level,
            "opt_name": {0: "O0", 1: "O1", 2: "O2", 3: "O3"}[self.opt_level],
            "inlining_threshold": self._get_inlining_threshold(),
            "vectorization_enabled": self.opt_level >= 2,
            "loop_unrolling_enabled": self.opt_level >= 2,
            "target_triple": self.target_machine.triple,
        }
