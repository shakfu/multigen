#!/usr/bin/env python3
"""Generate Markdown report from benchmark JSON results."""

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path


def format_time(seconds: float) -> str:
    """Format time in human-readable format."""
    if seconds < 0.001:
        return f"{seconds * 1000000:.1f}µs"
    elif seconds < 1:
        return f"{seconds * 1000:.1f}ms"
    else:
        return f"{seconds:.3f}s"


def format_size(bytes_val: int | float) -> str:
    """Format size in human-readable format."""
    if bytes_val < 1024:
        return f"{int(bytes_val)}B"
    elif bytes_val < 1024 * 1024:
        return f"{bytes_val / 1024:.1f}KB"
    else:
        return f"{bytes_val / (1024 * 1024):.1f}MB"


def generate_markdown_report(json_file: Path, output_file: Path) -> None:
    """Generate Markdown report from JSON results."""
    with open(json_file, "r") as f:
        data = json.load(f)

    summary = data["summary"]
    results = data["results"]

    # Group results by benchmark
    by_benchmark: dict[str, list[dict]] = {}
    for result in results:
        name = result["name"]
        if name not in by_benchmark:
            by_benchmark[name] = []
        by_benchmark[name].append(result)

    # Generate report
    lines = [
        "# MultiGen Benchmark Results",
        "",
        f"**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
        "",
        "## Summary",
        "",
        f"- **Total Benchmarks**: {summary['total_benchmarks']}",
        f"- **Total Backends**: {summary['total_backends']}",
        f"- **Total Runs**: {len(results)}",
        f"- **Successful**: {summary['successful_runs']} ({summary['successful_runs'] * 100 // len(results)}%)",
        f"- **Failed**: {summary['failed_runs']}",
        "",
        "## Backend Comparison",
        "",
        "| Backend | Success Rate | Avg Compile Time | Avg Run Time | Avg Binary Size | Avg LOC |",
        "|---------|--------------|------------------|--------------|-----------------|---------|",
    ]

    for backend in sorted(summary["by_backend"].keys()):
        stats = summary["by_backend"][backend]
        success_rate = (
            f"{stats['successful']}/{stats['total']} ({stats['successful'] * 100 // stats['total']}%)"
        )
        compile_time = format_time(stats["avg_compilation_time"])
        run_time = format_time(stats["avg_execution_time"])
        binary_size = format_size(stats["avg_binary_size"])
        loc = f"{int(stats['avg_lines_of_code'])}"

        lines.append(
            f"| {backend} | {success_rate} | {compile_time} | {run_time} | {binary_size} | {loc} |"
        )

    lines.extend(["", "## Detailed Results", ""])

    # Results by benchmark
    for benchmark_name in sorted(by_benchmark.keys()):
        lines.extend([f"### {benchmark_name}", ""])

        # Create comparison table
        lines.extend(
            [
                "| Backend | Status | Compile Time | Run Time | Binary Size | LOC | Output |",
                "|---------|--------|--------------|----------|-------------|-----|--------|",
            ]
        )

        benchmark_results = by_benchmark[benchmark_name]
        for result in sorted(benchmark_results, key=lambda x: x["backend"]):
            status = "✅" if result["success"] else "❌"
            compile_time = format_time(result["compilation_time"])
            run_time = format_time(result["execution_time"]) if result["success"] else "-"
            binary_size = (
                format_size(result["binary_size"]) if result["binary_size"] > 0 else "-"
            )
            loc = str(result["lines_of_code"]) if result["lines_of_code"] > 0 else "-"
            output = result["output"][:20] if result["output"] else (result["error"][:20] if result["error"] else "-")

            lines.append(
                f"| {result['backend']} | {status} | {compile_time} | {run_time} | {binary_size} | {loc} | {output} |"
            )

        lines.append("")

    # Performance rankings
    lines.extend(["## Performance Rankings", "", "### Fastest Execution Time", ""])

    successful_results = [r for r in results if r["success"]]
    if successful_results:
        fastest = sorted(successful_results, key=lambda x: x["execution_time"])[:10]
        lines.extend(
            [
                "| Rank | Backend | Benchmark | Time |",
                "|------|---------|-----------|------|",
            ]
        )
        for i, result in enumerate(fastest, 1):
            lines.append(
                f"| {i} | {result['backend']} | {result['name']} | {format_time(result['execution_time'])} |"
            )

        lines.extend(["", "### Fastest Compilation Time", ""])
        fastest_compile = sorted(
            successful_results, key=lambda x: x["compilation_time"]
        )[:10]
        lines.extend(
            [
                "| Rank | Backend | Benchmark | Time |",
                "|------|---------|-----------|------|",
            ]
        )
        for i, result in enumerate(fastest_compile, 1):
            lines.append(
                f"| {i} | {result['backend']} | {result['name']} | {format_time(result['compilation_time'])} |"
            )

        lines.extend(["", "### Smallest Binary Size", ""])
        smallest_binary = sorted(
            [r for r in successful_results if r["binary_size"] > 0],
            key=lambda x: x["binary_size"],
        )[:10]
        if smallest_binary:
            lines.extend(
                [
                    "| Rank | Backend | Benchmark | Size |",
                    "|------|---------|-----------|------|",
                ]
            )
            for i, result in enumerate(smallest_binary, 1):
                lines.append(
                    f"| {i} | {result['backend']} | {result['name']} | {format_size(result['binary_size'])} |"
                )

        lines.extend(["", "### Most Compact Code (LOC)", ""])
        smallest_loc = sorted(
            [r for r in successful_results if r["lines_of_code"] > 0],
            key=lambda x: x["lines_of_code"],
        )[:10]
        if smallest_loc:
            lines.extend(
                [
                    "| Rank | Backend | Benchmark | LOC |",
                    "|------|---------|-----------|-----|",
                ]
            )
            for i, result in enumerate(smallest_loc, 1):
                lines.append(
                    f"| {i} | {result['backend']} | {result['name']} | {result['lines_of_code']} |"
                )

    # Failed runs
    failed_results = [r for r in results if not r["success"]]
    if failed_results:
        lines.extend(["", "## Failed Runs", ""])
        lines.extend(
            [
                "| Backend | Benchmark | Error |",
                "|---------|-----------|-------|",
            ]
        )
        for result in failed_results:
            error = result["error"][:50] if result["error"] else "Unknown error"
            lines.append(f"| {result['backend']} | {result['name']} | {error} |")

    lines.append("")

    # Write report
    with open(output_file, "w") as f:
        f.write("\n".join(lines))

    print(f"Markdown report generated: {output_file}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate Markdown report from benchmark results"
    )
    parser.add_argument(
        "json_file",
        type=str,
        help="Input JSON file with benchmark results",
    )
    parser.add_argument(
        "--output",
        type=str,
        help="Output Markdown file (default: benchmark_report.md)",
        default="benchmark_report.md",
    )

    args = parser.parse_args()

    json_file = Path(args.json_file)
    if not json_file.exists():
        print(f"Error: JSON file not found: {json_file}")
        return 1

    output_file = Path(args.output)
    generate_markdown_report(json_file, output_file)

    return 0


if __name__ == "__main__":
    sys.exit(main())
