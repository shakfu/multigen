"""C runtime vectors must not write through a failed allocation.

Each case compiles the runtime with malloc/realloc replaced by a switchable
failing allocator (via ``-include``), then runs push after init, grow, and
capacity-overflow failures.
"""

import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.backends.c.template_substitution import TemplateSubstitutionEngine

RUNTIME = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime"

pytestmark = pytest.mark.skipif(shutil.which("gcc") is None, reason="gcc not available")

INJECT = """\
#include <stdlib.h>
extern int fail_alloc;
extern int alloc_calls;
static inline void* test_malloc(size_t n) { alloc_calls++; return fail_alloc ? NULL : malloc(n); }
static inline void* test_realloc(void* p, size_t n) { alloc_calls++; return fail_alloc ? NULL : realloc(p, n); }
#define malloc test_malloc
#define realloc test_realloc
"""

# Exit codes identify the failing check.
DRIVER = """\
#include <stdint.h>
#include <stdio.h>
#include "multigen_vec_int.h"
int fail_alloc = 0;
int alloc_calls = 0;

int main(void) {
    /* 1. Initial allocation fails: push must not dereference NULL. */
    fail_alloc = 1;
    vec_int a = vec_int_init();
    if (a.data != NULL || a.capacity != 0) return 10;
    vec_int_push(&a, 1);
    if (a.size != 0) return 11;

    /* 2. Growth fails: push must not write past capacity. */
    fail_alloc = 0;
    vec_int b = vec_int_init();
    size_t cap = b.capacity;
    for (size_t i = 0; i < cap; i++) vec_int_push(&b, (int)i);
    fail_alloc = 1;
    vec_int_push(&b, 99);
    if (b.size != cap || b.capacity != cap) return 20;
    fail_alloc = 0;
    vec_int_push(&b, 99);
    if (b.size != cap + 1 || b.data[cap] != 99) return 21;

    /* 3. Capacity doubling would overflow size_t: no realloc, no write. */
    int dummy = 0;
    vec_int c;
    c.data = &dummy;
    c.capacity = SIZE_MAX / 2;
    c.size = c.capacity;
    alloc_calls = 0;
    vec_int_push(&c, 5);
    if (c.size != SIZE_MAX / 2 || alloc_calls != 0 || dummy != 0) return 30;

    /* 4. Reserve that overflows the byte count must not allocate. */
    vec_int d = vec_int_init();
    alloc_calls = 0;
    vec_int_reserve(&d, SIZE_MAX / 2);
    if (alloc_calls != 0) return 40;

    printf("ok\\n");
    return 0;
}
"""


def _compile_and_run(tmp_path: Path, sources: list[Path], include_dirs: list[Path]) -> subprocess.CompletedProcess[str]:
    inject = tmp_path / "inject.h"
    inject.write_text(INJECT)
    driver = tmp_path / "driver.c"
    driver.write_text(DRIVER)
    exe = tmp_path / "driver"
    cmd = ["gcc", "-std=c11", "-D_POSIX_C_SOURCE=200809L", "-Wall", "-include", str(inject)]
    cmd += [f"-I{d}" for d in include_dirs]
    cmd += [str(driver), *map(str, sources), str(RUNTIME / "multigen_error_handling.c"), "-o", str(exe)]
    build = subprocess.run(cmd, capture_output=True, text=True)
    assert build.returncode == 0, build.stderr
    return subprocess.run([str(exe)], capture_output=True, text=True, timeout=10)


def test_static_header_vec_int(tmp_path: Path) -> None:
    run = _compile_and_run(tmp_path, [], [RUNTIME])
    assert run.returncode == 0, f"check {run.returncode} failed"
    assert run.stdout.strip() == "ok"


def test_template_vec_int(tmp_path: Path) -> None:
    engine = TemplateSubstitutionEngine()
    templates = RUNTIME / "templates"
    gen = tmp_path / "gen"
    gen.mkdir()
    (gen / "multigen_vec_int.h").write_text(
        engine.substitute_vec_template((templates / "vec_T.h.tmpl").read_text(), "int")
    )
    impl = gen / "multigen_vec_int.c"
    impl.write_text(engine.substitute_vec_template((templates / "vec_T.c.tmpl").read_text(), "int"))

    # gen/ precedes the runtime dir so its multigen_vec_int.h shadows the static header.
    run = _compile_and_run(tmp_path, [impl], [gen, RUNTIME])
    assert run.returncode == 0, f"check {run.returncode} failed"
    assert run.stdout.strip() == "ok"
