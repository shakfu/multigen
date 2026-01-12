Quick Start Guide
=================

This guide walks you through your first MultiGen conversion.

Basic Conversion
----------------

Create a simple Python file ``fibonacci.py``::

   def fibonacci(n: int) -> int:
       if n <= 1:
           return n
       return fibonacci(n - 1) + fibonacci(n - 2)

   def main() -> None:
       result: int = fibonacci(10)
       print(f"Fibonacci(10) = {result}")

   if __name__ == "__main__":
       main()

Convert to C::

   multigen convert -t c fibonacci.py

This generates ``fibonacci.c`` with a self-contained C implementation.

Building and Running
--------------------

Compile with any C compiler::

   gcc -o fibonacci fibonacci.c
   ./fibonacci

Or let MultiGen generate a Makefile::

   multigen build -t c fibonacci.py -m
   make
   ./fibonacci

Supported Targets
-----------------

Convert to different languages::

   # C++
   multigen convert -t cpp fibonacci.py

   # Rust
   multigen convert -t rust fibonacci.py

   # Go
   multigen convert -t go fibonacci.py

   # Haskell
   multigen convert -t haskell fibonacci.py

   # OCaml
   multigen convert -t ocaml fibonacci.py

Advanced Example
----------------

Create ``wordcount.py`` with more complex features::

   from pathlib import Path

   def count_words(text: str) -> dict[str, int]:
       counts: dict[str, int] = {}
       words: list[str] = text.lower().split()

       for word in words:
           if word in counts:
               counts[word] += 1
           else:
               counts[word] = 1

       return counts

   def main() -> None:
       path: Path = Path("input.txt")
       text: str = path.read_text()
       counts: dict[str, int] = count_words(text)

       for word, count in counts.items():
           print(f"{word}: {count}")

   if __name__ == "__main__":
       main()

Convert to Rust::

   multigen convert -t rust wordcount.py

This demonstrates:

- File I/O (Path operations)
- Dictionaries with type inference
- String operations
- For-each loops

Next Steps
----------

- :doc:`backends` - Learn about backend-specific features
- :doc:`/api/pipeline` - API reference for programmatic use
