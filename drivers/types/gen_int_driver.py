from sys import argv

template = """
printer "ocaml"

module uint{width}.UInt{width}

  syntax type uint{width}  "int"
  syntax literal uint{width} "%1"

  syntax val (+)     "%1 + %2"
  syntax val (-)     "%1 - %2"
  syntax val (-_)    "-%1"
  syntax val ( * )   "%1 * %2"
  syntax val (/)     "%1 / %2"
  syntax val (%)     "%1 % %2"
  syntax val (=)     "%1 == %2"
  syntax val (<=)    "%1 <= %2"
  syntax val (<)     "%1 < %2"
  syntax val (>=)    "%1 >= %2"
  syntax val (>)     "%1 > %2"

  syntax val of_int "%1"
  syntax val to_int "%1"

end
"""

assert len(argv) == 2, "using `python3 gen_int_driver.py <width>` to generate a driver"
width = argv[1]
content = template.format(width=width)
print(content)
