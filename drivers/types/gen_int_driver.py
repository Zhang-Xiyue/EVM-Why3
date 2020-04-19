from sys import argv

template = """
printer "ocaml"

module uint{width}.UInt{width}

  syntax type uint{width}  "Z.t"
  syntax literal uint{width} "(Z.of_int %1)"

  syntax val (+)     "Z.add %1 %2"
  syntax val (-)     "Z.sub %1 %2"
  syntax val (-_)    "Z.negate %1"
  syntax val ( * )   "Z.mul %1 %2"
  syntax val (/)     "Z.div %1 %2"
  syntax val (%)     "Z.zmod %1 %2"
  syntax val (=)     "Z.eq %1 %2"
  syntax val (<=)    "Z.le %1 %2"
  syntax val (<)     "Z.lt %1 %2"
  syntax val (>=)    "Z.ge %1 %2"
  syntax val (>)     "Z.gt %1 %2"

  syntax val of_int "%1"
  syntax val to_int "%1"

end
"""

assert len(argv) == 2, "using `python3 gen_int_driver.py <width>` to generate a driver"
width = argv[1]
content = template.format(width=width)
print(content)
