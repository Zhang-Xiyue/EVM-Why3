template = """
printer "ocaml"

module mach.int.Int_WIDTH_

  syntax type int_WIDTH_  "int"
  syntax literal int_WIDTH_ "%1"

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

for i in range(8, 256 + 8, 8):
  content = template.replace("_WIDTH_", str(i))
  with open("int%d.drv" % i, "w") as f:
      f.write(content)
