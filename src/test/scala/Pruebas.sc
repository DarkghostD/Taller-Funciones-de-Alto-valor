import Circuitos._

val chip_not = crearChipUnario((x: Int) => (1 - x))
chip_not(List(1))
chip_not(List(0))

val chip_and = crearChipBinario((x: Int, y: Int) => (x * y))
chip_and(List(1, 1))
chip_and(List(1, 0))

val chip_or = crearChipBinario((x: Int, y: Int) => x + y - (x * y))
chip_or(List(1, 1))
chip_or(List(1, 0))

half_adder(List(0, 0))
half_adder(List(0, 1))
half_adder(List(1, 0))
half_adder(List(1, 1))

full_adder(List(1, 1, 0))
full_adder(List(1, 1, 1))
full_adder(List(0, 1, 0))
full_adder(List(0, 1, 1))

adder(1)(List(1) ++ List(1))
adder(1)(List(0) ++ List(0))
adder(1)(List(1) ++ List(0))
adder(1)(List(0) ++ List(1))

adder(2)(List(1, 0) ++ List(0, 1))
adder(2)(List(1, 1) ++ List(0, 1))

adder(3)(List(1, 0, 1) ++ List(0, 0, 0))
adder(3)(List(1, 0, 1) ++ List(1, 0, 1))

val ad4 = adder(4)
ad4(List(1, 0, 1, 1) ++ List(1, 0, 1, 0))
