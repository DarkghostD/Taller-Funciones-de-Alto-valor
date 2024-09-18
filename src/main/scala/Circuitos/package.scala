package object Circuitos {
    type Chip = List[Int] => List[Int]
    def crearChipBinario (funcion: (Int, Int) => Int): Chip = {
      // Dada la función que calcula una operación lógica binaria,
      // devuelve la representación del Chip respectivo
      (chipList: List[Int]) => List(funcion(chipList.head, chipList.tail.head))
    }
    def crearChipUnario (funcion: Int => Int): Chip = {
      // Dada la función que calcula una operación lógica unaria (como la negación),
      // devuelve la representación del Chip respectivo
      (chipList: List[Int]) => List(funcion(chipList.head))
    }
    def half_adder: Chip = {
      // Devuelve el Chip correspondiente a un half adder
      (list: List[Int]) =>
        if (list.head == 1 && list.head == list.tail.head) List(list.head, 0)
        else List(0, list.head + list.tail.head)
    }
    def full_adder: Chip = {
      // Devuelve el Chip correspondiente a un full adder
      (list: List[Int]) =>
        val firstHaResult = half_adder(List(list(1), list(2)))
        val secondHaResult = half_adder(List(list.head, firstHaResult(1)))
        val logicOr = crearChipBinario((x: Int, y: Int) => x + y - (x * y))
        val cout = logicOr(List(secondHaResult.head, firstHaResult.head))
        List(cout.head, secondHaResult(1))
    }
    def adder (n: Int): Chip = {
      // Devuelve un Chip que recibe una lista de 2n bits de entrada (los primeros n bits corresponden
      // al primer número a sumar y los segundos n bits corresponden al segundo número a sumar)
      // y devuelve n+1 bits de salida, donde el primero es el acarreo de la suma y los otros n
      // corresponden a los n bits de salida
      def sumaBits(primerNumero: List[Int], segundoNumero: List[Int], carryIn: Int, acc: List[Int]): List[Int] = {
        if (primerNumero.isEmpty) carryIn :: acc
        else {
          val bit1 = primerNumero.head
          val bit2 = segundoNumero.head
          val sumaIntermedia = bit1 ^ bit2 ^ carryIn
          val nuevoAcarreo = (bit1 & bit2) | (carryIn & (bit1 ^ bit2))
          sumaBits(primerNumero.tail, segundoNumero.tail, nuevoAcarreo, sumaIntermedia :: acc)
        }
      }

      List => {
        val primerNumero = List.take(n)
        val segundoNumero = List.drop(n)
        sumaBits(primerNumero, segundoNumero, 0, Nil)
      }
    }
}
