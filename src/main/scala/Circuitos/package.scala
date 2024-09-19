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
        val halfAdder = half_adder
        val logicOr = crearChipBinario((x: Int, y: Int) => x + y - (x * y))
        val firstHaResult = halfAdder(List(list.tail.head, list.tail.tail.head))
        val secondHaResult = halfAdder(List(list.head, firstHaResult.tail.head))
        val cout = logicOr(List(secondHaResult.head, firstHaResult.head))
        List(cout.head, secondHaResult.tail.head)
    }

  def adder(n: Int): Chip = {
    // Devuelve un Chip que recibe una lista de 2n bits de entrada (los primeros n bits corresponden
    // al primer número a sumar y los segundos n bits corresponden al segundo número a sumar)
    // y devuelve n+1 bits de salida, donde el primero es el acarreo de la suma y los otros n
    // corresponden a los n bits de salida
    def aux(bitsNumero1: List[Int], bitsNumero2: List[Int], digitoActual: Int, acarreo: Int,
            resultado: List[Int]): List[Int] = {
      if (digitoActual == -1)
        acarreo :: resultado
      else {
        val resultadoSuma = full_adder(List(bitsNumero1(digitoActual), bitsNumero2(digitoActual), acarreo))
        val nuevoAcarreo = resultadoSuma.head
        val suma = resultadoSuma.tail.head
        aux(bitsNumero1, bitsNumero2, digitoActual - 1, nuevoAcarreo, suma :: resultado)
      }
    }

    (lista: List[Int]) => {
      val numero1 = lista.take(n)
      val numero2 = lista.drop(n)
      aux(numero1, numero2, n - 1, 0, Nil)
    }
  }
}
