package object Circuitos {
    type Chip = List[Int] => List[Int]
    def crearChipBinario (funcion: (Int, Int) => Int): Chip = {
      // Dada la función que calcula una operación lógica binaria,
      // devuelve la representación del Chip respectivo
      entrada => List(funcion(entrada.head, entrada(1)))
    }
    def crearChipUnario (funcion: Int => Int): Chip = {
      // Dada la función que calcula una operación lógica unaria (como la negación),
      // devuelve la representación del Chip respectivo
      entrada => List(funcion(entrada.head))
    }
    def half_adder: Chip = {
      // Devuelve el Chip correspondiente a un half adder
      entrada => {
        val a = entrada.head
        val b = entrada(1)
        val suma = a ^ b // XOR para la suma
        val acarreo = a & b // AND para el acarreo
        List(acarreo, suma)
      }
    }
    def full_adder: Chip = {
      // Devuelve el Chip correspondiente a un full adder
      entrada => {
        val a = entrada.head
        val b = entrada(1)
        val carryIn = entrada(2)
        val sumaIntermedia = a ^ b
        val suma = sumaIntermedia ^ carryIn
        val acarreo = (a & b) | (carryIn & sumaIntermedia)
        List(acarreo, suma)
      }
    }
    def adder (n: Int): Chip = {
      // Devuelve un Chip que recibe una lista de 2n bits de entrada (los primeros n bits corresponden
      // al primer número a sumar y los segundos n bits corresponden al segundo número a sumar)
      // y devuelve n+1 bits de salida, donde el primero es el acarreo de la suma y los otros n
      // corresponden a los n bits de salida
      entrada => {
        val primerNumero = entrada.take(n) // Primer número de n bits
        val segundoNumero = entrada.drop(n) // Segundo número de n bits
        val resultado = (primerNumero zip segundoNumero).foldRight((0, List[Int]())) {
          case ((bit1, bit2), (carryIn, acc)) =>
            val sumaIntermedia = bit1 ^ bit2 ^ carryIn
            val acarreo = (bit1 & bit2) | (carryIn & (bit1 ^ bit2))
            (acarreo, sumaIntermedia :: acc)
        }
        List(resultado._1) ++ resultado._2 // Acarreo final seguido de la suma
      }
    }
}
