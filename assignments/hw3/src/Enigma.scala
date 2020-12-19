package pp202002.hw3

/*
  Exercise 3: Enigma machine

  Implement Enigma machine described below.

  Enigma machine is encryption system used by Nazi Germany during World War II
  Alan Turing, the father of Computer Science, is famous for his development of Enigma-deciphering machine.

  Enigma machine encrypts an alphabet by the connections and arrangements of internal optical wires.
  The connection of the optical wire is composed of 3 'Rotors' and a single 'Reflector'.
  The overall encoding path is shown as below.
                  +++++++++++         +++++++++++         +++++++++++        ===
  input:  'A' --> |         | --'C'-> |         | --'D'-> |         | --'F'----+\
                  | Rotor 1 |         | Rotor 2 |         | Rotor 3 |          | |  Reflector
  output: 'B' <-- |         | <-'E'-- |         | <-'S'-- |         | <-'S'----+/
                  +++++++++++         +++++++++++         +++++++++++        ===
  Each Rotor and Reflector has internal optical 'Wire's. The wire maps each alphabet to another alphabet, e.g., 'A' to 'F'.
  At first, the input alphabet goes forward through the wires of three rotors, and 'reflected' by the reflector.
  Reflector maps an alphabet to another alphabet, and vice versa, e.g. 'A' to 'F', and 'F' to 'A'

  Because of the reflector, decoding path of Enigma is just reversal of the encoding path.
                  +++++++++++         +++++++++++         +++++++++++        ===
  output: 'A' <-- |         | <-'C'-- |         | <-'D'-- |         | <-'F'----+\
                  | Rotor 1 |         | Rotor 2 |         | Rotor 3 |          | |  Reflector
  input:  'B' --> |         | --'E'-> |         | --'S'-> |         | --'S'----+/
                  +++++++++++         +++++++++++         +++++++++++        ===

  Before the first input enters, the first rotor 'rotates' one tick, that means, the internal wire is entirely rotated by 1/26 round counterclockwise.
  The effect of rotations can be decomposed into two events, 1) rotating the whole connection, 2) and shifting left by one alphabet.
  If the first rotor has turned full circle, the seconds rotor rotates one tick, and so on.

  e.g.) Suppose that the first rotor initially maps 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to 'QWERTYUIOPASDFGHJKLZXCVBNM'
        When we type first letter, the first rotor maps 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to 'VDQSXTHNOZRCEFGIJKYWBUAMLP',
        so if we typed 'B', it would be encrypted into 'D'.
        (rotation of wire shifts the connections into 'WERTYUIOPASDFGHJKLZXCVBNMQ', then its output is shifted left,
         that makes the map to 'VDQSXTHNOZRCEFGIJKYWBUAMLP')

  Our Enigma machine uses 1 to 5 rotors and a single reflector.
  (We will not use a plugboard)

  EnigmaSettings contains a list of initial rotor settings and reflector settings.
  You should implement the backward path of wires, and the full encryption and decryption paths of Enigma machine.

  The input will be restricted to upper cased alphabets. ('A'-'Z')

  reference links:
  https://en.wikipedia.org/wiki/Enigma_rotor_details
  https://www.theguardian.com/technology/2014/nov/14/how-did-enigma-machine-work-imitation-game
  https://hackaday.com/2017/08/22/the-enigma-enigma-how-the-enigma-machine-worked/
  https://www.youtube.com/watch?v=QwQVMqfoB2E

  P.S.) As we ignore turnover notch positions and plugboard, our Enigma machine behaves 
        slightly differently than most of the web implementations of Enigma machine.
 */

/** The initial setting of Enigma machine
 *
 * We guarantee that the connection of reflectorState is involutive,
 * that means, forall x, reflectorState.forward(reflectorState.forward(x)) == x
 *
 * @param rotorState     The internal wire connections of each Rotor. The first object of the list should be the first rotor.
 * @param reflectorState The internal wire connection of Reflector.
 */
case class EnigmaSettings(rotorState: List[Wire], reflectorState: Wire)

object Enigma extends CipherGen[EnigmaSettings]
{
    def buildEncryptor(initSetting: EnigmaSettings): Enigma = {
        val rotor = Rotor(initSetting.rotorState, initSetting.rotorState)
        new Enigma(rotor.next(), initSetting.reflectorState, initSetting.rotorState)
    }

    def buildDecryptor(initSetting: EnigmaSettings): Enigma = {
        val rotor = Rotor(initSetting.rotorState, initSetting.rotorState)
        new Enigma(rotor.next(), initSetting.reflectorState, initSetting.rotorState)
    }
}

class Enigma(rotorState: List[Wire], reflectorState: Wire, initState: List[Wire]) extends Encryptor with Decryptor
{

    val rotor: Rotor = Rotor(rotorState, initState)
    val reflector: Reflector = Reflector(reflectorState)


    def encrypt(c: Char): (Char, Enigma) = {
        val res = rotor.backward(reflector.forward(rotor.forward(c)))
        (res, new Enigma(rotor.next(), reflectorState, initState))
    }

    // Decryption of Enigma machine is same to the Encryption
    def decrypt(c: Char): (Char, Enigma) = encrypt(c)
}

sealed abstract class EnigmaParts
{
    def forward(c: Char): Char

    def backward(c: Char): Char
}

case class Wire(connection: String) extends EnigmaParts
{
    def forward(c: Char): Char = connection(c - 'A')

    def backward(c: Char): Char = {
        def find(c: Char, n: Int): Int = {
            if (connection(n) == c) n
            else find(c, n + 1)
        }

        (find(c, 0) + 'A').toChar
    }
}

case class Rotor(wires: List[Wire], initState: List[Wire]) extends EnigmaParts
{
    def forward(c: Char): Char = {
        def forwardIter(c: Char, l: List[Wire]): Char = {
            l match {
                case hd :: tl => forwardIter(hd.forward(c), tl)
                case Nil      => c
            }
        }

        forwardIter(c, wires)
    }

    def backward(c: Char): Char = {
        def backwardIter(c: Char, l: List[Wire]): Char = {
            l match {
                case hd :: tl => hd.backward(backwardIter(c, tl))
                case Nil      => c
            }
        }

        backwardIter(c, wires)
    }

    def tick(value: Wire): Wire = {
        val s = value.connection
        val x = s.length


        def tickIter(s: String, n: Int): String = {
            if (n < x - 1) {
                //println("CASE 0")
                val c = ((s(n + 1) - 'A'.toInt - 1 + 26) % 26 + 'A'.toInt).toChar
                c.toString + tickIter(s, n + 1)
            }
            else if (n == x - 1) {
                val c = ((s(0) - 'A'.toInt - 1 + 26) % 26 + 'A'.toInt).toChar
                c.toString + tickIter(s, n + 1)
            }
                 else {
                     //println("CASE ELSE")
                     ""
                 }
        }

        Wire(tickIter(s, 0))
    }

    def next(): List[Wire] = {
        def nextIter(n: Int, state: Boolean, l: List[Wire]): List[Wire] = {
            l match {
                case Nil                  => Nil
                case hd :: tl if (n == 0) =>
                    if (tick(hd).connection == initState.head.connection) tick(hd) +: nextIter(n + 1, state = true, tl)
                    else tick(hd) +: nextIter(n + 1, state = false, tl)

                case hd :: tl if (state) =>
                    if (tick(hd) == initState(n)) tick(hd) +: nextIter(n + 1, state = true, tl)
                    else tick(hd) +: nextIter(n + 1, state = false, tl)
                case x                   => x
            }
        }

        nextIter(0, state = false, wires)
    }
}

case class Reflector(wire: Wire) extends EnigmaParts
{
    def forward(c: Char): Char = wire.forward(c)

    def backward(c: Char): Char = wire.backward(c)
}

