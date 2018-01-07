import java.io.File

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

case class NFA(start: String, transitions: List[(String, Char, String)], finals: Set[String]) {

  def solveByBacktracking(start: String, input: String, seqOfTran: ListBuffer[(String, Char, String)], inputLength: Int): Option[List[(String, Char, String)]] = {
    // Using Backtracking to find at least one accepted sequence of input characters
    val currentState = start
    var sequenceOfTransitions: ListBuffer[(String, Char, String)] = seqOfTran
    var outputTransitions = sequenceOfTransitions.toList
    if (finals.contains(currentState) && sequenceOfTransitions.length == inputLength) {
      outputTransitions = sequenceOfTransitions.toList
      println("Accepted Sequence of Transitions")
      println(outputTransitions)
      sequenceOfTransitions.clear()
      return Some(outputTransitions)
    }
    else if (input.isEmpty){
      sequenceOfTransitions.clear()
      println("Rejected Sequence of Transitions")
      return None
    }
    var ch = input.head

    for (state <- transitions) {
      if (currentState == state._1 && ch == state._2) {
        sequenceOfTransitions += state
        this.solveByBacktracking(state._3, input.tail, sequenceOfTransitions, inputLength)
      }
    }
    return None
  }

  def solveBySetOfPaths(start: String, input: String, seqOfTran: ListBuffer[(String, Char, String)]): Option[List[String]] = ???

}


object NFA {
  def main(args:Array[String]) = {
    // Read file, create NonDeterministicFiniteAutomata and feed it with input
    val file = new File("./input.txt")
    var start: String = new String()
    var transitions: List[(String, Char, String)] = List()
    var finals: Set[String] = Set()
    var input: ArrayBuffer[String] = ArrayBuffer()
    var automato: NFA = NFA(start, transitions, finals)
    var inputLength: Int = 0
    for (line <- Source.fromFile(file).getLines()) {

      if (line.startsWith("START")) {
        var words = line.split("\\s+").filterNot(_ == "")
        if (words.length == 2) {
          start = words(1)
          println("New Automato with start state " + start + " created")
        }
        else {
          println("Automato contains more than one START states")
        }
      }

      if (line.startsWith("END")) {
        println("Automato ready")
        automato = NFA(start, transitions, finals)
        for (inputLine <- input) {
          val output = automato.solveByBacktracking(start, inputLine, ListBuffer(), inputLength)
        }
      }

      if (line.startsWith("FINAL")) {
        var words = line.split("\\s+").filterNot(_ == "")
        words = words.tail
        for (word <- words) {
          finals += (word)
        }
      }

      if (line.startsWith("TRANSITIONS")) {
        var words = line.split("\\s+").filterNot(_ == "")

        words = words.tail
        var count = 0
        var from: String = new String()
        var in: Char = ' '
        var to: String = new String()
        var transReady = false
        for (word <- words) {
          if(count%3 == 0) {
            from = word
          }
          else if (count%3 == 1) {
            in = word.charAt(0)
          }
          else if (count%3 == 2) {
            to = word
            transReady = true
          }
          if (transReady == true) {
            transitions :+= (from, in, to)
            transReady = false
          }
          count += 1
        }
      }

      if (line.startsWith("INPUT")) {
        // feed Automato with input
        var words = line.split("\\s+").filterNot(_ == "")
        words = words.tail
        inputLength = 0
        var inputLine: String = new String()
        for (word <- words) {
          for (ch <- word) {
            inputLine += ch
            inputLength += 1
          }
        }
        input += inputLine
      }
    }
  }
}
