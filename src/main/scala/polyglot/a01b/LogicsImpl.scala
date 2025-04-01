package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.*

import scala.util.Random
import polyglot.a01b.Logics

import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */

trait Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

object Logics:
  def apply(size: Int, mines: Int): Logics = LogicsImpl(size, mines)

  private class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
    private val rnd: Random = Random()
    private val minesPositions = putMines()
    private var countMoves = 0
    private var isWon = false

    def hit(x: Int, y: Int): java.util.Optional[Integer] =
      if minesPositions.contains((x, y))
      then OptionToOptional(ScalaOptional.Empty())
      else
        countMoves = countMoves + 1
        if countMoves == size * size - mines then
          isWon = true
          OptionToOptional(ScalaOptional.Empty())
        else
          OptionToOptional(ScalaOptional.Just(if hasMinesNear(x, y) then 1 else 0))

    def won = isWon

    private def putMines(): Sequence[(Int, Int)] =
      @tailrec
      def generateUniquePositions(seq: Sequence[(Int, Int)], numMines: Int): Sequence[(Int, Int)] =
        if numMines == 0 then seq
        else
          val pos = (rnd.nextInt(size), rnd.nextInt(size))
          if seq.contains(pos) then generateUniquePositions(seq, numMines)
          else
            println(pos)
            generateUniquePositions(seq.concat(Sequence.apply(pos)), numMines - 1)

      generateUniquePositions(Sequence.apply(), mines)

    private def hasMinesNear(x: Int, y: Int): Boolean =
      val neighbors = Sequence(
        (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
        (x - 1, y), (x + 1, y),
        (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
      )
      !neighbors.filter(minesPositions.contains).isEmpty