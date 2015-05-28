object Main extends App {
  def printCards(c: Seq[Card]) = c.mkString(", ")


  //tests

  val flush = List(
    Card(1, 1)
  , Card(1, 2)
  , Card(1, 3)
  , Card(1, 4)
  , Card(1, 5)
  )
  val four = List(
    Card(1, 4)
  , Card(1, 7)
  , Card(1, 9)
  , Card(1, 1)
  , Card(2, 3)
  )
  val full = List(
    Card(1, 4)
  , Card(3, 4)
  , Card(1, 9)
  , Card(2, 9)
  , Card(4, 9)
  )

println(flush.map(_.col).distinct.length == 1)
val sortd = flush.sortBy(_.num).sliding(2)
println(sortd)
  //println(sortd.forall((l: Seq[Card]) => l(0) == l(1) - 1))


  println( Hand.best(flush))
  println( Hand.best(full))
  println( Hand.best(four))


  println("......")


  print("How many players (2-8) ? ")
  val nbPlayers = scala.io.StdIn.readInt

  if(nbPlayers < 2 || nbPlayers > 8) System.exit(1)

  println
  println("Your hand: "+printCards(Deck.deal(2)))
  (1 to nbPlayers-1).map(i => println(s"CPU $i hand: "+printCards(Deck.deal(2))))

  println
  Deck.burn
  println("Flop: "+printCards(Deck.deal(3)))
  Deck.burn
  println("Turn: "+printCards(Deck.deal(1)))
  Deck.burn
  println("River: "+printCards(Deck.deal(1)))
}

object Deck {
  val cards = scala.collection.mutable.Stack[Card](scala.util.Random.shuffle(for {
    col <- 1 to 4
    num <- 1 to 13
  } yield Card(col, num)): _*)

  def burn = deal(1)
  def deal(n: Int) = (1 to n).map { _ => cards.pop }
}

object Hand {
  val hands = List(
    ("Straight flush", (l:Seq[Card]) => l.map(_.col).distinct.length == 1 && l.sortBy(_.num).sliding(2).toList.forall((l: Seq[Card]) => l(0).num == l(1).num - 1))
  , ("Four of a kind", (l:Seq[Card]) => l.groupBy(_.col).map(_._2.length).find(e => e >= 4).isDefined)
  , ("Full House",     (l:Seq[Card]) => {
                          (for {
                            trp   <- triple(l)
                            pair  <- pair(l diff trp)
                          } yield (trp ++ pair)).map(_.length == 5).getOrElse(false)
                        }
    )
  , ("shitty hand",    (_: Seq[Card]) => true)
    )
  def best(l: Seq[Card]) = {
    println(hands.find(_._2(l)))
  }

  def triple(l: Seq[Card]): Option[Seq[Card]] = l.groupBy(_.num).find(_._2.length >= 3).map(_._2)
  def pair(l: Seq[Card]): Option[Seq[Card]] = l.groupBy(_.num).find(_._2.length >= 2).map(_._2)
}

case class Card(col: Int, num: Int) {
  def colStr = col match { case 1 => 9829.toChar case 2 => 9830.toChar case 3 => 9827.toChar case _ => 9824.toChar }
  def numStr = num match { case 11 => "J" case 12 => "Q" case 13 => "K" case _ => num }
  override def toString: String = colStr+""+numStr
}
