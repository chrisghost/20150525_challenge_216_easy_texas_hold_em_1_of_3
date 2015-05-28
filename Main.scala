object Main extends App {
  def printCards(c: Seq[Card]) = c.mkString(", ")

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

case class Card(col: Int, num: Int) {
  def colStr = col match { case 1 => 9829.toChar case 2 => 9830.toChar case 3 => 9827.toChar case _ => 9824.toChar }
  def numStr = num match { case 11 => "J" case 12 => "Q" case 13 => "K" case _ => num }
  override def toString: String = colStr+""+numStr
}
