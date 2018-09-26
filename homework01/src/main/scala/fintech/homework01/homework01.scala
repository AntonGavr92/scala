package fintech.homework01

// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |
// и т.д.

class Hangman(io: IODevice) {

  val NotGuessedSymbol = "_"
  val EmptyStatus = ""

  def play(word: String): Unit = {
    val startState = new GameState(Set[Char](), word.toCharArray.toSet, stages.iterator, EmptyStatus)
    Stream.iterate(startState)(state => play(word, state))
      .takeWhile(state => !state.gameIsOver && !state.gameIsLost).force
  }

  def play(word: String, state: GameState): GameState = {
    io.printLine("Word: " + guessedWord(word, state.guessedLetters))
    io.printLine("Guess a letter:")
    val newState = guess(state)
    io.printLine(newState.status)
    if (state.gameIsLost()) {
      io.printLine("You are dead")
    } else if (newState.gameIsOver()) {
      io.printLine("Congratulation! You win!")
    }
    newState
  }

  def guess(state: GameState): GameState = {
    try {
      state.guess(letterFrom(io.readLine()))
    } catch {
      case e: IllegalArgumentException =>
        io.printLine(e.getMessage)
        guess(state)
    }
  }

  def letterFrom(input: String): Char = {
    if (input.length != 1) {
      throw new IllegalArgumentException("Write only one letter")
    } else {
      input.head
    }
  }

  def guessedWord(word: String, guessedLetters: Set[Char]): String = {
    word.map(letter =>
      if (guessedLetters.contains(letter))
        letter
      else
        NotGuessedSymbol
    ).mkString
  }

  class GameState(val guessedLetters: Set[Char], val notGuessedLetters: Set[Char], val wrongLetterAnswers: Iterator[String], val status: String) {

    def guess(letter: Char): GameState = {
      if (guessedLetters.contains(letter)) {
        throw new IllegalArgumentException("You used this letter, try another")
      } else if (notGuessedLetters.contains(letter)) {
        new GameState(guessedLetters + letter, notGuessedLetters - letter, wrongLetterAnswers, status)
      } else {
        new GameState(guessedLetters, notGuessedLetters, wrongLetterAnswers, wrongLetterAnswers.next())
      }
    }

    def gameIsOver(): Boolean = notGuessedLetters.isEmpty

    def gameIsLost(): Boolean = !wrongLetterAnswers.hasNext

  }

  val stages = List(
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit

  def readLine(): String
}
