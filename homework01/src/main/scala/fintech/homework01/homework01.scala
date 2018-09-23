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

  val NOT_GUESSED_SYMBOL = "_"

  def play(word: String): Unit = {
    var guessedLetters = word.map(_ => NOT_GUESSED_SYMBOL).mkString
    val wrongLetterAnswers = stages.iterator

    while(guessedLetters.contains(NOT_GUESSED_SYMBOL) && wrongLetterAnswers.hasNext) {
      io.printLine("Word: " + guessedLetters)
      io.printLine("Guess a letter:")
      try {
        val updatedGuessedLetters = updateGuessedLetters(letterFrom(io.readLine()), guessedLetters, word)
        if (guessedLetters == updatedGuessedLetters) {
          io.printLine(wrongLetterAnswers.next())
        } else {
          guessedLetters = updatedGuessedLetters
        }
      } catch {
        case e: IllegalArgumentException => io.printLine(e.getMessage)
      }
    }
    printResultGame(guessedLetters)
  }

  def updateGuessedLetters(letter: Char, guessedLetters: String, word : String) : String = {
    word.map(x =>
      if (x == letter || guessedLetters.contains(x))
        x
      else
        NOT_GUESSED_SYMBOL
    ).mkString
  }

  def printResultGame(guessedLetters: String): Unit = {
    if(guessedLetters.contains(NOT_GUESSED_SYMBOL)) {
      io.printLine("You lose :(")
    } else {
      io.printLine("Congratulation! You win!")
    }
  }

  def letterFrom(input : String) : Char = {
    if(input.length != 1) {
      throw new IllegalArgumentException("Write only one letter")
    } else {
      input(0)
    }
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
