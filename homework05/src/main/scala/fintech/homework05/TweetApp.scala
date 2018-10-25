package fintech.homework05

import java.time.Instant
import java.util.UUID

import scala.collection.mutable

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int) {

  def this(tw: Tweet, likes: Int) {
    this(tw.id, tw.user, tw.text, tw.hashTags, tw.createdAt, likes)
  }
}

sealed trait Result[T] {
  def getValue: T
}
case class Success[T](value: T) extends Result[T] {
  override def getValue: T = {
    value
  }
}
case class Error[T](message: String) extends Result[T] {
  override def getValue: T = {
    throw new UnsupportedOperationException()
  }
}


case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {
  def save(tweet: Tweet): Option[Tweet]

  def get(id: String): Option[Tweet]
}

object InMemoryTweetStorage extends TweetStorage {
  val storage: mutable.Map[String, Tweet] = mutable.Map()

  override def save(tweet: Tweet): Option[Tweet] = {
      storage.put(tweet.id, tweet)
      storage.get(tweet.id)
  }

  override def get(id: String): Option[Tweet] = storage.get(id)

}

class TwitterApi(storage: TweetStorage) {

  def createTweet(request: CreateTweetRequest): Result[Tweet] = {
    if (requestIsValid(request)) {
      val tweet = Tweet(
        UUID.randomUUID.toString,
        request.user,
        request.text,
        hashTagsFrom(request.text),
        Option(Instant.now()),
        0)
      storage.save(tweet)
      Success[Tweet](tweet)
    }
    else
      Error[Tweet]("Request is not valid!")
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = {
    storage.get(request.id)
      .map(tweet => Success[Tweet](tweet))
      .getOrElse(Error[Tweet]("Tweet not found!"))
  }

  def addLike(request: LikeRequest): Result[Int] = {
    storage.get(request.id)
      .map(tweet => storage.save(new Tweet(tweet, tweet.likes + 1)))
      .map(option =>
        option.map(tw => Success[Int](tw.likes)).getOrElse(Error[Int]("Adding like error!"))
      )
      .getOrElse(Error[Int]("Tweet not found!"))
  }

  private def requestIsValid(request: CreateTweetRequest): Boolean = {
    request.text.length > 0
  }

  private def hashTagsFrom(text: String): Seq[String] = {
    "#[a-zA-Z0-9]*".r.findAllIn(text).toSeq
  }

}

object TweetApiExample extends App {

  val storage: TweetStorage = InMemoryTweetStorage
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }

}
