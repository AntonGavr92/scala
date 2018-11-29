package ru.tinkoff.fintech.homework09.twtr

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

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
  * Все функции должны возвращать значение типа Future[?]
  *
  *
  * Если же сложилось непоправимое(например обрушилась сеть),
  * то необходимо обработать это достойным образом.
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)




trait TweetStorage {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def save(tweet: Tweet): Future[Tweet]

  def get(id: String): Future[Option[Tweet]]
}

object InMemoryTweetStorage extends TweetStorage {
  private val storage: mutable.Map[String, Tweet] = mutable.Map()

  override def save(tweet: Tweet): Future[Tweet] = {
    Future{
      storage.put(tweet.id, tweet)
      tweet
    }
  }

  override def get(id: String): Future[Option[Tweet]] = Future{storage.get(id)}
}


class TwitterApi(storage: TweetStorage) {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def createTweet(request: CreateTweetRequest): Future[Tweet] = {
    if (requestIsValid(request)) {
      val tweet = Tweet(
        UUID.randomUUID.toString,
        request.user,
        request.text,
        hashTagsFrom(request.text),
        Option(Instant.now()),
        0)
      storage.save(tweet)
    }
    else
      Future.failed(new IllegalArgumentException("Request is not valid!"))
  }

  def getTweet(request: GetTweetRequest): Future[Option[Tweet]] = {
    storage.get(request.id)
  }

  def addLike(request: LikeRequest): Future[Int] = {
    storage.get(request.id)
      .flatMap(optTweet => optTweet.map(tweet => storage.save(tweet.copy(likes = tweet.likes + 1)))
        .getOrElse(Future.failed(new IllegalArgumentException("Tweet not found"))))
      .map(x => x.likes)
  }

  private def requestIsValid(request: CreateTweetRequest): Boolean = {
    request.text.length > 0
  }

  private def hashTagsFrom(text: String): Seq[String] = {
    "#[a-zA-Z0-9]*".r.findAllIn(text).toSeq
  }

}

object TweetApiExample extends App {

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  val storage: TweetStorage = InMemoryTweetStorage
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val res = app.createTweet(request)

  res.onComplete {
    case Success(x) => println("tweet creating success " + x)
    case Failure(e) => println("Oups, something not ok " + e)
  }
  Await.result(res, 1 second)

  // конечно-же программа должна что-то напечатать в stdout
}