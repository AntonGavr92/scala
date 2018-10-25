package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage: TweetStorage = InMemoryTweetStorage
  val app = new TwitterApi(storage)

  "Tweet app method create tweet" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = unpack(app.createTweet(request))
    tweet.text shouldBe "Test text #test #123#text"
    tweet.likes shouldBe 0
    tweet.user shouldBe "me"

    tweet.hashTags.head shouldBe "#test"
    tweet.hashTags(1) shouldBe "#123"
    tweet.hashTags(2) shouldBe "#text"
  }

  "Tweet app method get tweet" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = unpack(app.createTweet(request))
    unpack(app.getTweet(GetTweetRequest(tweet.id))) shouldBe tweet
  }

  "Tweet app method addLike" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = unpack(app.createTweet(request))
    unpack(app.addLike(LikeRequest(tweet.id))) shouldBe 1
  }

  private def unpack[E](result: Result[E]): E = {
    result match {
      case Success(res) => res
      case Error(_) => throw new IllegalArgumentException();
    }
  }

}
