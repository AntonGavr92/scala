package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage: TweetStorage = InMemoryTweetStorage
  val app = new TwitterApi(storage)

  "Tweet app method create tweet" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = app.createTweet(request).getValue
    tweet.text shouldBe "Test text #test #123#text"
    tweet.likes shouldBe 0
    tweet.user shouldBe "me"

    tweet.hashTags.head shouldBe "#test"
    tweet.hashTags(1) shouldBe "#123"
    tweet.hashTags(2) shouldBe "#text"
  }

  "Tweet app method get tweet" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = app.createTweet(request).getValue
    app.getTweet(GetTweetRequest(tweet.id)).getValue shouldBe tweet
  }

  "Tweet app method addLike" should "be work correct" in {
    val request = CreateTweetRequest("Test text #test #123#text", "me")
    val tweet = app.createTweet(request).getValue
    app.addLike(LikeRequest(tweet.id)).getValue shouldBe 1
  }

}
