package objsets

import TweetReader.*

import java.util.NoSuchElementException

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface:

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, Empty())

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  def retweetedAcc(mostOrLeastRetweetedSoFar: Tweet, mostRetweeted: Boolean): Tweet

  /**
   *
   */

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = descendingByRetweetAcc(Nil)
  
  def descendingByRetweetAcc(acc: TweetList): TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw NoSuchElementException()

  def retweetedAcc(mostOrLeastRetweetedSoFar: Tweet, mostRetweeted: Boolean): Tweet = mostOrLeastRetweetedSoFar

  def descendingByRetweetAcc(acc: TweetList): TweetList = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:

  /**
   * We kick off the algorithm by passing an empty tweet set in the base class
   *
   * If the current tweet matches the predicate, then we include the tweet in the tweet set and we get a new acc tweet
   * set. If it does not match, then we keep the same acc tweet set
   * Then we recursively search in the left sub tweet set passing the acc acquired in the previous step. We get a new
   * tweet set
   * Finally we recursively search in the right sub tweet set passing the acc acquired in the previous step and we
   * return the tweet set resulting from this step
   * The base case is implemented in the Empty subclass, in which we simply return the acc since an empty tweet set does
   * not contain a tweet to check the predicate
   *
   * Basically, we recursively search the left nodes until we reach an empty tweet set, and then we search the right
   * nodes until we reach another empty tweet set. In each iteration we include the current tweet in the acc if it
   * matches the predicate
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val acc1 = if p(elem) then acc.incl(elem) else acc
    val acc2 = left.filterAcc(p, acc1)
    right.filterAcc(p, acc2)
  }

  /**
   * We kick off the algorithm directly in each subclass
   *
   * The base case is implemented in the Empty subclass and the only possible result of joining an empty tweet set with
   * another tweet set (which may or may not be empty) is the latter, so it returns that
   *
   * In the given tweet set we include the current tweet and we get an acc tweet set
   * We recursively join the left sub tweet set with the acc tweet set acquired in the previous step and we get another
   * acc tweet set
   * Finally we recursively join the right sub tweet set with the acc tweet set acquired in the previous step and return
   * the tweet set resulting from this step
   *
   * Basically, we recursively join the left nodes until we join with an empty tweet set, and then we join the right
   * nodes until we join with another empty tweet set. In each iteration we include the current tweet in the acc
   */
  def union(that: TweetSet): TweetSet = {
    val acc1 = that.incl(elem)
    val acc2 = left.union(acc1)
    right.union(acc2)
  }

  def mostRetweeted: Tweet = retweetedAcc(elem, true)

  /**
   * Let's assume that we want the most retweeted tweet (useMostRetweeted = true). We kick off the algorithm by passing
   * the current tweet as the most retweeted one so far. Getting the most or least retweeted works in the same way so
   * for the explanation let's assume that we are searching for the former
   *
   * We compare the current tweet with the most retweeted one so far to see which one will become the most retweeted
   * (so far xd)
   * Passing this most retweeted one we recursively search in the left sub tweet set for a new most retweeted and get it
   * Finally, we pass the most retweeted so far and search in the right sub tree, finally getting the tweet with the
   * most retweets, and return this result
   *
   * The base case is implemented in the Empty subclass and simply returns the most retweeted so far because there is no
   * tweet for comparison
   *
   * The process is basically the same as the filter one but instead of checking a predicate and getting a tweet set
   * we check the number of retweets and get a tweet
   */
  def retweetedAcc(mostOrLeastRetweetedSoFar: Tweet, useMostRetweeted: Boolean): Tweet = {
    val mostOrLeastRetweeted1 = if (useMostRetweeted) {
      if elem.retweets > mostOrLeastRetweetedSoFar.retweets then elem else mostOrLeastRetweetedSoFar
    } else {
      if elem.retweets < mostOrLeastRetweetedSoFar.retweets then elem else mostOrLeastRetweetedSoFar
    }
    val mostOrLeastRetweeted2 = left.retweetedAcc(mostOrLeastRetweeted1, useMostRetweeted)
    right.retweetedAcc(mostOrLeastRetweeted2, useMostRetweeted)
  }

  def leastRetweeted: Tweet = retweetedAcc(elem, false)

  /**
   * We kick off the algorithm by passing an empty list in the base class
   *
   * For getting this list of tweets in descending order we need to get the tweet with the least retweets first, and we
   * are going to add it at the beginning (head) of the list. This "leastRetweeted" is then removed from the tweetSet.
   * In each iteration the "leastRetweeted" that we get from the tweetSet will actually have more retweets than the
   * previous one, and as we keep on adding to the list each "leastRetweeted" will be pushed to the end. In the final
   * iteration, the tweet we get will be the one with the most retweets and will be positioned at the beginning of the
   * final list, thus getting an ascending order
   *
   * We get the least retweeted tweet
   * Then we remove this tweet from the current tweet set and get a new tweet set
   * We create a new list by putting the tweet as head and the acc tweet list as tail
   * We pass the new list to the new tweet set and begin again
   * When the tweet set is empty we simply return the acc and break off recursion
   */
  def descendingByRetweetAcc(acc: TweetList): TweetList = {
    val mostRetweeted = this.leastRetweeted
    val tweetSet = remove(mostRetweeted)
    val newList = Cons(mostRetweeted, acc)
    tweetSet.descendingByRetweetAcc(newList)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = getTweetSetByKeywords(google)

  lazy val appleTweets: TweetSet = getTweetSetByKeywords(apple)

  /**
   * For each tweet in the tweet set we check if it contains at least one of the keywords we are searching for. We get a
   * tweet set with tweets that match this condition
   */
  def getTweetSetByKeywords(keywords: List[String]): TweetSet =
    TweetReader.allTweets.filter(tweet => keywords.exists(keyword => tweet.text.contains(keyword)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
