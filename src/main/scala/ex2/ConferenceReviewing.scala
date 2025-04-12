package ex2

import scala.:+
import scala.collection.immutable


enum Question:
  case RELEVANCE // ("È importante per questa conferenza?"),
  case SIGNIFICANCE // ("Produce contributo scientifico?"),
  case CONFIDENCE // ("Ti senti competente a commentarlo?");
  case FINAL // ("É un articolo da accettare?")

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 */
trait ConferenceReviewing:
  /**
   * * @param article
   * * @param scores
   * * loads a review for the specified article, with complete scores as a map
   * */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * * @param article
   * * @param relevance
   * * @param significance
   * * @param confidence
   * * @param fin
   * * loads a review for the specified article, with the 4 explicit scores
   * */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * * @param article
   * * @param question
   * * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   * */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * * @param article
   * * @return the average score to question FINAL taken by the specified article
   * */
  def averageFinalScore(article: Int): Double

  /**
   * * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * * and at least one RELEVANCE score that is >= 8.
   * * @return the set of accepted articles
   * */
  def acceptedArticles(): Set[Int]

  /**
   * * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   * */
  def sortedAcceptedArticles(): List[(Int, Double)]

  /**
   * * @return a map from articles to their average "weighted final score", namely,
   * * the average value of CONFIDENCE*FINAL/10
   * * Note: this method is optional in this exam
   * */
  def averageWeightedFinalScoreMap(): Map[Int, Double]

  /**
   * For each article, the reviewer has to reply to all the following questions
   */


object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    private var reviews: List[(Int, Map[Question, Int])] = List()
    def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      reviews = reviews :+ (article, scores)
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article,
        Map(Question.RELEVANCE -> relevance,
          Question.CONFIDENCE -> confidence,
          Question.SIGNIFICANCE -> significance,
          Question.FINAL -> fin))
    def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter(_._1 == article).map(_._2(question)).sorted
    def averageFinalScore(article: Int): Double =
      reviews.filter(_._1 == article).map(_._2(Question.FINAL)).average
    def acceptedArticles(): Set[Int] = ???
    def sortedAcceptedArticles(): List[(Int, Double)] = ???
    def averageWeightedFinalScoreMap(): Map[Int, Double] = ???
    extension (l: List[Int])
      private def average: Double =
        l.foldLeft((0.0, 1))((acc, v) => ((acc._1 + (v - acc._1) / acc._2), acc._2 + 1))._1