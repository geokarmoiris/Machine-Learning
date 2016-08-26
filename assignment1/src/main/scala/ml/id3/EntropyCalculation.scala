package ml.id3

import scala.math._

object EntropyCalculation extends App {

  def log2(x: Double) = log(x) / log(2)

  val parser  = new ArffFileParser(System.getProperty("user.home") + "/play-ball.arff")

  val dataSet = parser.parseArffFile()

  def calculateEntropy(attributeName: String, valueCount: Map[String, Double], totalCount: Double) = {
    val attributeValues = dataSet.attributes.find(_.name == attributeName).get.values.map(_.trim)
    attributeValues.map { attribute =>
      val p = valueCount(attribute) / totalCount
      if (p == 0.0) 0.0
      else - p * log2(p)
    }.sum
  }

  val entropy = calculateEntropy("Class", dataSet.valueCount, dataSet.totalInstances.toDouble)

  def attributeToClassifierRelation(instances: List[Instance], classifier: String, attributeValue: String): Double = {
    instances.filter(_.values.contains(attributeValue)).flatMap(_.values).count(v => v == classifier).toDouble
  }

  def calculateInformationGain(attributeName: String, collectionEntropy: Double, totalCount: Double, valueCount: Map[String, Double]) = {
    val attributeValues = dataSet.attributes.find(_.name == attributeName).get.values.map(_.trim)
    val valEntropy = attributeValues.map { attrValue =>
      val positives = attributeToClassifierRelation(dataSet.instances, "Yes", attrValue)
      val negatives = attributeToClassifierRelation(dataSet.instances, "No", attrValue)
      val valCount = valueCount(attrValue)
      (valCount / totalCount) * calculateEntropy("Class", Map("Yes" -> positives, "No" -> negatives), valCount)
    }.sum
    collectionEntropy - valEntropy
  }

  def decisionNodeSelection(attributes: List[Attribute]) = {
    attributes.filterNot(_.name == "Class").map(_.name).map { attrName =>
      attrName -> calculateInformationGain(attrName, entropy, dataSet.totalInstances, dataSet.valueCount)
    }.toMap
  }

  val e = decisionNodeSelection(dataSet.attributes).maxBy(_._2)
  println("EEEE", e)
  val rootNode = {
    //val branches =
    Node(e._1, e._2, None, Nil)
  }

  def splitDataSet(dataSet: DataSet, attributeToSplit: Attribute) = {
    attributeToSplit.values.map { value =>
      val attributes = dataSet.attributes.filterNot(_.name == attributeToSplit.name)
      val instances = dataSet.instances.filter(_.values.contains(value))
      val valCount = instances.flatMap(_.values).groupBy(s=>s).transform((_, v) => v.length.toDouble)
      DataSet(valCount, attributes, instances, instances.length, Some(attributeToSplit.name))
    }
  }

  val n = splitDataSet(dataSet, Attribute("Outlook", 2, List("Sunny", "Overcast", "Rain")))
  println("NNN", n)

}

//object EntropyCalculation extends EntropyCalculation
