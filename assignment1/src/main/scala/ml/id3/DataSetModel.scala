package ml.id3


sealed trait DataSetModel

case class Attribute(
  name: String,
  index: Int,
  values: List[String]
) extends DataSetModel

case class Instance(
  id: String,
  values: List[String]
) extends DataSetModel

case class DataSet(
  valueCount: Map[String, Double],
  attributes: List[Attribute],
  instances: List[Instance],
  totalInstances: Int,
  attributeToSplit: Option[String]
) extends DataSetModel

case class Node(
  name: String,
  gain: Double,
  parentNode: Option[String],
  branches: List[Branch]
) extends DataSetModel

case class Branch(
  name: String,
  targetNode: Option[Node],
  positive: Option[Boolean],
  negative: Option[Boolean]
) extends DataSetModel {
  lazy val isLeaf = positive.isDefined || negative.isDefined
}
