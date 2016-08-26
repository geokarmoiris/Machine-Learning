package ml.id3

import java.io.File


class ArffFileParser(filePath: String) {

  def parseArffFile(): DataSet = {
    val arffFile = new File(filePath)
    val source = scala.io.Source.fromFile(arffFile).getLines().toList.zipWithIndex
    var dataIndex = 0

    val (attributes, instances) = source.foldLeft(Nil: List[Attribute], Nil: List[Instance]) { case ((attrs, instans), (line, index)) =>
      if (line.startsWith("@DATA")) {
        dataIndex = index
      }
      if (line.startsWith("@ATTRIBUTE")) {
        val splitted = line.split(" ", 3)
        val a = Attribute(splitted(1), index, splitted(2).trim.drop(1).dropRight(1).split(',').toList)
        (a :: attrs, instans)
      } else if (index > dataIndex && ! line.startsWith("@ATTRIBUTE") && ! line.startsWith("@RELATION") && ! line.isEmpty) {
        (attrs,  Instance("test", line.split(',').toList) :: instans)
      } else {
        (attrs, instans)
      }
    }
    val valueCount = instances.flatMap(_.values).groupBy(s=>s).transform((_, v) => v.length.toDouble)
    DataSet(valueCount, attributes, instances, instances.length, None)
  }

}
