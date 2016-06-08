package services.grouping

import models._
import weka.clusterers.SimpleKMeans
import weka.core.Attribute
import weka.core.FastVector
import weka.core.Instance
import weka.core.Instances
import weka.core.tokenizers.NGramTokenizer
import weka.filters.Filter
import weka.filters.unsupervised.attribute.StringToWordVector
import java.io.File

object KMeans {

  def group (recs : List[Record], dataType : String) : Array[List[Record]] = {
    if (recs == null || recs.isEmpty) return Array.fill[List[Record]](0)(List[Record]())
    val numRecords = recs.size.toDouble
    val dataSet =
      if (dataType == "Publication") publicationDataset(recs.asInstanceOf[List[Publication]])
      else if (dataType == "Grant") grantDataset(recs.asInstanceOf[List[Grant]])
      else if (dataType == "Patent") patentDataset(recs.asInstanceOf[List[Patent]])
      else new Instances("Empty", new FastVector(0), 0)
    if (dataSet.numInstances() == 0) new Array[List[Record]](0)
    else {
      try {
        val tokenizer : NGramTokenizer  = new NGramTokenizer();
        tokenizer.setNGramMinSize(1)
        tokenizer.setNGramMaxSize(1)
        tokenizer.setDelimiters("\\W")
        val filter  : StringToWordVector = new StringToWordVector()
        filter.setInputFormat(dataSet)
        filter.setTokenizer(tokenizer)
        filter.setWordsToKeep(1024)
        filter.setDoNotOperateOnPerClassBasis(true)
        filter.setOutputWordCounts(true)
        filter.setLowerCaseTokens(true)
        filter.setTFTransform(true)
        filter.setUseStoplist(true)
        filter.setStopwords(new File("utils/stopwords.txt"))
        val transformedDataSet = Filter.useFilter(dataSet, filter)
        val kMeans : SimpleKMeans  = new SimpleKMeans()
        val numClusters = (Math.sqrt(numRecords/2)+1).toInt
        kMeans.setNumClusters(numClusters)
        kMeans.buildClusterer(transformedDataSet)
        val groups = Array.fill[List[Record]](numClusters)(List[Record]())
        var i = 0
        for (rec <- recs) {
          groups( kMeans.clusterInstance(transformedDataSet.instance(i)) ) ::= rec
          i += 1
        }
        groups
      }
      catch {
        case e : Exception => {
          e.printStackTrace()
          Array.fill[List[Record]](0)(List[Record]())
        }
      }
    }
  }

  def publicationDataset(pubs : List[Publication]): Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(6)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION"))
    val ret = new Instances("dataSet", attrs, pubs.size)
    //  Build specific record
    for (pub <- pubs) {
      val inst = new Instance(6)
      inst.setDataset(ret)
      if (pub.title.isDefined)
        inst.setValue(0, pub.title.get)
      else
        inst.setMissing(0)
      if (pub.authors.isDefined)
        inst.setValue(1, pub.authors.get)
      else
        inst.setMissing(1)
      if (pub.publisher.isDefined)
        inst.setValue(2, pub.publisher.get)
      else
        inst.setMissing(2)
      if (pub.year.isDefined)
        inst.setValue(3, pub.year.get)
      else
        inst.setMissing(3)
      if (pub.affiliation.isDefined)
        inst.setValue(4, pub.affiliation.get)
      else
        inst.setMissing(4)
      if (pub.citation.isDefined)
        inst.setValue(5, pub.citation.get)
      else
        inst.setMissing(5)
      ret.add(inst)
    }
    ret
  }

  def grantDataset(recs : List[Grant]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(6)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AMOUNT"))
    val ret = new Instances("dataSet", attrs, recs.size)
    //  Build specific record
    for (rec <- recs) {
      val inst = new Instance(6)
      inst.setDataset(ret)
      if (rec.title.isDefined)
        inst.setValue(0, rec.title.get)
      else
        inst.setMissing(0)
      if (rec.investigator.isDefined)
        inst.setValue(1, rec.investigator.get)
      else
        inst.setMissing(1)
      if (rec.affiliation.isDefined)
        inst.setValue(2, rec.affiliation.get)
      else
        inst.setMissing(2)
      if (rec.agency.isDefined)
        inst.setValue(3, rec.agency.get)
      else
        inst.setMissing(3)
      if (rec.year.isDefined)
        inst.setValue(4, rec.year.get)
      else
        inst.setMissing(4)
      if (rec.amount.isDefined)
        inst.setValue(5, rec.amount.get)
      else
        inst.setMissing(5)
      ret.add(inst)
    }
    ret
  }

  def patentDataset(recs : List[Patent]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ABSTRACT", null.asInstanceOf[FastVector]))
    val ret = new Instances("dataSet", attrs, recs.size)
    //  Build specific record
    for (rec <- recs) {
      val inst = new Instance(7)
      inst.setDataset(ret)
      if (rec.title.isDefined)
        inst.setValue(0, rec.title.get)
      else
        inst.setMissing(0)
      if (rec.inventor.isDefined)
        inst.setValue(1, rec.inventor.get)
      else
        inst.setMissing(1)
      if (rec.filed.isDefined)
        inst.setValue(2, rec.filed.get)
      else
        inst.setMissing(2)
      if (rec.issued.isDefined)
        inst.setValue(3, rec.issued.get)
      else
        inst.setMissing(3)
      if (rec.patentNum.isDefined)
        inst.setValue(4, rec.patentNum.get)
      else
        inst.setMissing(4)
      if (rec.asignee.isDefined)
        inst.setValue(5, rec.asignee.get)
      else
        inst.setMissing(5)
      if (rec.description.isDefined)
        inst.setValue(6, rec.description.get)
      else
        inst.setMissing(6)
      ret.add(inst)
    }
    ret
  }

}
