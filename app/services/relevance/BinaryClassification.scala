package services.relevance

import models._
import play.api.db.Database
import weka.classifiers.meta.FilteredClassifier
import weka.classifiers.trees.RandomForest
import weka.core.Attribute
import weka.core.FastVector
import weka.core.Instance
import weka.core.Instances
import weka.core.tokenizers.NGramTokenizer
import weka.filters.unsupervised.attribute.StringToWordVector
import java.io.File

object BinaryClassification {

  def predictRelevance (recs : List[Record], dataType : String, srch : Search, db : Database) : Array[List[Record]] = {
    if (recs == null || recs.isEmpty) return Array.fill[List[Record]](2)(List[Record]())
    val dataSet =
      if (dataType == "Publication") publicationTrainingSet(publicationTrainingData(db, srch))
      else if (dataType == "Grant") grantTrainingSet(grantTrainingData(db, srch))
      else if (dataType == "Patent") patentTrainingSet(patentTrainingData(db, srch))
      else new Instances("Empty", new FastVector(0), 0)
    if (dataSet.numInstances() == 0) {
      val ret = Array.fill[List[Record]](2)(List[Record]())
      ret(0) :::= recs
      ret
    }
    else {
      val predictSet =
        if (dataType == "Publication") publicationTestingSet(recs.asInstanceOf[List[Publication]])
        else if (dataType == "Grant") grantTestingSet(recs.asInstanceOf[List[Grant]])
        else if (dataType == "Patent") patentTestingSet(recs.asInstanceOf[List[Patent]])
        else new Instances("Empty", new FastVector(0), 0)
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
        val rf : RandomForest  = new RandomForest()
        val classifier = new FilteredClassifier()
        classifier.setFilter(filter)
        classifier.setClassifier(rf)
        classifier.buildClassifier(dataSet)
        val groups = Array.fill[List[Record]](2)(List[Record]())
        var i = 0
        for (rec <- recs) {
          groups( classifier.classifyInstance(predictSet.instance(i)).toInt ) ::= rec
          i += 1
        }
        groups
      }
      catch {
        case e : Exception => {
          e.printStackTrace()
          Array.fill[List[Record]](2)(List[Record]())
        }
      }
    }
  }

  def publicationTrainingData(db : Database, search: Search) : List[(Publication, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Publication, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT title,authors,publisher,year,affiliation,citation,relevant FROM publication_relevance WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Publication(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("authors")),
          Some(rs.getString("publisher")),
          if (rs.getInt("year") != -1) Some(rs.getInt("year")) else None,
          Some(rs.getString("affiliation")),
          if (rs.getInt("citation") != -1) Some(rs.getInt("citation")) else None
        ), rs.getBoolean("relevant"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def publicationTrainingSet(pubs : List[(Publication, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, pubs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- pubs) {
      val pub = pair._1
      val inst = new Instance(7)
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
      inst.setClassValue(if (pair._2) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }


  def publicationTestingSet(pubs : List[Publication]): Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, pubs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pub <- pubs) {
      val inst = new Instance(7)
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

  def grantTrainingData(db : Database, search: Search) : List[(Grant, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Grant, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT title,investigator,affiliation,agency,year,amount,relevant FROM grant_relevance WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Grant(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("investigator")),
          Some(rs.getString("affiliation")),
          Some(rs.getString("agency")),
          if (rs.getInt("year") != -1) Some(rs.getInt("year")) else None,
          if (rs.getInt("amount") != -1) Some(rs.getLong("amount")) else None
        ), rs.getBoolean("relevant"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def grantTrainingSet(recs : List[(Grant, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AMOUNT"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, recs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- recs) {
      val pub = pair._1
      val inst = new Instance(7)
      inst.setDataset(ret)
      if (pub.title.isDefined)
        inst.setValue(0, pub.title.get)
      else
        inst.setMissing(0)
      if (pub.investigator.isDefined)
        inst.setValue(1, pub.investigator.get)
      else
        inst.setMissing(1)
      if (pub.affiliation.isDefined)
        inst.setValue(2, pub.affiliation.get)
      else
        inst.setMissing(2)
      if (pub.agency.isDefined)
        inst.setValue(3, pub.agency.get)
      else
        inst.setMissing(3)
      if (pub.year.isDefined)
        inst.setValue(4, pub.year.get)
      else
        inst.setMissing(4)
      if (pub.amount.isDefined)
        inst.setValue(5, pub.amount.get)
      else
        inst.setMissing(5)
      inst.setClassValue(if (pair._2) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }

  def grantTestingSet(recs : List[Grant]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR"))
    attrs.addElement(new Attribute("FIELD: AMOUNT"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, recs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (rec <- recs) {
      val inst = new Instance(7)
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

  def patentTrainingData(db : Database, search: Search) : List[(Patent, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Patent, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT * FROM patent_relevance WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Patent(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("inventor")),
          Some(rs.getString("filed")),
          Some(rs.getString("issued")),
          Some(rs.getString("patentNum")),
          Some(rs.getString("asignee")),
          None
        ), rs.getBoolean("relevant"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def patentTrainingSet(recs : List[(Patent, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE", null.asInstanceOf[FastVector]))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, recs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- recs) {
      val pub = pair._1
      val inst = new Instance(7)
      inst.setDataset(ret)
      if (pub.title.isDefined)
        inst.setValue(0, pub.title.get)
      else
        inst.setMissing(0)
      if (pub.inventor.isDefined)
        inst.setValue(1, pub.inventor.get)
      else
        inst.setMissing(1)
      if (pub.filed.isDefined)
        inst.setValue(2, pub.filed.get)
      else
        inst.setMissing(2)
      if (pub.issued.isDefined)
        inst.setValue(3, pub.issued.get)
      else
        inst.setMissing(3)
      if (pub.patentNum.isDefined)
        inst.setValue(4, pub.patentNum.get)
      else
        inst.setMissing(4)
      if (pub.asignee.isDefined)
        inst.setValue(5, pub.asignee.get)
      else
        inst.setMissing(5)
      inst.setClassValue(if (pair._2) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }

  def patentTestingSet(recs : List[Patent]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(7)
    attrs.addElement(new Attribute("FIELD: TITLE", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE", null.asInstanceOf[FastVector]))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, recs.size)
    ret.setClass(classLabel)
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
      ret.add(inst)
    }
    ret
  }

}
