package services.recordLinkage

import java.io.File

import models._
import play.api.db.Database
import weka.classifiers.meta.FilteredClassifier
import weka.classifiers.trees.RandomForest
import weka.core.tokenizers.NGramTokenizer
import weka.core.{Instance, Attribute, FastVector, Instances}
import weka.filters.unsupervised.attribute.StringToWordVector

object LeanredPairwiseMergability {

  def link(recs : List[Record], dataType : String, db : Database) : List[Record] = {
    if (recs == null || recs.isEmpty) return recs
    val dataSet =
      if (dataType == "Publication") publicationTrainingSet(publicationTrainingData(db))
      else if (dataType == "Grant") grantTrainingSet(grantTrainingData(db))
      else if (dataType == "Patent") patentTrainingSet(patentTrainingData(db))
      else new Instances("Empty", new FastVector(0), 0)
    if (dataSet.numInstances() == 0) {
      recs
    }
    else {
      val predictSet =
        if (dataType == "Publication") publicationTestingSet(recs.asInstanceOf[List[Publication]])
        else if (dataType == "Grant") grantTestingSet(recs.asInstanceOf[List[Grant]])
        else if (dataType == "Patent") patentTestingSet(recs.asInstanceOf[List[Patent]])
        else new Instances("Empty", new FastVector(0), 0)
      try {
        val tokenizer : NGramTokenizer  = new NGramTokenizer()
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
        val linked = Array.fill[Boolean](recs.size)(false)
        var i = 0
        val pub_arr = recs.toArray
        var ret = List[Record]()
        for (x <- pub_arr.indices) {
          if (linked(x)) {
            i += pub_arr.length - x - 1
          }
          else {
            for (y <- x+1 until pub_arr.length) {
              if (!linked(y)) {
                val merge = classifier.classifyInstance(predictSet.instance(i)) == 0
                if (merge) {
                  pub_arr(x).origRecords ::= pub_arr(y)
                  linked(y) = true
                }
              }
              i += 1
            }
            ret ::= pub_arr(x)
          }
          linked(x) = true
        }
        ret
      }
      catch {
        case e : Exception => {
          e.printStackTrace()
          List[Record]()
        }
      }
    }
  }

  def publicationTrainingData(db : Database) : List[(Publication, Publication, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Publication, Publication, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT title1,authors1,publisher1,year1,affiliation1,citation1,title2,authors2,publisher2,year2,affiliation2,citation2,mergable FROM publication_linkage;")
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Publication(
          "",
          "",
          Some(rs.getString("title1")),
          Some(rs.getString("authors1")),
          Some(rs.getString("publisher1")),
          if (rs.getInt("year1") != -1) Some(rs.getInt("year1")) else None,
          Some(rs.getString("affiliation1")),
          if (rs.getInt("citation1") != -1) Some(rs.getLong("citation1")) else None
        ),
        Publication(
          "",
          "",
          Some(rs.getString("title2")),
          Some(rs.getString("authors2")),
          Some(rs.getString("publisher2")),
          if (rs.getInt("year2") != -1) Some(rs.getInt("year2")) else None,
          Some(rs.getString("affiliation2")),
          if (rs.getInt("citation2") != -1) Some(rs.getLong("citation2")) else None
        ),
        rs.getBoolean("mergable"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def publicationTrainingSet(pubs : List[(Publication, Publication, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR1"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION1"))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR2"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION2"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, pubs.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- pubs) {
      var pub = pair._1
      val inst = new Instance(13)
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
      pub = pair._2
      if (pub.title.isDefined)
        inst.setValue(6, pub.title.get)
      else
        inst.setMissing(6)
      if (pub.authors.isDefined)
        inst.setValue(7, pub.authors.get)
      else
        inst.setMissing(7)
      if (pub.publisher.isDefined)
        inst.setValue(8, pub.publisher.get)
      else
        inst.setMissing(8)
      if (pub.year.isDefined)
        inst.setValue(9, pub.year.get)
      else
        inst.setMissing(9)
      if (pub.affiliation.isDefined)
        inst.setValue(10, pub.affiliation.get)
      else
        inst.setMissing(10)
      if (pub.citation.isDefined)
        inst.setValue(11, pub.citation.get)
      else
        inst.setMissing(11)
      inst.setClassValue(if (pair._3) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }

  def publicationTestingSet(pubs : List[Publication]): Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR1"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION1"))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AUTHORS2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PUBLISHER2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR2"))
    attrs.addElement(new Attribute("FIELD: AFFILIATION2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: CITATION2"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, pubs.size)
    ret.setClass(classLabel)
    //  Build specific record
    val pub_arr : Array[Publication] = pubs.toArray
    for (i <- pub_arr.indices) {
      for (j <- i + 1 until pub_arr.size) {
        val inst = new Instance(13)
        inst.setDataset(ret)
        val pub1 = pub_arr(i)
        val pub2 = pub_arr(j)
        if (pub1.title.isDefined)
          inst.setValue(0, pub1.title.get)
        else
          inst.setMissing(0)
        if (pub1.authors.isDefined)
          inst.setValue(1, pub1.authors.get)
        else
          inst.setMissing(1)
        if (pub1.publisher.isDefined)
          inst.setValue(2, pub1.publisher.get)
        else
          inst.setMissing(2)
        if (pub1.year.isDefined)
          inst.setValue(3, pub1.year.get)
        else
          inst.setMissing(3)
        if (pub1.affiliation.isDefined)
          inst.setValue(4, pub1.affiliation.get)
        else
          inst.setMissing(4)
        if (pub1.citation.isDefined)
          inst.setValue(5, pub1.citation.get)
        else
          inst.setMissing(5)
        if (pub2.title.isDefined)
          inst.setValue(6, pub2.title.get)
        else
          inst.setMissing(6)
        if (pub2.authors.isDefined)
          inst.setValue(7, pub2.authors.get)
        else
          inst.setMissing(7)
        if (pub2.publisher.isDefined)
          inst.setValue(8, pub2.publisher.get)
        else
          inst.setMissing(8)
        if (pub2.year.isDefined)
          inst.setValue(9, pub2.year.get)
        else
          inst.setMissing(9)
        if (pub2.affiliation.isDefined)
          inst.setValue(10, pub2.affiliation.get)
        else
          inst.setMissing(10)
        if (pub2.citation.isDefined)
          inst.setValue(11, pub2.citation.get)
        else
          inst.setMissing(11)
        ret.add(inst)
      }

    }
    ret
  }

  def grantTrainingData (db : Database) : List[(Grant, Grant, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Grant, Grant, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT TITLE1, INVESTIGATOR1, AFFILIATION1, AGENCY1, YEAR1, AMOUNT1, TITLE2, INVESTIGATOR2, AFFILIATION2, AGENCY2, YEAR2, AMOUNT2, MERGABLE FROM grant_linkage;")
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Grant(
          "",
          "",
          Some(rs.getString("TITLE1")),
          Some(rs.getString("INVESTIGATOR1")),
          Some(rs.getString("AFFILIATION1")),
          Some(rs.getString("AGENCY1")),
          if (rs.getInt("YEAR1") != -1) Some(rs.getInt("YEAR1")) else None,
          if (rs.getInt("AMOUNT1") != -1) Some(rs.getLong("AMOUNT1")) else None
        ),
          Grant(
            "",
            "",
            Some(rs.getString("TITLE2")),
            Some(rs.getString("INVESTIGATOR2")),
            Some(rs.getString("AFFILIATION2")),
            Some(rs.getString("AGENCY2")),
            if (rs.getInt("YEAR2") != -1) Some(rs.getInt("YEAR2")) else None,
            if (rs.getInt("AMOUNT2") != -1) Some(rs.getLong("AMOUNT2")) else None
          ),
          rs.getBoolean("MERGABLE"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def grantTrainingSet(grants : List[(Grant, Grant, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR1"))
    attrs.addElement(new Attribute("FIELD: AMOUNT1"))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR2"))
    attrs.addElement(new Attribute("FIELD: AMOUNT2"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, grants.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- grants) {
      var g = pair._1
      val inst = new Instance(13)
      inst.setDataset(ret)
      if (g.title.isDefined)
        inst.setValue(0, g.title.get)
      else
        inst.setMissing(0)
      if (g.investigator.isDefined)
        inst.setValue(1, g.investigator.get)
      else
        inst.setMissing(1)
      if (g.affiliation.isDefined)
        inst.setValue(2, g.affiliation.get)
      else
        inst.setMissing(2)
      if (g.agency.isDefined)
        inst.setValue(3, g.agency.get)
      else
        inst.setMissing(3)
      if (g.year.isDefined)
        inst.setValue(4, g.year.get)
      else
        inst.setMissing(4)
      if (g.amount.isDefined)
        inst.setValue(5, g.amount.get)
      else
        inst.setMissing(5)
      g = pair._2
      if (g.title.isDefined)
        inst.setValue(6, g.title.get)
      else
        inst.setMissing(6)
      if (g.investigator.isDefined)
        inst.setValue(7, g.investigator.get)
      else
        inst.setMissing(7)
      if (g.affiliation.isDefined)
        inst.setValue(8, g.affiliation.get)
      else
        inst.setMissing(8)
      if (g.agency.isDefined)
        inst.setValue(9, g.agency.get)
      else
        inst.setMissing(9)
      if (g.year.isDefined)
        inst.setValue(10, g.year.get)
      else
        inst.setMissing(10)
      if (g.amount.isDefined)
        inst.setValue(11, g.amount.get)
      else
        inst.setMissing(11)
      inst.setClassValue(if (pair._3) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }

  def grantTestingSet(grants : List[Grant]): Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR1"))
    attrs.addElement(new Attribute("FIELD: AMOUNT1"))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVESTIGATOR2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AFFILIATION2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: AGENCY2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: YEAR2"))
    attrs.addElement(new Attribute("FIELD: AMOUNT2"))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, grants.size)
    ret.setClass(classLabel)
    //  Build specific record
    val pub_arr : Array[Grant] = grants.toArray
    for (i <- pub_arr.indices) {
      for (j <- i + 1 until pub_arr.size) {
        val inst = new Instance(13)
        inst.setDataset(ret)
        val grant1 = pub_arr(i)
        val grant2 = pub_arr(j)
        if (grant1.title.isDefined)
          inst.setValue(0, grant1.title.get)
        else
          inst.setMissing(0)
        if (grant1.investigator.isDefined)
          inst.setValue(1, grant1.investigator.get)
        else
          inst.setMissing(1)
        if (grant1.affiliation.isDefined)
          inst.setValue(2, grant1.affiliation.get)
        else
          inst.setMissing(2)
        if (grant1.agency.isDefined)
          inst.setValue(3, grant1.agency.get)
        else
          inst.setMissing(3)
        if (grant1.year.isDefined)
          inst.setValue(4, grant1.year.get)
        else
          inst.setMissing(4)
        if (grant1.amount.isDefined)
          inst.setValue(5, grant1.amount.get)
        else
          inst.setMissing(5)
        if (grant2.title.isDefined)
          inst.setValue(6, grant2.title.get)
        else
          inst.setMissing(6)
        if (grant2.investigator.isDefined)
          inst.setValue(7, grant2.investigator.get)
        else
          inst.setMissing(7)
        if (grant2.affiliation.isDefined)
          inst.setValue(8, grant2.affiliation.get)
        else
          inst.setMissing(8)
        if (grant2.agency.isDefined)
          inst.setValue(9, grant2.agency.get)
        else
          inst.setMissing(9)
        if (grant2.year.isDefined)
          inst.setValue(10, grant2.year.get)
        else
          inst.setMissing(10)
        if (grant2.amount.isDefined)
          inst.setValue(11, grant2.amount.get)
        else
          inst.setMissing(11)
        ret.add(inst)
      }

    }
    ret
  }

  def patentTrainingData (db : Database) : List[(Patent, Patent, Boolean)] = {
    val conn = db.getConnection()
    var lst = List[(Patent, Patent, Boolean)]()
    try {
      val stmt = conn.prepareStatement("SELECT * FROM patent_linkage;")
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= (Patent(
          "",
          "",
          Some(rs.getString("TITLE1")),
          Some(rs.getString("INVENTOR1")),
          Some(rs.getString("FILED1")),
          Some(rs.getString("ISSUED1")),
          Some(rs.getString("PATENTNUM1")),
          Some(rs.getString("ASIGNEE1")),
          None
        ),
          Patent(
            "",
            "",
            Some(rs.getString("TITLE2")),
            Some(rs.getString("INVENTOR2")),
            Some(rs.getString("FILED2")),
            Some(rs.getString("ISSUED2")),
            Some(rs.getString("PATENTNUM2")),
            Some(rs.getString("ASIGNEE2")),
            None
          ),
          rs.getBoolean("MERGABLE"))
      }
    } finally {
      conn.close()
    }
    lst
  }

  def patentTrainingSet(patents : List[(Patent, Patent, Boolean)]) : Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE2", null.asInstanceOf[FastVector]))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, patents.size)
    ret.setClass(classLabel)
    //  Build specific record
    for (pair <- patents) {
      var p = pair._1
      val inst = new Instance(13)
      inst.setDataset(ret)
      if (p.title.isDefined)
        inst.setValue(0, p.title.get)
      else
        inst.setMissing(0)
      if (p.inventor.isDefined)
        inst.setValue(1, p.inventor.get)
      else
        inst.setMissing(1)
      if (p.filed.isDefined)
        inst.setValue(2, p.filed.get)
      else
        inst.setMissing(2)
      if (p.issued.isDefined)
        inst.setValue(3, p.issued.get)
      else
        inst.setMissing(3)
      if (p.patentNum.isDefined)
        inst.setValue(4, p.patentNum.get)
      else
        inst.setMissing(4)
      if (p.asignee.isDefined)
        inst.setValue(5, p.asignee.get)
      else
        inst.setMissing(5)
      p = pair._2
      if (p.title.isDefined)
        inst.setValue(6, p.title.get)
      else
        inst.setMissing(6)
      if (p.inventor.isDefined)
        inst.setValue(7, p.inventor.get)
      else
        inst.setMissing(7)
      if (p.filed.isDefined)
        inst.setValue(8, p.filed.get)
      else
        inst.setMissing(8)
      if (p.issued.isDefined)
        inst.setValue(9, p.issued.get)
      else
        inst.setMissing(9)
      if (p.patentNum.isDefined)
        inst.setValue(10, p.patentNum.get)
      else
        inst.setMissing(10)
      if (p.asignee.isDefined)
        inst.setValue(11, p.asignee.get)
      else
        inst.setMissing(11)
      inst.setClassValue(if (pair._3) "positive" else "negative")
      ret.add(inst)
    }
    ret
  }

  def patentTestingSet(patents : List[Patent]): Instances = {
    //  Build data set & schema
    val attrs : FastVector = new FastVector(13)
    attrs.addElement(new Attribute("FIELD: TITLE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE1", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: TITLE2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: INVENTOR2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: FILED2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ISSUED2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: PATENTNUM2", null.asInstanceOf[FastVector]))
    attrs.addElement(new Attribute("FIELD: ASIGNEE2", null.asInstanceOf[FastVector]))
    val classValues : FastVector = new FastVector(2)
    classValues.addElement("positive")
    classValues.addElement("negative")
    val classLabel : Attribute = new Attribute("CLASS LABEL", classValues)
    attrs.addElement(classLabel)
    val ret = new Instances("dataSet", attrs, patents.size)
    ret.setClass(classLabel)
    //  Build specific record
    val pub_arr : Array[Patent] = patents.toArray
    for (i <- pub_arr.indices) {
      for (j <- i + 1 until pub_arr.size) {
        val inst = new Instance(13)
        inst.setDataset(ret)
        val grant1 = pub_arr(i)
        val grant2 = pub_arr(j)
        if (grant1.title.isDefined)
          inst.setValue(0, grant1.title.get)
        else
          inst.setMissing(0)
        if (grant1.inventor.isDefined)
          inst.setValue(1, grant1.inventor.get)
        else
          inst.setMissing(1)
        if (grant1.filed.isDefined)
          inst.setValue(2, grant1.filed.get)
        else
          inst.setMissing(2)
        if (grant1.issued.isDefined)
          inst.setValue(3, grant1.issued.get)
        else
          inst.setMissing(3)
        if (grant1.patentNum.isDefined)
          inst.setValue(4, grant1.patentNum.get)
        else
          inst.setMissing(4)
        if (grant1.asignee.isDefined)
          inst.setValue(5, grant1.asignee.get)
        else
          inst.setMissing(5)
        if (grant2.title.isDefined)
          inst.setValue(6, grant2.title.get)
        else
          inst.setMissing(6)
        if (grant2.inventor.isDefined)
          inst.setValue(7, grant2.inventor.get)
        else
          inst.setMissing(7)
        if (grant2.filed.isDefined)
          inst.setValue(8, grant2.filed.get)
        else
          inst.setMissing(8)
        if (grant2.issued.isDefined)
          inst.setValue(9, grant2.issued.get)
        else
          inst.setMissing(9)
        if (grant2.patentNum.isDefined)
          inst.setValue(10, grant2.patentNum.get)
        else
          inst.setMissing(10)
        if (grant2.asignee.isDefined)
          inst.setValue(11, grant2.asignee.get)
        else
          inst.setMissing(11)
        ret.add(inst)
      }

    }
    ret
  }
}
