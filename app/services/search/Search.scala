package services.search

import models._
import play.api.db._
import services.grouping.KMeans
import services.relevance.BinaryClassification
import services.recordLinkage.LeanredPairwiseMergability

object Search {

  def searchPublication(search: Search, db : Database) : Array[List[Publication]] = {
    val conn = db.getConnection()
    var lst = List[Publication]()
    try {
      val stmt = conn.prepareStatement("SELECT title,authors,publisher,year,affiliation,citation FROM publication WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= Publication(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("authors")),
          Some(rs.getString("publisher")),
          if (rs.getInt("year") != -1) Some(rs.getInt("year")) else None,
          Some(rs.getString("affiliation")),
          if (rs.getInt("citation") != -1) Some(rs.getInt("citation")) else None
        )
      }
    } finally {
      conn.close()
    }
    lst = LeanredPairwiseMergability.link(lst, "Publication", db).map(rec => rec.asInstanceOf[Publication])
    val rel = BinaryClassification.predictRelevance(lst, "Publication", search, db)
    val grouped = KMeans.group(rel(0), "Publication").asInstanceOf[Array[List[Publication]]]
      .map(_.sortWith((p1,p2) => {
        if (p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) != 0) p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) < 0
        else if (p1.publisher.getOrElse("").compareTo(p2.publisher.getOrElse("")) != 0) p1.publisher.getOrElse("").compareTo(p2.publisher.getOrElse("")) < 0
        else if (p1.year.getOrElse(-1) != p2.year.getOrElse(-1)) p1.year.getOrElse(-1)<p2.year.getOrElse(-1)
        else if (p1.authors.getOrElse("").compareTo(p2.authors.getOrElse("")) != 0) p1.authors.getOrElse("").compareTo(p2.authors.getOrElse(""))<0
        else if (p1.affiliation.getOrElse("").compareTo(p2.affiliation.getOrElse(""))>0) p1.affiliation.getOrElse("").compareTo(p2.affiliation.getOrElse(""))<0
        else p1.citation.getOrElse((-1).asInstanceOf[Long]) < p2.citation.getOrElse((-1).asInstanceOf[Long])
      }))
    grouped.asInstanceOf[Array[List[Publication]]] :+ rel(1).asInstanceOf[List[Publication]]
  }

  def searchGrant(search: Search, db : Database) : Array[List[Grant]] = {
    val conn = db.getConnection()
    var lst = List[Grant]()
    try {
      val stmt = conn.prepareStatement("SELECT * FROM grants WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= Grant(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("investigator")),
          Some(rs.getString("affiliation")),
          Some(rs.getString("agency")),
          if (rs.getInt("year") != -1) Some(rs.getInt("year")) else None,
          if (rs.getInt("amount") != -1) Some(rs.getLong("amount")) else None
        )
      }
    } finally {
      conn.close()
    }
    lst = LeanredPairwiseMergability.link(lst, "Grant", db).map(rec => rec.asInstanceOf[Grant])
    val rel = BinaryClassification.predictRelevance(lst, "Grant", search, db)
    val grouped = KMeans.group(rel(0), "Grant").asInstanceOf[Array[List[Grant]]]
      .map(_.sortWith((p1,p2) => {
        if (p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) != 0) p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) < 0
        else if (p1.investigator.getOrElse("").compareTo(p2.investigator.getOrElse("")) != 0) p1.investigator.getOrElse("").compareTo(p2.investigator.getOrElse("")) < 0
        else if (p1.year.getOrElse(-1) != p2.year.getOrElse(-1)) p1.year.getOrElse(-1)<p2.year.getOrElse(-1)
        else if (p1.agency.getOrElse("").compareTo(p2.agency.getOrElse("")) != 0) p1.agency.getOrElse("").compareTo(p2.agency.getOrElse(""))<0
        else if (p1.affiliation.getOrElse("").compareTo(p2.affiliation.getOrElse(""))>0) p1.affiliation.getOrElse("").compareTo(p2.affiliation.getOrElse(""))<0
        else p1.amount.getOrElse((-1).asInstanceOf[Long]) < p2.amount.getOrElse((-1).asInstanceOf[Long])
      }))
    grouped :+ rel(1).asInstanceOf[List[Grant]]
  }

  def searchPatent(search: Search, db : Database) : Array[List[Patent]] = {
    val conn = db.getConnection()
    var lst = List[Patent]()
    try {
      val stmt = conn.prepareStatement("SELECT * FROM patent WHERE name_searched = ? AND affiliation_searched = ?;")
      stmt.setString(1, search.name)
      stmt.setString(2, search.affiliation)
      val rs = stmt.executeQuery()
      while (rs.next()) {
        lst ::= Patent(
          search.name,
          search.affiliation,
          Some(rs.getString("title")),
          Some(rs.getString("inventor")),
          Some(rs.getString("filed")),
          Some(rs.getString("issued")),
          Some(rs.getString("patentNum")),
          Some(rs.getString("asignee")),
          Some(rs.getString("abstract"))
        )
      }
    } finally {
      conn.close()
    }
    lst = LeanredPairwiseMergability.link(lst, "Patent", db).map(rec => rec.asInstanceOf[Patent])
    val rel = BinaryClassification.predictRelevance(lst, "Patent", search, db)
    val grouped = KMeans.group(rel(0), "Patent").asInstanceOf[Array[List[Patent]]]
      .map(_.sortWith((p1,p2) => {
        if (p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) != 0) p1.title.getOrElse("").compareTo(p2.title.getOrElse("")) < 0
        else if (p1.inventor.getOrElse("").compareTo(p2.inventor.getOrElse("")) != 0) p1.inventor.getOrElse("").compareTo(p2.inventor.getOrElse("")) < 0
        else if (p1.filed.getOrElse("").compareTo(p2.filed.getOrElse("")) != 0) p1.filed.getOrElse("").compareTo(p2.filed.getOrElse("")) < 0
        else if (p1.issued.getOrElse("").compareTo(p2.issued.getOrElse("")) != 0) p1.issued.getOrElse("").compareTo(p2.issued.getOrElse("")) < 0
        else if (p1.patentNum.getOrElse("").compareTo(p2.patentNum.getOrElse("")) != 0) p1.patentNum.getOrElse("").compareTo(p2.patentNum.getOrElse("")) < 0
        else if (p1.asignee.getOrElse("").compareTo(p2.asignee.getOrElse("")) != 0) p1.asignee.getOrElse("").compareTo(p2.asignee.getOrElse("")) < 0
        else p1.description.getOrElse("").compareTo(p2.description.getOrElse("")) < 0
      }))
    grouped :+ rel(1).asInstanceOf[List[Patent]]
  }

}
