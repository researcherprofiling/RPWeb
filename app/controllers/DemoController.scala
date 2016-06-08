package controllers

import javax.inject.{Inject, Singleton}
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import models._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import services.Parsers._
import play.api.db._
import services.search.Search._

@Singleton
class DemoController @Inject() (db : Database) extends Controller {

  //  Initial demo page:
  //  Search panel + empty result
  def index = Action {
    Ok(views.html.main("Researcher Profiling Web App")(
      views.html.demo(searchForm)
      (new Array[List[Publication]](0))
      (new Array[List[Grant]](0))
      (new Array[List[Grant]](0))
    ))
  }

  val searchForm: Form[Search] = Form(
    mapping(
      "name" -> nonEmptyText,
      "affiliation" -> nonEmptyText,
      "srch" -> boolean,
      "refine" -> boolean
    )(Search.apply)(Search.unapply)
  )

  //  User interactions
  def search = Action { implicit request =>
    searchForm.bindFromRequest.fold(
      formWithErrors => {
        Ok(views.html.main("Researcher Profiling Web App")(
          views.html.demo(searchForm)(new Array[List[Publication]](0))(new Array[List[Grant]](0))(new Array[List[Patent]](0))
        ))
      },
      search => {
        if (search.srch) {
          Ok(views.html.main("Researcher Profiling Web App")(
            views.html.demo(searchForm.fill(search))(searchPublication(search, db))(searchGrant(search, db))(searchPatent(search, db))
          ))
        }
        else if (search.refine) {
          Ok(views.html.main("Researcher Profiling Web App")(
            views.html.demo(searchForm.fill(search))(searchPublication(search, db))(searchGrant(search, db))(searchPatent(search, db))
          ))
        }
        else BadRequest("Unrecognized post.")
      }
    )
  }

  def update (src : String) = Action { implicit request =>
    if (src == "IEEE") {
      //  Parse and cache IEEE data
      val recsOpt : Option[List[JsValue]] = request.body.asJson.map(_ \ "data").map(_.as[List[JsValue]])
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (recsOpt.isDefined) {
        val search = searchOpt.get
        val recs : List[Publication] = recsOpt.get.map(IEEE(search, _))
        val conn = db.getConnection()
        conn.setAutoCommit(false)
        val deleteStmt = conn.prepareStatement("DELETE FROM PUBLICATION WHERE SOURCE = ? AND NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ?;")
        deleteStmt.setString(1, "IEEE")
        deleteStmt.setString(2, (search \ "name").as[String])
        deleteStmt.setString(3, (search \ "affiliation").as[String])
        deleteStmt.execute()
        val stmt = conn.prepareStatement("INSERT INTO PUBLICATION (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,AUTHORS ,PUBLISHER ,YEAR ,AFFILIATION ,CITATION, SOURCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        recs foreach (pub => {
          //  Store into database
          stmt.setString(1, pub.nameSearched)
          stmt.setString(2, pub.affiliationSearched)
          stmt.setString(3, pub.title.getOrElse(""))
          stmt.setString(4, pub.authors.getOrElse(""))
          stmt.setString(5, pub.publisher.getOrElse(""))
          stmt.setInt(6, pub.year.getOrElse(-1))
          stmt.setString(7, pub.affiliation.getOrElse(""))
          stmt.setLong(8, pub.citation.getOrElse(-1))
          stmt.setString(9, "IEEE")
          stmt.execute()
        })
        conn.commit()
        conn.close()
        Ok("Updated.")
      }
      else BadRequest("Empty data set")
    }
    else if (src == "Scopus") {
      //  Parse and cache Scopus data
      val recsOpt : Option[List[JsValue]] = request.body.asJson.map(_ \ "data").map(_.as[List[JsValue]])
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (recsOpt.isDefined) {
        val search = searchOpt.get
        val recs : List[Publication] = recsOpt.get.map(Scopus(search, _))
        val conn = db.getConnection()
        conn.setAutoCommit(false)
        val deleteStmt = conn.prepareStatement("DELETE FROM PUBLICATION WHERE SOURCE = ? AND NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ?;")
        deleteStmt.setString(1, "Scopus")
        deleteStmt.setString(2, (search \ "name").as[String])
        deleteStmt.setString(3, (search \ "affiliation").as[String])
        deleteStmt.execute()
        val stmt = conn.prepareStatement("INSERT INTO PUBLICATION (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,AUTHORS ,PUBLISHER ,YEAR ,AFFILIATION ,CITATION, SOURCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        recs foreach (pub => {
          //  Store into database
          stmt.setString(1, pub.nameSearched)
          stmt.setString(2, pub.affiliationSearched)
          stmt.setString(3, pub.title.getOrElse(""))
          stmt.setString(4, pub.authors.getOrElse(""))
          stmt.setString(5, pub.publisher.getOrElse(""))
          stmt.setInt(6, pub.year.getOrElse(-1))
          stmt.setString(7, pub.affiliation.getOrElse(""))
          stmt.setLong(8, pub.citation.getOrElse(-1))
          stmt.setString(9, "Scopus")
          stmt.execute()
        })
        conn.commit()
        conn.close()
        Ok("Updated.")
      }
      else BadRequest("Empty data set")
    }
    else if (src == "ScienceDirect") {
      //  Parse and cache Science Direct data
      val recsOpt : Option[List[JsValue]] = request.body.asJson.map(_ \ "data").map(_.as[List[JsValue]])
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (recsOpt.isDefined) {
        val search = searchOpt.get
        val recs : List[Publication] = recsOpt.get.map(ScienceDirect(search, _))
        val conn = db.getConnection()
        conn.setAutoCommit(false)
        val deleteStmt = conn.prepareStatement("DELETE FROM PUBLICATION WHERE SOURCE = ? AND NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ?;")
        deleteStmt.setString(1, "Scopus")
        deleteStmt.setString(2, (search \ "name").as[String])
        deleteStmt.setString(3, (search \ "affiliation").as[String])
        deleteStmt.execute()
        val stmt = conn.prepareStatement("INSERT INTO PUBLICATION (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,AUTHORS ,PUBLISHER ,YEAR ,AFFILIATION ,CITATION, SOURCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        recs foreach (pub => {
          //  Store into database
          stmt.setString(1, pub.nameSearched)
          stmt.setString(2, pub.affiliationSearched)
          stmt.setString(3, pub.title.getOrElse(""))
          stmt.setString(4, pub.authors.getOrElse(""))
          stmt.setString(5, pub.publisher.getOrElse(""))
          stmt.setInt(6, pub.year.getOrElse(-1))
          stmt.setString(7, pub.affiliation.getOrElse(""))
          stmt.setLong(8, pub.citation.getOrElse(-1))
          stmt.setString(9, "Scopus")
          stmt.execute()
        })
        conn.commit()
        conn.close()
        Ok("Updated.")
      }
      else BadRequest("Empty data set")
    }
    else if (src == "Google") {
      BadRequest("Unrecognized post.")
    }
    else if (src == "NSF") {
      val recsOpt : Option[List[JsValue]] = request.body.asJson.map(_ \ "data").map(_.as[List[JsValue]])
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (recsOpt.isDefined) {
        val search = searchOpt.get
        val recs : List[Grant] = recsOpt.get.map(NSF(search, _))
        val conn = db.getConnection()
        conn.setAutoCommit(false)
        val deleteStmt = conn.prepareStatement("DELETE FROM grants WHERE name_searched = ? AND affiliation_searched = ? AND source = ?;")
        deleteStmt.setString(1, (search \ "name").as[String])
        deleteStmt.setString(2, (search \ "affiliation").as[String])
        deleteStmt.setString(3, "NSF")
        deleteStmt.executeUpdate()
        val stmt = conn.prepareStatement("INSERT INTO grants (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,INVESTIGATOR , AFFILIATION, AGENCY ,YEAR ,AMOUNT , SOURCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        recs foreach (grant => {
          //  Store into database
          stmt.setString(1, grant.nameSearched)
          stmt.setString(2, grant.affiliationSearched)
          stmt.setString(3, grant.title.getOrElse(""))
          stmt.setString(4, grant.investigator.getOrElse(""))
          stmt.setString(5, grant.affiliation.getOrElse(""))
          stmt.setString(6, grant.agency.getOrElse(""))
          stmt.setInt(7, grant.year.getOrElse(-1))
          stmt.setLong(8, grant.amount.getOrElse(-1))
          stmt.setString(9, "NSF")
          stmt.execute()
        })
        conn.commit()
        conn.close()
      }
      Ok("Updated.")
    }
    else if (src == "Justia") {
      val recsOpt : Option[List[JsValue]] = request.body.asJson.map(_ \ "data").map(_.as[List[JsValue]])
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (recsOpt.isDefined) {
        val search = searchOpt.get
        val recs : List[Patent] = recsOpt.get.map(Justia(search, _))
        val conn = db.getConnection()
        conn.setAutoCommit(false)
        val deleteStmt = conn.prepareStatement("DELETE FROM patent WHERE name_searched = ? AND affiliation_searched = ? AND source = ?;")
        deleteStmt.setString(1, (search \ "name").as[String])
        deleteStmt.setString(2, (search \ "affiliation").as[String])
        deleteStmt.setString(3, "Justia")
        deleteStmt.executeUpdate()
        val stmt = conn.prepareStatement("INSERT INTO patent (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,INVENTOR , FILED, ISSUED,PATENTNUM ,ASSIGNEE, ABSTRACT , SOURCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
        recs foreach (patent => {
          //  Store into database
          stmt.setString(1, patent.nameSearched)
          stmt.setString(2, patent.affiliationSearched)
          stmt.setString(3, patent.title.getOrElse(""))
          stmt.setString(4, patent.inventor.getOrElse(""))
          stmt.setString(5, patent.filed.getOrElse(""))
          stmt.setString(6, patent.issued.getOrElse(""))
          stmt.setString(7, patent.patentNum.getOrElse(""))
          stmt.setString(8, patent.asignee.getOrElse(""))
          stmt.setString(9, patent.description.getOrElse(""))
          stmt.setString(10, "Justia")
          stmt.execute()
        })
        conn.commit()
        conn.close()
      }
      Ok("Updated.")
    }
    else BadRequest("Unrecognized post.")
  }

  def relevance (aspect : String) = Action { implicit request =>
    if (aspect == "Publication") {
      val dataOpt : Option[JsValue] = request.body.asJson.map(_ \ "data" get)
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      val relevantOpt : Option[JsValue] = request.body.asJson.map(_ \ "relevant" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (dataOpt.isDefined && relevantOpt.isDefined) {
        val relevant = relevantOpt.get.as[Boolean]
        val pub = StandardPublication(searchOpt.get, dataOpt.get)
        val conn = db.getConnection()
        var stmt = conn.prepareStatement("DELETE FROM PUBLICATION_RELEVANCE WHERE NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ? AND TITLE = ? AND AUTHORS = ? AND PUBLISHER = ? AND YEAR = ? AND AFFILIATION = ? AND CITATION = ? AND RELEVANT = ?;")
        stmt.setString(1, pub.nameSearched)
        stmt.setString(2, pub.affiliationSearched)
        stmt.setString(3, pub.title.getOrElse(""))
        stmt.setString(4, pub.authors.getOrElse(""))
        stmt.setString(5, pub.publisher.getOrElse(""))
        stmt.setInt(6, pub.year.getOrElse(-1))
        stmt.setString(7, pub.affiliation.getOrElse(""))
        stmt.setLong(8, pub.citation.getOrElse(-1))
        stmt.setBoolean(9, !relevant)
        stmt.executeUpdate()
        stmt = conn.prepareStatement("INSERT INTO PUBLICATION_RELEVANCE (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,AUTHORS ,PUBLISHER ,YEAR ,AFFILIATION ,CITATION, RELEVANT) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        stmt.setString(1, pub.nameSearched)
        stmt.setString(2, pub.affiliationSearched)
        stmt.setString(3, pub.title.getOrElse(""))
        stmt.setString(4, pub.authors.getOrElse(""))
        stmt.setString(5, pub.publisher.getOrElse(""))
        stmt.setInt(6, pub.year.getOrElse(-1))
        stmt.setString(7, pub.affiliation.getOrElse(""))
        stmt.setLong(8, pub.citation.getOrElse(-1))
        stmt.setBoolean(9, relevant)
        stmt.executeUpdate()
        conn.commit()
        conn.close()
        Ok("Feedback cached!")
      }
      else BadRequest("Unrecognized post.")
    }
    else if (aspect == "Grant") {
      val dataOpt : Option[JsValue] = request.body.asJson.map(_ \ "data" get)
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      val relevantOpt : Option[JsValue] = request.body.asJson.map(_ \ "relevant" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (dataOpt.isDefined && relevantOpt.isDefined) {
        val relevant = relevantOpt.get.as[Boolean]
        val grant = StandardGrant(searchOpt.get, dataOpt.get)
        val conn = db.getConnection()
        var stmt = conn.prepareStatement("DELETE FROM GRANT_RELEVANCE WHERE NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ? AND TITLE = ? AND INVESTIGATOR = ? AND AFFILIATION = ? AND AGENCY = ? AND YEAR = ? AND AMOUNT = ? AND RELEVANT = ?;")
        stmt.setString(1, grant.nameSearched)
        stmt.setString(2, grant.affiliationSearched)
        stmt.setString(3, grant.title.getOrElse(""))
        stmt.setString(4, grant.investigator.getOrElse(""))
        stmt.setString(5, grant.affiliation.getOrElse(""))
        stmt.setString(6, grant.agency.getOrElse(""))
        stmt.setInt(7, grant.year.getOrElse(-1))
        stmt.setLong(8, grant.amount.getOrElse(-1))
        stmt.setBoolean(9, !relevant)
        stmt.executeUpdate()
        stmt = conn.prepareStatement("INSERT INTO GRANT_RELEVANCE (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,INVESTIGATOR ,AFFILIATION,AGENCY,YEAR,AMOUNT, RELEVANT) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        stmt.setString(1, grant.nameSearched)
        stmt.setString(2, grant.affiliationSearched)
        stmt.setString(3, grant.title.getOrElse(""))
        stmt.setString(4, grant.investigator.getOrElse(""))
        stmt.setString(5, grant.affiliation.getOrElse(""))
        stmt.setString(6, grant.agency.getOrElse(""))
        stmt.setInt(7, grant.year.getOrElse(-1))
        stmt.setLong(8, grant.amount.getOrElse(-1))
        stmt.setBoolean(9, relevant)
        stmt.executeUpdate()
        conn.commit()
        conn.close()
        Ok("Feedback cached!")
      }
      else BadRequest("Unrecognized post.")
    }
    else if (aspect == "Patent") {
      val dataOpt : Option[JsValue] = request.body.asJson.map(_ \ "data" get)
      val searchOpt : Option[JsValue] = request.body.asJson.map(_ \ "search" get)
      val relevantOpt : Option[JsValue] = request.body.asJson.map(_ \ "relevant" get)
      if (searchOpt.isEmpty || (searchOpt.get \ "name").asOpt[String].isEmpty || (searchOpt.get \ "affiliation").asOpt[String].isEmpty ) BadRequest("Empty search.")
      else if (dataOpt.isDefined && relevantOpt.isDefined) {
        val relevant = relevantOpt.get.as[Boolean]
        val grant = StandardPatent(searchOpt.get, dataOpt.get)
        val conn = db.getConnection()
        var stmt = conn.prepareStatement("DELETE FROM PATENT_RELEVANCE WHERE NAME_SEARCHED = ? AND AFFILIATION_SEARCHED = ? AND TITLE = ? AND INVENTOR = ? AND FILED = ? AND ISSUED = ? AND PATENTNUM = ? AND ASIGNEE = ? AND RELEVANT = ?;")
        stmt.setString(1, grant.nameSearched)
        stmt.setString(2, grant.affiliationSearched)
        stmt.setString(3, grant.title.getOrElse(""))
        stmt.setString(4, grant.inventor.getOrElse(""))
        stmt.setString(5, grant.filed.getOrElse(""))
        stmt.setString(6, grant.issued.getOrElse(""))
        stmt.setString(7, grant.patentNum.getOrElse(""))
        stmt.setString(8, grant.asignee.getOrElse(""))
        stmt.setBoolean(9, !relevant)
        stmt.executeUpdate()
        stmt = conn.prepareStatement("INSERT INTO GRANT_RELEVANCE (NAME_SEARCHED ,AFFILIATION_SEARCHED ,TITLE ,INVENTOR ,FILED,ISSUED,PATENTNUM,ASIGNEE, RELEVANT) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
        stmt.setString(1, grant.nameSearched)
        stmt.setString(2, grant.affiliationSearched)
        stmt.setString(3, grant.title.getOrElse(""))
        stmt.setString(4, grant.inventor.getOrElse(""))
        stmt.setString(5, grant.filed.getOrElse(""))
        stmt.setString(6, grant.issued.getOrElse(""))
        stmt.setString(7, grant.patentNum.getOrElse(""))
        stmt.setString(8, grant.asignee.getOrElse(""))
        stmt.setBoolean(9, relevant)
        stmt.executeUpdate()
        conn.commit()
        conn.close()
        Ok("Feedback cached!")
      }
      else BadRequest("Unrecognized post.")
    }
    else BadRequest("Unrecognized post.")
  }

  def process = Action { implicit request =>
    val data : Option[JsValue] = request.body.asJson
    if (data.isDefined) {
      BadRequest("Unrecognized post.")
    }
    else BadRequest("Empty data")
  }

  def linkage (aspect : String) = Action { implicit request =>
    if (aspect == "Publication") {
      val link : Boolean = request.body.asJson.map(_ \ "link" get).get.as[Boolean]
      val r1 : Publication = StandardPublication(null, request.body.asJson.map(_ \ "r1" get).get)
      val r2 : Publication = StandardPublication(null, request.body.asJson.map(_ \ "r2" get).get)
      val conn = db.getConnection()
      val remove = conn.prepareStatement("DELETE FROM PUBLICATION_LINKAGE WHERE TITLE1 = ? AND AUTHORS1 = ? AND PUBLISHER1 = ? AND YEAR1 = ? AND AFFILIATION1 = ? AND CITATION1 = ? AND TITLE2 = ? AND AUTHORS2 = ? AND PUBLISHER2 = ? AND YEAR2 = ? AND AFFILIATION2 = ? AND CITATION2 = ?;")
      remove.setString(1, r1.title.getOrElse(""))
      remove.setString(2, r1.authors.getOrElse(""))
      remove.setString(3, r1.publisher.getOrElse(""))
      remove.setInt(4, r1.year.getOrElse(-1))
      remove.setString(5, r1.affiliation.getOrElse(""))
      remove.setLong(6, r1.citation.getOrElse(-1))
      remove.setString(7, r2.title.getOrElse(""))
      remove.setString(8, r2.authors.getOrElse(""))
      remove.setString(9, r2.publisher.getOrElse(""))
      remove.setInt(10, r2.year.getOrElse(-1))
      remove.setString(11, r2.affiliation.getOrElse(""))
      remove.setLong(12, r2.citation.getOrElse(-1))
      remove.executeUpdate()
      remove.setString(1, r2.title.getOrElse(""))
      remove.setString(2, r2.authors.getOrElse(""))
      remove.setString(3, r2.publisher.getOrElse(""))
      remove.setInt(4, r2.year.getOrElse(-1))
      remove.setString(5, r2.affiliation.getOrElse(""))
      remove.setLong(6, r2.citation.getOrElse(-1))
      remove.setString(7, r1.title.getOrElse(""))
      remove.setString(8, r1.authors.getOrElse(""))
      remove.setString(9, r1.publisher.getOrElse(""))
      remove.setInt(10, r1.year.getOrElse(-1))
      remove.setString(11, r1.affiliation.getOrElse(""))
      remove.setLong(12, r1.citation.getOrElse(-1))
      remove.executeUpdate()
      val stmt = conn.prepareStatement("INSERT INTO PUBLICATION_LINKAGE (TITLE1, AUTHORS1, PUBLISHER1, YEAR1, AFFILIATION1, CITATION1, TITLE2, AUTHORS2, PUBLISHER2, YEAR2, AFFILIATION2, CITATION2, MERGABLE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      stmt.setString(1, r1.title.getOrElse(""))
      stmt.setString(2, r1.authors.getOrElse(""))
      stmt.setString(3, r1.publisher.getOrElse(""))
      stmt.setInt(4, r1.year.getOrElse(-1))
      stmt.setString(5, r1.affiliation.getOrElse(""))
      stmt.setLong(6, r1.citation.getOrElse(-1))
      stmt.setString(7, r2.title.getOrElse(""))
      stmt.setString(8, r2.authors.getOrElse(""))
      stmt.setString(9, r2.publisher.getOrElse(""))
      stmt.setInt(10, r2.year.getOrElse(-1))
      stmt.setString(11, r2.affiliation.getOrElse(""))
      stmt.setLong(12, r2.citation.getOrElse(-1))
      stmt.setBoolean(13, link)
      stmt.executeUpdate()
      stmt.setString(1, r2.title.getOrElse(""))
      stmt.setString(2, r2.authors.getOrElse(""))
      stmt.setString(3, r2.publisher.getOrElse(""))
      stmt.setInt(4, r2.year.getOrElse(-1))
      stmt.setString(5, r2.affiliation.getOrElse(""))
      stmt.setLong(6, r2.citation.getOrElse(-1))
      stmt.setString(7, r1.title.getOrElse(""))
      stmt.setString(8, r1.authors.getOrElse(""))
      stmt.setString(9, r1.publisher.getOrElse(""))
      stmt.setInt(10, r1.year.getOrElse(-1))
      stmt.setString(11, r1.affiliation.getOrElse(""))
      stmt.setLong(12, r1.citation.getOrElse(-1))
      stmt.setBoolean(13, link)
      stmt.executeUpdate()
      conn.commit()
      conn.close()
      Ok("Feedback cached!")
    }
    else if (aspect == "Grant") {
      val link : Boolean = request.body.asJson.map(_ \ "link" get).get.as[Boolean]
      val r1 : Grant = StandardGrant(null, request.body.asJson.map(_ \ "r1" get).get)
      val r2 : Grant = StandardGrant(null, request.body.asJson.map(_ \ "r2" get).get)
      val conn = db.getConnection()
      val remove = conn.prepareStatement("DELETE FROM GRANT_LINKAGE WHERE TITLE1 = ? AND INVESTIGATOR1 = ? AND AFFILIATION1 = ? AND AGENCY1 = ? AND YEAR1 = ? AND AMOUNT1 = ? AND TITLE2 = ? AND INVESTIGATOR2 = ? AND AFFILIATION2= ? AND AGENCY2 = ? AND YEAR2 = ? AND AMOUNT2 = ?;")
      remove.setString(1, r1.title.getOrElse(""))
      remove.setString(2, r1.investigator.getOrElse(""))
      remove.setString(3, r1.affiliation.getOrElse(""))
      remove.setString(4, r1.agency.getOrElse(""))
      remove.setInt(5, r1.year.getOrElse(-1))
      remove.setLong(6, r1.amount.getOrElse(-1))
      remove.setString(7, r2.title.getOrElse(""))
      remove.setString(8, r2.investigator.getOrElse(""))
      remove.setString(9, r2.affiliation.getOrElse(""))
      remove.setString(10, r2.agency.getOrElse(""))
      remove.setInt(11, r2.year.getOrElse(-1))
      remove.setLong(12, r2.amount.getOrElse(-1))
      remove.executeUpdate()
      remove.setString(1, r2.title.getOrElse(""))
      remove.setString(2, r2.investigator.getOrElse(""))
      remove.setString(3, r2.affiliation.getOrElse(""))
      remove.setString(4, r2.agency.getOrElse(""))
      remove.setInt(5, r2.year.getOrElse(-1))
      remove.setLong(6, r2.amount.getOrElse(-1))
      remove.setString(7, r1.title.getOrElse(""))
      remove.setString(8, r1.investigator.getOrElse(""))
      remove.setString(9, r1.affiliation.getOrElse(""))
      remove.setString(10, r1.agency.getOrElse(""))
      remove.setInt(11, r1.year.getOrElse(-1))
      remove.setLong(12, r1.amount.getOrElse(-1))
      remove.executeUpdate()
      val stmt = conn.prepareStatement("INSERT INTO GRANT_LINKAGE (TITLE1, INVESTIGATOR1, AFFILIATION1, AGENCY1, YEAR1, AMOUNT1, TITLE2, INVESTIGATOR2, AFFILIATION2, AGENCY2, YEAR2, AMOUNT2, MERGABLE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      stmt.setString(1, r1.title.getOrElse(""))
      stmt.setString(2, r1.investigator.getOrElse(""))
      stmt.setString(3, r1.affiliation.getOrElse(""))
      stmt.setString(4, r1.agency.getOrElse(""))
      stmt.setInt(5, r1.year.getOrElse(-1))
      stmt.setLong(6, r1.amount.getOrElse(-1))
      stmt.setString(7, r2.title.getOrElse(""))
      stmt.setString(8, r2.investigator.getOrElse(""))
      stmt.setString(9, r2.affiliation.getOrElse(""))
      stmt.setString(10, r2.agency.getOrElse(""))
      stmt.setInt(11, r2.year.getOrElse(-1))
      stmt.setLong(12, r2.amount.getOrElse(-1))
      stmt.setBoolean(13, link)
      stmt.executeUpdate()
      stmt.setString(1, r2.title.getOrElse(""))
      stmt.setString(2, r2.investigator.getOrElse(""))
      stmt.setString(3, r2.affiliation.getOrElse(""))
      stmt.setString(4, r2.agency.getOrElse(""))
      stmt.setInt(5, r2.year.getOrElse(-1))
      stmt.setLong(6, r2.amount.getOrElse(-1))
      stmt.setString(7, r1.title.getOrElse(""))
      stmt.setString(8, r1.investigator.getOrElse(""))
      stmt.setString(9, r1.affiliation.getOrElse(""))
      stmt.setString(10, r1.agency.getOrElse(""))
      stmt.setInt(11, r1.year.getOrElse(-1))
      stmt.setLong(12, r1.amount.getOrElse(-1))
      stmt.setBoolean(13, link)
      stmt.executeUpdate()
      conn.commit()
      conn.close()
      Ok("Feedback cached!")
    }
    else BadRequest("Unrecognized post.")
  }


}
