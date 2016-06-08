package services

import models._
import play.api.libs.json.JsValue

object Parsers {

  /*
  * Publications:
  *  1. Title
  *  2. Author
  *  3. Publisher
  *  4. Year
  *  5. Affiliation
  *  6. Citations
  * */
  def IEEE (search: JsValue, jsValue: JsValue) : Publication =  Publication(
    (search \ "name").as[String],
    (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[List[JsValue]].map(lst => lst.head.as[String]),
    (jsValue \ "authors").asOpt[List[JsValue]].map(lst => lst.head.as[String]),
    (jsValue \ "publisher").asOpt[List[JsValue]].map(lst => lst.head.as[String]),
    (jsValue \ "py").asOpt[List[JsValue]].map(lst => Integer.parseInt(lst.head.as[String])),
    (jsValue \ "affiliations").asOpt[List[JsValue]].map(lst => lst.head.as[String]),
    None
  )

  def Scopus (search: JsValue, jsValue: JsValue) : Publication = Publication (
    (search \ "name").as[String],
    (search \ "affiliation").as[String],
    (jsValue \ "dc:title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "dc:creator").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "prism:publicationName").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "prism:coverDate").asOpt[List[JsValue]].map(lst => Integer.parseInt(lst.head.as[String].substring(0, 4))),
    (jsValue \ "affiliation").asOpt[List[JsValue]].map(lst => (lst.head \ "affilname").as[String]),
    (jsValue \ "citedby-count").asOpt[List[JsValue]].map(lst => Integer.parseInt(lst.head.as[String]))
  )

  def ScienceDirect (search: JsValue, jsValue: JsValue) : Publication = Publication (
    (search \ "name").as[String],
    (search \ "affiliation").as[String],
    (jsValue \ "dc:title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "authors").asOpt[List[JsValue]].map(lst => lst.map(auth => (auth \ "surname") + ", " + (auth \ "given-name") ).mkString("; ")),
    (jsValue \ "prism:publicationName").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "prism:coverDate").asOpt[List[JsValue]].map(lst => Integer.parseInt((lst.head \ "$").as[String].substring(0, 4))),
    None,
    None
  )

  def StandardPublication (search: JsValue, jsValue: JsValue) : Publication = Publication (
    if (search == null ) "" else (search \ "name").as[String],
    if (search == null ) "" else (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "authors").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "publisher").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "year").asOpt[JsValue].map(value => value.as[Int]),
    (jsValue \ "affiliation").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "citation").asOpt[JsValue].map(value => value.as[Long])
  )

/*
* Grants:
*  1. Title
*  2. Investigator
*  3. Affiliation
*  4. Agency
*  5. Year
*  6. Amount
* */

  def NSF (search: JsValue, jsValue: JsValue) : Grant = Grant (
    (search \ "name").as[String],
    (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[JsValue].map(value => value.as[String]),
    Option((jsValue \ "piFirstName").asOpt[JsValue].map(value => value.as[String]).get + " " + (jsValue \ "piLastName").asOpt[JsValue].map(value => value.as[String]).get),
    (jsValue \ "awardeeName").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "agency").asOpt[JsValue].map(value => value.as[String]),
    {val str = (jsValue \ "date").asOpt[JsValue].map(value => value.as[String]).getOrElse("-1")
    Option(str.substring(Math.max(str.length()-4, 0), str.length()).toInt)},
    Option((jsValue \ "fundsObligatedAmt").asOpt[JsValue].map(value => value.as[String]).getOrElse("-1").toLong)
  )

  def StandardGrant (search: JsValue, jsValue: JsValue) : Grant = Grant (
    if (search == null ) "" else (search \ "name").as[String],
    if (search == null ) "" else (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "investigator").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "affiliation").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "agency").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "year").asOpt[JsValue].map(value => value.as[Int]),
    (jsValue \ "amount").asOpt[JsValue].map(value => value.as[Long])
  )

  /*
  * Grants:
  *  1. Title
  *  2. Inventor
  *  3. Filed
  *  4. Issued
  *  5. Patent Number
  *  6. Asignee
  *  7. Abstract
  * */
  def Justia (search: JsValue, jsValue: JsValue) : Patent = Patent (
    if (search == null ) "" else (search \ "name").as[String],
    if (search == null ) "" else (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "inventors").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "filed").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "date").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "number").asOpt[JsValue].map(value => value.as[String]),
    None,
    (jsValue \ "abstract").asOpt[JsValue].map(value => value.as[String])
  )

  def StandardPatent (search: JsValue, jsValue: JsValue) : Patent = Patent (
    if (search == null ) "" else (search \ "name").as[String],
    if (search == null ) "" else (search \ "affiliation").as[String],
    (jsValue \ "title").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "inventors").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "filed").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "issued").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "patentNum").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "asignee").asOpt[JsValue].map(value => value.as[String]),
    (jsValue \ "abstract").asOpt[JsValue].map(value => value.as[String])
  )
}
