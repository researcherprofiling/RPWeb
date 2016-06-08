package models

import play.api.libs.json.{Json, JsValue}


abstract class Record (nameSearched : String, affiliationSearched : String, addThis : Boolean = true) {
  var origRecords : List[Record] = List()
  if (addThis) origRecords ::= this
}

/*
* For all string types: "" = missing
* For all numeric types: -1 = missing
* */
case class Publication (nameSearched : String, affiliationSearched : String,
                        title : Option[String], authors : Option[String], publisher : Option[String],
                        year : Option[Int], affiliation : Option[String], citation : Option[Long], addThis : Boolean = true) extends Record(nameSearched, affiliationSearched, addThis)
{
  override lazy val toString : String = {
    var temp = ""
    if (title.isDefined) temp += "Title: " + title.get + "\n"
    if (authors.isDefined) temp += "Authors: " + authors.get + "\n"
    if (publisher.isDefined) temp += "Publisher: " + publisher.get + "\n"
    if (year.isDefined) temp += "Year: " + year.get + "\n"
    if (affiliation.isDefined) temp += "Affiliation: " + affiliation.get + "\n"
    if (citation.isDefined) temp += "Citation: " + citation.get + "\n"
    temp
  }

}


case class Grant (nameSearched : String, affiliationSearched : String,
                  title : Option[String], investigator : Option[String], affiliation : Option[String],
                  agency : Option[String], year : Option[Int], amount : Option[Long], addThis : Boolean = true) extends Record(nameSearched, affiliationSearched, addThis)
{
  override lazy val toString : String = {
    var temp = ""
    if (title.isDefined) temp += "Title: " + title.get + "\n"
    if (investigator.isDefined) temp += "Investigator: " + investigator.get + "\n"
    if (affiliation.isDefined) temp += "Affiliation: " + affiliation.get + "\n"
    if (agency.isDefined) temp += "Agency: " + agency.get + "\n"
    if (year.isDefined) temp += "Year: " + year.get + "\n"
    if (amount.isDefined) temp += "Amount: " + amount.get + "\n"
    temp
  }

}

case class Patent (nameSearched : String, affiliationSearched : String,
                   title : Option[String], inventor : Option[String], filed : Option[String],
                   issued : Option[String], patentNum : Option[String], asignee : Option[String],
                   description : Option[String], addThis : Boolean = true) extends Record(nameSearched, affiliationSearched, addThis)
{
  override lazy val toString : String = {
    var temp = ""
    if (title.isDefined) temp += "Title: " + title.get + "\n"
    if (inventor.isDefined) temp += "Inventor: " + inventor.get + "\n"
    if (filed.isDefined) temp += "Filed: " + filed.get + "\n"
    if (issued.isDefined) temp += "Issued: " + issued.get + "\n"
    if (patentNum.isDefined) temp += "Application/Patent Number: " + patentNum.get + "\n"
    if (asignee.isDefined) temp += "Asignee: " + asignee.get + "\n"
    if (description.isDefined) temp += "Abstract: " + description.get + "\n"
    temp
  }
}