package utils

import com.typesafe.config.ConfigFactory
import scala.util.parsing.json._

final class ConfigurationReader {
  private var language_list = Map[String,String]()
  private var language_status = Map[String,String]()
  var result = Map[String,String]()

  def language() = {

    val language = ConfigFactory.load("language.conf").getString("language.list").split(",")
    val status = ConfigFactory.load("language.conf").getString("language.status").split(",")

    for(i <- 0 to (language.length - 1)){
      this.language_list += (language(i) -> status(i))
    }

    this
  }

  def status(status:String) = {

    for((_key,_value) <- this.language_list) {
      if (_value.equalsIgnoreCase(status)) this.language_status += (_key -> _value)
    }

    this
  }

  def filter():Map[String, String] = {
    this.language_status
  }
}
