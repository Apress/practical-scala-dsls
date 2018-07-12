package controllers

import javax.inject.Inject

import play.api.mvc.{Action, Controller}
import utils._
import play.api.libs.json.Json


class LanguageController @Inject() extends Controller{
  def language = Action{
    val configurationReader=new ConfigurationReader()

    val filter_language = configurationReader
                                .language()
                                  .status("active")
                                    .filter()

    Ok(Json.toJson(filter_language))
  }
}
