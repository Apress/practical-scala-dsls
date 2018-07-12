package practicalscaladsl.com.chapter_8.util

import java.util

import sun.net.www.http.HttpClient

final class WebFluentInterface {
  private var entity=""
  private var httpResponse= ""
  private var httpClient=""
  private var webService = ""

  case class Username(username:String)

  def Connect(webservice:String):Unit ={
    this.webService= webservice
    this
  }

  def Find(username:String):Unit ={
    httpClient = new DefaultHttpClient()
    httpResponse = httpClient.execute(new HttpGet(webservice))
    entity = httpResponse.getEntity()
    this
  }

  def add(username:String):Unit = {
    // create our object as a json string
    val user = new Username(username)
    val userJson = new Gson().toJson(user)

    // add name value pairs to a post object
    val post = new HttpPost(this.webService)
    val nameValuePairs = new util.ArrayList[NameValuePair]()
    nameValuePairs.add(new BasicNameValuePair("JSON", userJson))
    post.setEntity(new UrlEncodedFormEntity(nameValuePairs))

    // send the post request
    val client = new DefaultHttpClient
    val response = client.execute(post)
    println("--- HEADERS ---")
    response.getAllHeaders.foreach(arg => println(arg))
    this
  }

}
