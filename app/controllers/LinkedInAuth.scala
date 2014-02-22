package controllers

import play.api.mvc._
import play.api.libs.ws._
import views.html.defaultpages.unauthorized
import java.net.URLEncoder
import scala.util.Success
import views.html.defaultpages.badRequest
import scala.util.Failure
import scala.util.Try
import scala.concurrent.Future
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global

object LinkedInAuth extends Controller{
  
  type Request = play.api.mvc.Request[play.api.mvc.AnyContent]
  //TODO: store these somewhere, probably encrypted  Possibly in application.conf
  val apiKey = "77ind0mayn2hrn"
  val secretKey = "N8kH4Vvfje0eWaAZ"
  val redirectUrlScheme = "http"
  val redirectPath = "/linkedin/redirect/accept" //TODO Could we use routes here?


  def login(name: String) = Action { request =>
    println("host " + request.host)
    println("path" + request.path)
    println("remote address " +request.remoteAddress)
    println("domain " + request.domain)
    println("request headers:  " + request.headers)
    
    //TODO  Would rather get this with the  query string, but that's an option, son need
    //logic to handle bad request.
    println(request.getQueryString("yo")) //Returns an Option[String]
    Ok("Welcome!").withSession {
      "name" -> name
    }

  }
  
  
  def parmprac = Action { request =>
    val heyParm: Option[String] = request.getQueryString("hey")
    
    val combined = for {
      hey <- getParam("hey", request)
      you <- getParam("you", request)
    }yield (hey, you) 
    
   
    combined match {
      case Success( (hey, you) ) =>  Ok(hey + " " + you)
      case Failure(ex) => BadRequest(ex.getMessage)
    }

    
  }

  
 def getParam(key: String, request: Request):Try[String] = Try {
    request.getQueryString(key).getOrElse(throw new Exception("no param named " + key))
 }  
  
  
  //http://demo-project-c9-groovyflow.c9.io/login?name=whoever
  //http://localhost:9000/linkedin/authredirect
  
  //I think the problem is that I'm redirecting to an address that's in a different domain than mine!
  
  //Here's what we're supposed to send.  The last one is what we're sending
  //https://www.linkedin.com/uas/oauth2/authorization?response_type=code&client_id=77ind0mayn2hrn&state=uQ799hGxl3G2&redirect_uri=http%3A%2F%2Fdemo-project-c9-groovyflow.c9.io%2Flinkedin%2Fauth%2Fredirect%2Faccept
  //https://www.linkedin.com/uas/oauth2/authorization?response_type=code&client_id=77ind0mayn2hrn&state=wVOhNUOa3eYa&redirect_uri=http%3A%2F%2Fdemo-project-c9-groovyflow.c9.io%2Flinkedin%2Fauth%2Fredirect%2Faccept
  //https://www.linkedin.com/uas/oauth2/authorization?response_type=code&client_id=77ind0mayn2hrn&state=L2booTcOxla2&redirect_url=http%3A%2F%2Fdemo-project-c9-groovyflow.c9.io%2Flinkedin%2Fauth%2Fredirect%2Faccept

 def redirectURL(request: Request):String = {
   val url = redirectUrlScheme + "://" + request.host + redirectPath
   println("Constructed redirectURL for linkedIn permissions is " + url)
   url
 }
  
  def authredirect = Action { request =>
    //"http://demo-project-c9-groovyflow.c9.io/linkedin/auth/redirect/accept" 
    val state = makeRegistrationState
    println("registration state is " + state)
    val params = queryParams(Map("response_type" -> "code",  "client_id" -> apiKey, "state" -> state, 
        "redirect_url" -> redirectURL(request)  ))
    Redirect("https://www.linkedin.com/uas/oauth2/authorization?" + params).withSession("registrationState" -> state)
  }

  def redirectaccept = Action.async { request =>
    codeAndState(request) match {
      case None => request.getQueryString("error") match {
        case None => Future { BadRequest("We didn't get the expected response paramaters nor was an error message sent") }
        case Some(errorMessage) =>
          if (errorMessage == "access_denied") /*TODO  Obviously send JSON when we've got a GUI*/ Future { Ok("We're sorry you decided not to give us LinkedIn permissions") }
          else Future { BadRequest(errorMessage) }
      }
      case Some((code, state)) => request.session.get("registrationState") match {
        case None => Future { InternalServerError("Server should have had registrationState in user's session") }
        case Some(sessionRegistrationState) =>
          if (sessionRegistrationState != state) Future { BadRequest("LinkedIn auth state not the same as user's state") }
          else
            accessTokenFuture(code, redirectURL(request)).flatMap { response =>
              if (response.status < 200 || response.status >= 300)
                Future { BadRequest("Unable to get authcode from LinkedIn " + response.status + " " + response.statusText) }
              else {
                val body = Json.parse(response.body)
                val accessToken = (body \ "access_token").as[String]
                val expiresIn = body \ "expires_in" //TODO We'll  get this one later. Not sure if it's a JsString, JsNumber.
                println("accessToken is " + accessToken)
                println("expiresIn == " + expiresIn + " of class " + expiresIn.getClass())
                //TODO Not handling possibility of bad response here!! Maybe we'll just see an exception
                profile(accessToken).map{profileResponse => 
                  val profileBody = Json.parse(profileResponse.body)
                  val firstName = (profileBody \ "firstName").as[String]
                  val lastName = (profileBody \ "lastName").as[String]
                  println("Found profile information for " + firstName + " " + lastName)
                  Ok("Hello " + firstName + " " + lastName).withSession {
                    "accessToken" -> accessToken
                  }
                }
              }
            }

      }
    }

  }
  
  def accessTokenFuture(code: String, redirectURL: String):Future[play.api.libs.ws.Response] = {
    WS.url("https://www.linkedin.com/uas/oauth2/accessToken").withQueryString(
      ("grant_type","authorization_code"),("code", code),
      ("redirect_uri", redirectURL), ("client_id", apiKey), ("client_secret", secretKey)
    ).post(Json.toJson(Map("not supposed to send" ->"body for this post!")))
  }  
  
  def profile(authToken: String):Future[play.api.libs.ws.Response] = {
    linkedInApi("/v1/people/~",  ("oauth2_access_token" -> authToken)  )
  }
  
  def linkedInApi(path: String, parameters: (String, String)*):Future[play.api.libs.ws.Response] = {
    //Note how we unpack a Seq(String, String) so that variadic withQueryString can accept it
    WS.url("https://api.linkedin.com" + path).withHeaders(("x-li-format" -> "json")).withQueryString(parameters:_*).get
  }
  
  
  def codeAndState(request: Request):Option[(String, String)] = {
    for {
       code <- request.getQueryString("code")
       state <- request.getQueryString("state")
     } yield (code, state) 
  }
  
  
  def queryParams(map: Map[String, String]):String = {
    if(map.isEmpty)
      ""
    else
    	map.map( entry => URLEncoder.encode(entry._1, "UTF-8") + "=" + URLEncoder.encode(entry._2, "UTF-8")).reduce(_ + "&" + _)
  }

  val possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  val numChars = 12
  def makeRegistrationState() = {
    def nextRand = Math.floor(Math.random() * possible.size).toInt
    def nextChar = possible.charAt(nextRand)
    
    def loop(accum: String, numToGo: Int):String = numToGo match {
      case 0 => accum
      case _ => loop(accum + nextChar, numToGo - 1)
    } 		
    loop("", numChars)
    
  }
  
  def show = Action {request =>
    println("Contents of frickin' session: " + request.session)
    request.session.get("name").map(Ok(_) ).getOrElse(Unauthorized("You are not logged in"))
  }
  
}