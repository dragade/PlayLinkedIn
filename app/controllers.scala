package controllers

import play._
import play.mvc._
import play.mvc.results._


import org.scribe.builder._
import org.scribe.builder.api._
import org.scribe.model._
import org.scribe.oauth._
import scala.xml._

/**
 * The main controller for the application.
 * There are four actions: index, connections, profile, and nus
 *
 * index -- any user can see it without authentication
 * connections -- shows a page of connections after authenticating
 * nus -- shows network updates
 * profile --shows your profile
 */
object Application extends Controller {

  // Values stored in the session
  val KEY_REQUEST_TOKEN = "requestToken"
  val KEY_REQUEST_TOKEN_SECRET = "requestTokenSecret"
  val KEY_ACCESS_TOKEN = "accessToken"
  val KEY_ACCESS_TOKEN_SECRET = "accessTokenSecret"

  //supposed to be thread safe
  val oauthService = new ServiceBuilder()
    .provider(classOf[LinkedInApi])
    .apiKey(ApiKeys.apiKey)
    .apiSecret(ApiKeys.secretKey)
    .callback("http://localhost:7000/application/profile")
    .build();


  /**
   * Shows the main intro page (no need to authenticate here)
   */
  def index = Template()

  // just removes all 4 keys from the session
  private def cleanSession() = {
    println("Cleaning session!")
    session.remove(KEY_ACCESS_TOKEN)
    session.remove(KEY_ACCESS_TOKEN_SECRET)
    session.remove(KEY_REQUEST_TOKEN)
    session.remove(KEY_REQUEST_TOKEN_SECRET)
  }

  //the request token string and secret make up the request token and are stored in the session
  private def rebuildRequestToken() = {
    val token = session.get(KEY_REQUEST_TOKEN)
    val secret = session.get(KEY_REQUEST_TOKEN_SECRET)
    println("Rebuilding with request token        " + token)
    println("Rebuilding with request token secret " + secret)
    new Token(token, secret)
  }

  //save the access token in the session since we'll resuse it
  private def saveAccessToken(accessToken: Token): Unit = {
    session.put(KEY_ACCESS_TOKEN, accessToken.getToken())
    session.put(KEY_ACCESS_TOKEN_SECRET, accessToken.getSecret())
  }

  /**
   * Handles the 3 authentication cases:
   * 1) just got redirected from LinkedIn
   * 2) alrady logged in (has access token in the session)
   * 3) have nothing and need to redirect to LinkedIn for auth
   *
   * The method is defined as taking oauth_token and oauth_verifier.
   * The "oauth_token" is the request token value, LinkedIn just calls the callback URL
   * and passes that param back as well as the oauth_verifier code which is what we really need,
   * so we can ignore the oauth_token param.
   */
  private def authenticate(oauth_token: String, oauth_verifier: String): (Token, Boolean) = {
    val accessTokenToken = session.get(KEY_ACCESS_TOKEN)
    val accessTokenSecret = session.get(KEY_ACCESS_TOKEN_SECRET)
    val needsRedirect = true

    if (accessTokenToken != null && accessTokenSecret != null) {
      //have the access token already so get its parts out of the session and reconstruct
      println("Already logged in")
      println("session access token         :" + accessTokenToken)
      println("session access token secret  :" + accessTokenSecret)
      val accessToken = new Token(accessTokenToken, accessTokenSecret)
      (accessToken, !needsRedirect)
    }
    else if (oauth_verifier != null) {
      //got redirected from LinkedIn and the oauth_verifier is passed as a parameter
      println("Redirected from LinkedIn with oauth_verifier " + oauth_verifier)
      val verifier = new Verifier(oauth_verifier)
      val accessToken = oauthService.getAccessToken(rebuildRequestToken(), verifier);
      cleanSession();
      saveAccessToken(accessToken);
      (accessToken, !needsRedirect)
    }
    else {
      println("Fresh Start")
      val requestToken = oauthService.getRequestToken()
      println("got request token: " + requestToken.toString())
      (requestToken, needsRedirect)
    }
  }

  /**
   * Tries the main logic to display a template but if fails, then logs the error and redirects to index.
   */
  private def doAndRedirectToIndexOnError(oauth_token: String, oauth_verifier: String, mainAction: (Token => Result)): Result = {
    try {
      val (token, needsRedirect) = authenticate(oauth_token, oauth_verifier)
      if (needsRedirect) {
        doRedirect(token)
      } else {
        mainAction(token)
      }
    } catch {
      case e: Exception =>
        println("Failed due to " + e.getMessage)
        cleanSession()
        Action(index)
    }
  }

  /**
   * Redirects to the authorization URL from LinkedIn
   */
  private def doRedirect(requestToken: Token) = {
    //now redirect to the authorization url from LinkedIn. the callback will bring us back
    //to this method but we'll have additional request parameters for oauth_token and oauth_verifier
    val url = oauthService.getAuthorizationUrl(requestToken)

    //in playframework we can only store Strings in the cookie based "session", so
    //just stash the 2 components of the requestToken
    cleanSession();
    session.put(KEY_REQUEST_TOKEN, requestToken.getToken())
    session.put(KEY_REQUEST_TOKEN_SECRET, requestToken.getSecret())
    println("Redirecting to " + url + "\n\n")
    Redirect(url)
  }


  /**
   * Makes a REST API call and returns the result
   */
  private def makeApiCall(accessToken: Token, restUrl: String) = {
    //create and sign a request for the resource
    val orequest: OAuthRequest = new OAuthRequest(Verb.GET, restUrl);
    oauthService.signRequest(accessToken, orequest);

    //actually send the request and get the xml body back
    val oresponse: Response = orequest.send();
    oresponse.getBody();
  }

  // just gets my profile info and displays the XML data
  def profile(oauth_token: String, oauth_verifier: String) = {
    def doProfile(token: Token): Result = {
      println("Getting ready to make a profile call")
      val restUrl = "http://api.linkedin.com/v1/people/~:(id,first-name,last-name,picture-url)"
      val apiResponse = makeApiCall(token, restUrl)
      Template('apiResponse -> apiResponse)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doProfile)
  }

  // just gets my network updates info and displays the XML data
  def nus(oauth_token: String, oauth_verifier: String) = {
    def doNus(token: Token): Result = {
      println("Getting ready to make a nus call")
      val restUrl = "http://api.linkedin.com/v1/people/~/network/updates?scope=self"
      val apiResponse = makeApiCall(token, restUrl)
      Template('apiResponse -> apiResponse)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doNus)
  }

  // gets my voldemort data and displays the data
  def vm(oauth_token: String, oauth_verifier: String) = {
    def doVm(token: Token): Result = {
      //val storeName = "li-employees"
      //val key = 325284
      val storeName = "wipa-state"
      val key = 325284

      println("Getting ready to make a voldemort call to store " + storeName)
      val restUrl = "http://api.linkedin.com/v1/voldemort/stores/" + storeName + "/values/" + key
      val apiResponse = makeApiCall(token, restUrl)
      println("JSON from Voldemort:")
      println(apiResponse)
      Template('apiResponse -> apiResponse)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doVm)
  }


  /**
   * The main action for the site, just showing pictures of your connections
   */
  def connections(oauth_token: String, oauth_verifier: String) = {
    def doConns(token: Token): Result = {
      println("Getting ready to make a connections call")
      val restUrl = "http://api.linkedin.com/v1/people/~/connections:(id,first-name,last-name,picture-url)"
      val apiResponse = makeApiCall(token, restUrl)
      val people = parseConnectionXml(apiResponse)
      Template('people -> people)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doConns)
  }

  /**
   * Simple class to hold person info
   */
  case class Person(val firstName: String, val lastName: String, val picture: String)

  /**
   * Parse the XML response from the API and return a list of Person
   */
  private def parseConnectionXml(apiResponse: String) = {
    val xml = XML.loadString(apiResponse)
    val people = xml \\ "person"
    people.map(p => {
      val firstName = (p \ "first-name").text
      val lastName = (p \ "last-name").text
      val picture = (p \ "picture-url").text
      Person(firstName, lastName, picture)
    })
  }
}
