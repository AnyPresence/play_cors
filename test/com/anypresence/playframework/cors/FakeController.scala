package com.anypresence.playframework.cors

import play.api.mvc.Controller
import play.api.mvc.Action
import play.api.mvc.Handler

object FakeController extends Controller { 
  def index = Action {
    play.api.Logger.debug("Saying hello ...")
    Ok("Hello world")
  }
  
  def fakeRouteForOptions = Action {
    NotImplemented("This route shouldn't actually be invoked")
  }
}