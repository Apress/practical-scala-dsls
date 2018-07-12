package practicalscaladsl.com.chapter_8

import android.app.{Activity, Service}
import android.content.Intent
import android.os.{Bundle, IBinder}
import practicalscaladsl.com.chapter_8.util.WebFluentInterface

class WebServiceExample extends Service {

  val mStartMode:Int

  var mBinder:IBinder

  val mAllowRebind:Boolean

  val webFluentInterface:WebFluentInterface

  override def onCreate():Unit

  override def onStartCommand(itent:Intent, flags:Int, startId:Int):Int = {
    webFluentInterface.
      Connect("http://localhost:8080/add").
      Add("Test")
    return mStartMode
  }

  override def onBind(intent:Intent):IBinder = return mBinder

  override def onUnbind(intent:Intent):Boolean = return mAllowRebind

  override def onRebind(intent:Intent)

  override def onDestroy():Unit
}
