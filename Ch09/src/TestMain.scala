import com.practicalscala.v1._

object TestMain{
  def main(args:Array[String]):Unit ={
    val _pair = new TradingSystem()
    println(_pair.pair("USD","EUR")
        .account("MINI")
        .start_position(1.0455)
        .actual_position(1.0554)
        .stop_loss(10)
        .take_profit(10)
        .capital(10000)
        .execute())

    println(_pair.pair("USD","EUR")
      .account("MINI")
      .start_position(1.0455)
      .actual_position(1.0354)
      .stop_loss(10)
      .take_profit(10)
      .capital(10000)
      .execute())
  }
}
