import scala.annotation.switch
import scala.math.BigDecimal

package com {
  package practicalscala {
    package v1 {
      class TradingSystem {
        protected var _pair: String = ""
        protected var _pip_value: BigDecimal = 0.0
        protected var _pip_profit:Int = 0
        protected var _pip_lost:Int = 0
        protected var _start_position:BigDecimal =0.0
        protected var _actual_value:BigDecimal = 0.0
        protected var _capital:BigDecimal = 0
        protected var _max_capital_lost:BigDecimal = 0
        protected var _max_capital_profit:BigDecimal =0


        def pair(first: String, second: String): this.type  = {
          this._pair = s"$first/$second"
          this
        }

        def account(_type:String):this.type = {
          this._pip_value = (_type: @switch) match {
            case "MICRO" => 0.1
            case "MINI" => 1
            case "STANDARD" => 10
          }
          this
        }

        def take_profit(pip:Int):this.type ={
          this._pip_profit = pip
          this._max_capital_profit =(this._capital+(this._pip_value * pip))
          this
        }

        def stop_loss(pip:Int):this.type  ={
          this._pip_lost = pip
          this._max_capital_lost = (this._capital-(this._pip_value * pip))
          this
        }

        def start_position(start_value:Double):this.type ={
          this._start_position=start_value
          this
        }

        def actual_position(actual_value:Double):this.type ={
          this._actual_value=actual_value
          this
        }

        def capital(investment:Int):this.type ={
          this._capital=investment
          this
        }

        def execute() ={
          val _gain=(this._actual_value - this._start_position)
          val _check_profit =((_gain * this._pip_value) + _capital)

          if(_check_profit > _max_capital_profit){
            println("Max Profit gain")
          }else if(_check_profit > _max_capital_lost){
            println("Stop loss gain")
          }
          println(s"PIP gained:"+_gain)
          println("Actual Capital:"+_check_profit)
        }
      }
    }
  }
}
